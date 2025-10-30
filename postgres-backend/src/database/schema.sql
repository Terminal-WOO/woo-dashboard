-- WOO Dashboard PostgreSQL Schema
-- Modern PostgreSQL features: JSONB, Generated columns, Triggers, LISTEN/NOTIFY

-- Enable UUID extension for primary keys
CREATE EXTENSION IF NOT EXISTS "uuid-ossp";

-- Enable pg_trgm for fuzzy text search
CREATE EXTENSION IF NOT EXISTS pg_trgm;

-- Organizations table
CREATE TABLE IF NOT EXISTS organizations (
    id UUID PRIMARY KEY DEFAULT uuid_generate_v4(),
    name TEXT NOT NULL UNIQUE,
    type TEXT NOT NULL CHECK (type IN ('gemeente', 'provincie')),
    status_workflow JSONB NOT NULL DEFAULT '["Ontvangen", "In behandeling", "1e Concept", "2e Concept", "Definitief", "Gepubliceerd"]'::jsonb,
    metadata JSONB DEFAULT '{}'::jsonb,
    created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);

-- Create GIN index for JSONB columns (fast queries)
CREATE INDEX IF NOT EXISTS idx_organizations_metadata ON organizations USING GIN (metadata);

-- WOO Requests table with modern PostgreSQL features
CREATE TABLE IF NOT EXISTS woo_requests (
    id UUID PRIMARY KEY DEFAULT uuid_generate_v4(),
    document_id TEXT NOT NULL UNIQUE, -- WOO-UTR-2024-001
    title TEXT NOT NULL,
    status TEXT NOT NULL,
    submitted_date DATE NOT NULL,
    decided_date DATE,
    organization_id UUID NOT NULL REFERENCES organizations(id) ON DELETE CASCADE,
    category TEXT NOT NULL,
    subject TEXT NOT NULL,
    requester TEXT,
    handler TEXT NOT NULL,

    -- JSONB for flexible metadata storage
    metadata JSONB DEFAULT '{}'::jsonb,

    -- Full-text search vector (auto-updated via trigger)
    search_vector tsvector GENERATED ALWAYS AS (
        setweight(to_tsvector('dutch', coalesce(title, '')), 'A') ||
        setweight(to_tsvector('dutch', coalesce(subject, '')), 'B') ||
        setweight(to_tsvector('dutch', coalesce(category, '')), 'C')
    ) STORED,

    -- Timestamps
    created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),

    -- Check constraint for valid statuses
    CONSTRAINT valid_status CHECK (
        status IN ('Ontvangen', 'In behandeling', '1e Concept', '2e Concept', 'Definitief', 'Gepubliceerd', 'Afgerond')
    )
);

-- Indexes for fast queries
CREATE INDEX IF NOT EXISTS idx_woo_requests_status ON woo_requests(status);
CREATE INDEX IF NOT EXISTS idx_woo_requests_organization ON woo_requests(organization_id);
CREATE INDEX IF NOT EXISTS idx_woo_requests_submitted_date ON woo_requests(submitted_date);
CREATE INDEX IF NOT EXISTS idx_woo_requests_document_id ON woo_requests(document_id);

-- GIN index for full-text search
CREATE INDEX IF NOT EXISTS idx_woo_requests_search ON woo_requests USING GIN (search_vector);

-- GIN index for JSONB metadata
CREATE INDEX IF NOT EXISTS idx_woo_requests_metadata ON woo_requests USING GIN (metadata);

-- Trigram index for fuzzy search on title
CREATE INDEX IF NOT EXISTS idx_woo_requests_title_trgm ON woo_requests USING GIN (title gin_trgm_ops);

-- Status history table (audit trail)
CREATE TABLE IF NOT EXISTS status_history (
    id UUID PRIMARY KEY DEFAULT uuid_generate_v4(),
    request_id UUID NOT NULL REFERENCES woo_requests(id) ON DELETE CASCADE,
    old_status TEXT,
    new_status TEXT NOT NULL,
    changed_by TEXT,
    changed_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    metadata JSONB DEFAULT '{}'::jsonb
);

-- Index for efficient history queries
CREATE INDEX IF NOT EXISTS idx_status_history_request ON status_history(request_id, changed_at DESC);
CREATE INDEX IF NOT EXISTS idx_status_history_changed_at ON status_history(changed_at DESC);

-- Actor state table (for persistent actor state)
CREATE TABLE IF NOT EXISTS actor_states (
    pid TEXT PRIMARY KEY,
    state JSONB NOT NULL DEFAULT '{}'::jsonb,
    updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);

-- View for easy document queries with organization info
CREATE OR REPLACE VIEW woo_requests_view AS
SELECT
    w.id,
    w.document_id,
    w.title,
    w.status,
    w.submitted_date,
    w.decided_date,
    w.category,
    w.subject,
    w.requester,
    w.handler,
    w.metadata,
    w.created_at,
    w.updated_at,
    o.name as organization,
    o.type as organization_type,
    o.status_workflow
FROM woo_requests w
JOIN organizations o ON w.organization_id = o.id;

-- Function to update updated_at timestamp
CREATE OR REPLACE FUNCTION update_updated_at_column()
RETURNS TRIGGER AS $$
BEGIN
    NEW.updated_at = NOW();
    RETURN NEW;
END;
$$ language 'plpgsql';

-- Trigger to auto-update updated_at
CREATE TRIGGER update_organizations_updated_at
    BEFORE UPDATE ON organizations
    FOR EACH ROW
    EXECUTE FUNCTION update_updated_at_column();

CREATE TRIGGER update_woo_requests_updated_at
    BEFORE UPDATE ON woo_requests
    FOR EACH ROW
    EXECUTE FUNCTION update_updated_at_column();

-- Function to record status changes in history
CREATE OR REPLACE FUNCTION record_status_change()
RETURNS TRIGGER AS $$
BEGIN
    -- Only record if status actually changed
    IF OLD.status IS DISTINCT FROM NEW.status THEN
        INSERT INTO status_history (request_id, old_status, new_status, changed_by)
        VALUES (NEW.id, OLD.status, NEW.status, current_user);

        -- NOTIFY for real-time updates (Erlang-style pub/sub!)
        PERFORM pg_notify(
            'status_change',
            json_build_object(
                'document_id', NEW.document_id,
                'title', NEW.title,
                'old_status', OLD.status,
                'new_status', NEW.status,
                'organization', (SELECT name FROM organizations WHERE id = NEW.organization_id),
                'timestamp', extract(epoch from now()) * 1000
            )::text
        );
    END IF;

    RETURN NEW;
END;
$$ LANGUAGE plpgsql;

-- Trigger to automatically record status changes
CREATE TRIGGER record_woo_request_status_change
    AFTER UPDATE ON woo_requests
    FOR EACH ROW
    EXECUTE FUNCTION record_status_change();

-- Function to send notification on new document
CREATE OR REPLACE FUNCTION notify_new_document()
RETURNS TRIGGER AS $$
BEGIN
    PERFORM pg_notify(
        'new_document',
        json_build_object(
            'document_id', NEW.document_id,
            'title', NEW.title,
            'status', NEW.status,
            'organization', (SELECT name FROM organizations WHERE id = NEW.organization_id),
            'timestamp', extract(epoch from now()) * 1000
        )::text
    );

    RETURN NEW;
END;
$$ LANGUAGE plpgsql;

-- Trigger for new documents
CREATE TRIGGER notify_on_new_document
    AFTER INSERT ON woo_requests
    FOR EACH ROW
    EXECUTE FUNCTION notify_new_document();

-- Materialized view for statistics (fast aggregations)
CREATE MATERIALIZED VIEW IF NOT EXISTS woo_statistics AS
SELECT
    COUNT(*) as total_requests,
    COUNT(*) FILTER (WHERE status = 'Ontvangen') as received,
    COUNT(*) FILTER (WHERE status IN ('In behandeling', '1e Concept', '2e Concept', 'Definitief')) as in_progress,
    COUNT(*) FILTER (WHERE status = 'Gepubliceerd') as completed,
    AVG(EXTRACT(EPOCH FROM (decided_date - submitted_date)) / 86400)::integer as avg_handling_days,
    json_object_agg(
        o.name,
        json_build_object(
            'total', COUNT(*),
            'by_status', (
                SELECT json_object_agg(status, count)
                FROM (
                    SELECT status, COUNT(*)::integer as count
                    FROM woo_requests w2
                    WHERE w2.organization_id = o.id
                    GROUP BY status
                ) status_counts
            )
        )
    ) as by_organization
FROM woo_requests w
JOIN organizations o ON w.organization_id = o.id
GROUP BY ();

-- Index for materialized view refresh
CREATE UNIQUE INDEX IF NOT EXISTS idx_woo_statistics_unique ON woo_statistics((1));

-- Function to refresh statistics
CREATE OR REPLACE FUNCTION refresh_statistics()
RETURNS void AS $$
BEGIN
    REFRESH MATERIALIZED VIEW CONCURRENTLY woo_statistics;
END;
$$ LANGUAGE plpgsql;

-- Function for full-text search
CREATE OR REPLACE FUNCTION search_documents(search_query TEXT)
RETURNS TABLE (
    document_id TEXT,
    title TEXT,
    status TEXT,
    organization TEXT,
    rank REAL
) AS $$
BEGIN
    RETURN QUERY
    SELECT
        w.document_id,
        w.title,
        w.status,
        o.name as organization,
        ts_rank(w.search_vector, websearch_to_tsquery('dutch', search_query)) as rank
    FROM woo_requests w
    JOIN organizations o ON w.organization_id = o.id
    WHERE w.search_vector @@ websearch_to_tsquery('dutch', search_query)
    ORDER BY rank DESC
    LIMIT 50;
END;
$$ LANGUAGE plpgsql;

-- Comments for documentation
COMMENT ON TABLE woo_requests IS 'WOO (Wet Open Overheid) document requests with full-text search and automatic audit trail';
COMMENT ON COLUMN woo_requests.search_vector IS 'Auto-generated full-text search vector (Dutch language)';
COMMENT ON COLUMN woo_requests.metadata IS 'Flexible JSONB storage for additional document metadata';
COMMENT ON TRIGGER record_woo_request_status_change ON woo_requests IS 'Automatically records status changes and sends NOTIFY events';
COMMENT ON FUNCTION pg_notify IS 'PostgreSQL NOTIFY for real-time event streaming (Erlang-style pub/sub)';
