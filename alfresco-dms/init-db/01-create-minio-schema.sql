-- MinIO Metadata Tracking Schema for Alfresco
-- This schema tracks MinIO buckets, objects, and Alfresco node mappings

-- Create separate schema for MinIO metadata
CREATE SCHEMA IF NOT EXISTS minio_metadata;

SET search_path TO minio_metadata, public;

-- Buckets table
CREATE TABLE IF NOT EXISTS buckets (
    id SERIAL PRIMARY KEY,
    name VARCHAR(255) UNIQUE NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
    versioning_enabled BOOLEAN DEFAULT FALSE,
    encryption_enabled BOOLEAN DEFAULT FALSE,
    total_objects BIGINT DEFAULT 0,
    total_size_bytes BIGINT DEFAULT 0,
    policy JSONB,
    tags JSONB,
    metadata JSONB
);

-- Objects table
CREATE TABLE IF NOT EXISTS objects (
    id BIGSERIAL PRIMARY KEY,
    bucket_id INTEGER REFERENCES buckets(id) ON DELETE CASCADE,
    object_key TEXT NOT NULL,
    etag VARCHAR(255),
    size_bytes BIGINT NOT NULL,
    content_type VARCHAR(255),
    last_modified TIMESTAMP WITH TIME ZONE,
    version_id VARCHAR(255),
    is_latest BOOLEAN DEFAULT TRUE,
    is_delete_marker BOOLEAN DEFAULT FALSE,
    storage_class VARCHAR(50),
    metadata JSONB,
    tags JSONB,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
    UNIQUE(bucket_id, object_key, version_id)
);

CREATE INDEX IF NOT EXISTS idx_objects_bucket_id ON objects(bucket_id);
CREATE INDEX IF NOT EXISTS idx_objects_key ON objects(object_key);
CREATE INDEX IF NOT EXISTS idx_objects_last_modified ON objects(last_modified DESC);
CREATE INDEX IF NOT EXISTS idx_objects_content_type ON objects(content_type);
CREATE INDEX IF NOT EXISTS idx_objects_metadata ON objects USING GIN(metadata);

-- Alfresco node mapping (links Alfresco nodes to MinIO objects)
CREATE TABLE IF NOT EXISTS alfresco_node_mapping (
    id SERIAL PRIMARY KEY,
    node_uuid UUID NOT NULL,
    node_name TEXT,
    node_path TEXT,
    content_url TEXT,  -- Alfresco's contentUrl format
    object_id BIGINT REFERENCES objects(id) ON DELETE SET NULL,
    mime_type VARCHAR(255),
    size_bytes BIGINT,
    created_date TIMESTAMP WITH TIME ZONE,
    modified_date TIMESTAMP WITH TIME ZONE,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
    UNIQUE(node_uuid)
);

CREATE INDEX IF NOT EXISTS idx_alfresco_node_uuid ON alfresco_node_mapping(node_uuid);
CREATE INDEX IF NOT EXISTS idx_alfresco_node_path ON alfresco_node_mapping(node_path);
CREATE INDEX IF NOT EXISTS idx_alfresco_object_id ON alfresco_node_mapping(object_id);

-- Access logs table
CREATE TABLE IF NOT EXISTS access_logs (
    id BIGSERIAL PRIMARY KEY,
    bucket_id INTEGER REFERENCES buckets(id) ON DELETE SET NULL,
    object_key TEXT,
    operation VARCHAR(50) NOT NULL,
    status_code INTEGER,
    user_agent TEXT,
    ip_address INET,
    bytes_sent BIGINT,
    bytes_received BIGINT,
    time_taken_ms INTEGER,
    timestamp TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP
);

CREATE INDEX IF NOT EXISTS idx_access_logs_timestamp ON access_logs(timestamp DESC);
CREATE INDEX IF NOT EXISTS idx_access_logs_bucket ON access_logs(bucket_id);

-- Usage statistics table
CREATE TABLE IF NOT EXISTS usage_stats (
    id SERIAL PRIMARY KEY,
    bucket_id INTEGER REFERENCES buckets(id) ON DELETE CASCADE,
    stat_date DATE NOT NULL,
    total_requests BIGINT DEFAULT 0,
    get_requests BIGINT DEFAULT 0,
    put_requests BIGINT DEFAULT 0,
    delete_requests BIGINT DEFAULT 0,
    list_requests BIGINT DEFAULT 0,
    total_bytes_uploaded BIGINT DEFAULT 0,
    total_bytes_downloaded BIGINT DEFAULT 0,
    avg_response_time_ms NUMERIC(10, 2),
    error_count INTEGER DEFAULT 0,
    unique_objects INTEGER DEFAULT 0,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
    UNIQUE(bucket_id, stat_date)
);

CREATE INDEX IF NOT EXISTS idx_usage_stats_date ON usage_stats(stat_date DESC);

-- Function to update bucket stats
CREATE OR REPLACE FUNCTION update_bucket_stats(bucket_name VARCHAR)
RETURNS VOID AS $$
BEGIN
    UPDATE buckets b
    SET
        total_objects = (
            SELECT COUNT(*)
            FROM objects o
            WHERE o.bucket_id = b.id
            AND o.is_latest = TRUE
            AND o.is_delete_marker = FALSE
        ),
        total_size_bytes = (
            SELECT COALESCE(SUM(size_bytes), 0)
            FROM objects o
            WHERE o.bucket_id = b.id
            AND o.is_latest = TRUE
            AND o.is_delete_marker = FALSE
        ),
        updated_at = CURRENT_TIMESTAMP
    WHERE b.name = bucket_name;
END;
$$ LANGUAGE plpgsql;

-- Trigger function
CREATE OR REPLACE FUNCTION log_object_operation()
RETURNS TRIGGER AS $$
BEGIN
    IF TG_OP = 'INSERT' THEN
        PERFORM update_bucket_stats((SELECT name FROM buckets WHERE id = NEW.bucket_id));
    ELSIF TG_OP = 'UPDATE' THEN
        IF NEW.is_latest != OLD.is_latest OR NEW.is_delete_marker != OLD.is_delete_marker THEN
            PERFORM update_bucket_stats((SELECT name FROM buckets WHERE id = NEW.bucket_id));
        END IF;
    ELSIF TG_OP = 'DELETE' THEN
        PERFORM update_bucket_stats((SELECT name FROM buckets WHERE id = OLD.bucket_id));
    END IF;
    RETURN NULL;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER trg_update_bucket_stats
AFTER INSERT OR UPDATE OR DELETE ON objects
FOR EACH ROW
EXECUTE FUNCTION log_object_operation();

-- Views
CREATE OR REPLACE VIEW v_bucket_overview AS
SELECT
    b.id,
    b.name,
    b.created_at,
    b.total_objects,
    b.total_size_bytes,
    CASE
        WHEN b.total_size_bytes < 1024 THEN b.total_size_bytes || ' B'
        WHEN b.total_size_bytes < 1048576 THEN ROUND(b.total_size_bytes::NUMERIC / 1024, 2) || ' KB'
        WHEN b.total_size_bytes < 1073741824 THEN ROUND(b.total_size_bytes::NUMERIC / 1048576, 2) || ' MB'
        ELSE ROUND(b.total_size_bytes::NUMERIC / 1073741824, 2) || ' GB'
    END AS size_human_readable,
    b.versioning_enabled,
    (SELECT MAX(last_modified) FROM objects WHERE bucket_id = b.id) AS last_activity
FROM buckets b;

CREATE OR REPLACE VIEW v_alfresco_storage AS
SELECT
    anm.node_uuid,
    anm.node_name,
    anm.node_path,
    anm.mime_type,
    b.name AS bucket_name,
    o.object_key,
    o.size_bytes,
    o.last_modified,
    anm.created_date,
    anm.modified_date
FROM alfresco_node_mapping anm
LEFT JOIN objects o ON o.id = anm.object_id
LEFT JOIN buckets b ON b.id = o.bucket_id
ORDER BY anm.modified_date DESC;

-- Insert initial buckets
INSERT INTO buckets (name, versioning_enabled)
VALUES
    ('alfresco-content', FALSE),
    ('alfresco-deleted', FALSE)
ON CONFLICT (name) DO NOTHING;

-- Grant permissions
GRANT USAGE ON SCHEMA minio_metadata TO alfresco;
GRANT ALL PRIVILEGES ON ALL TABLES IN SCHEMA minio_metadata TO alfresco;
GRANT ALL PRIVILEGES ON ALL SEQUENCES IN SCHEMA minio_metadata TO alfresco;
GRANT EXECUTE ON ALL FUNCTIONS IN SCHEMA minio_metadata TO alfresco;

COMMENT ON SCHEMA minio_metadata IS 'MinIO object storage metadata tracking for Alfresco DMS';
COMMENT ON TABLE alfresco_node_mapping IS 'Maps Alfresco content nodes to MinIO objects';
