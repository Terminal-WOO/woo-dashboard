# PostgreSQL Backend with Actor Model

Een geavanceerde PostgreSQL backend implementatie die het Erlang actor model combineert met moderne PostgreSQL features voor het WOO Dashboard.

## 🎯 Features

### Actor Model Patterns
- **Actor System**: PostgreSQL-backed actors met transactionele message processing
- **Supervisors**: Fault-tolerant supervision trees met restart strategies
- **Process Registry**: Named process lookup voor actor discovery
- **Message Passing**: Immutable messages zonder shared state

### Modern PostgreSQL Features
- **LISTEN/NOTIFY**: Real-time pub/sub voor event streaming
- **JSONB**: Flexible schema voor metadata
- **Full-text Search**: Nederlandse volledige-tekst zoekmachine met tsvector
- **Materialized Views**: Fast aggregaties voor statistieken
- **Triggers**: Automatische audit trails en notificaties
- **GIN Indexes**: Geoptimaliseerde JSONB en full-text search
- **Row-level Locking**: Transactionele consistency
- **Generated Columns**: Auto-computed search vectors

## 🚀 Quick Start

### Vereisten
- Node.js 18+
- PostgreSQL 14+
- npm of yarn

### Installatie

```bash
# Installeer dependencies
npm install

# Configureer database
cp .env.example .env
# Edit .env met je PostgreSQL credentials

# Start de server (runt automatisch migrations en seeding)
npm run dev
```

### Database Setup

Als je de database handmatig wilt opzetten:

```bash
# Maak database
createdb woo_dashboard

# Run migrations
npm run migrate

# Seed initial data
npm run seed
```

## 📋 Database Schema

### Tables

**organizations**
- Overheidsorganisaties (gemeenten, provincies)
- Unieke code per organisatie

**woo_requests**
- WOO verzoeken met status tracking
- JSONB metadata voor flexibele properties
- Auto-generated search_vector voor full-text search
- Timestamps: created_at, updated_at

**status_history**
- Audit trail van alle statuswijzigingen
- Automatisch gevuld via triggers

### Materialized View

**statistics_summary**
- Pre-computed statistieken per status
- Refresh via `REFRESH MATERIALIZED VIEW statistics_summary`

### Indexes

- **GIN index** op JSONB metadata
- **GIN index** op search_vector (full-text)
- **GiST index** op trigrams (fuzzy search)
- **B-tree indexes** op vaak gebruikte kolommen

## 🎭 Actor System

### Actor Lifecycle

```typescript
// Create actor
const actor = app.spawn(behavior, initialState);

// Send message
await actor.send({ type: 'get_document', id: 123 });

// Listen to PostgreSQL notifications
await actor.listen('status_change');
actor.on('postgres_notify', (notification) => {
  console.log('Database event:', notification);
});

// Terminate
await actor.stop();
```

### Supervisor Strategies

```typescript
// permanent: Always restart
const actor = app.spawnSupervised(behavior, state, 'permanent');

// temporary: Never restart
const actor = app.spawnSupervised(behavior, state, 'temporary');

// transient: Restart only on abnormal exit
const actor = app.spawnSupervised(behavior, state, 'transient');
```

### Process Registry

```typescript
// Register actor met naam
app.register('document_manager', actor);

// Lookup actor
const manager = app.whereis('document_manager');
```

## 🌐 REST API

### Base URL
```
http://localhost:8081/api
```

### Endpoints

#### GET /api/documents
Haal alle WOO verzoeken op.

**Response:**
```json
{
  "documents": [
    {
      "id": 1,
      "title": "Bestemmingsplan Binnenstad",
      "subject": "Herziening bestemmingsplan",
      "organization": "Gemeente Utrecht",
      "status": "ontvangen",
      "priority": "high",
      "metadata": { "theme": "ruimtelijke_ordening" },
      "created_at": "2025-01-15T10:00:00Z",
      "updated_at": "2025-01-15T10:00:00Z"
    }
  ]
}
```

#### GET /api/documents/:id
Haal een specifiek document op.

**Response:**
```json
{
  "document": { ... }
}
```

#### PUT /api/documents/:id/status
Update document status.

**Request:**
```json
{
  "status": "in_behandeling"
}
```

**Response:**
```json
{
  "document": { ... }
}
```

#### GET /api/statistics
Haal statistieken op.

**Response:**
```json
{
  "statistics": [
    {
      "status": "ontvangen",
      "count": 5,
      "avg_days_in_status": 2.5
    }
  ]
}
```

#### GET /api/events
Haal recente events op.

**Query Parameters:**
- `limit` (default: 50)

**Response:**
```json
{
  "events": [
    {
      "type": "status_change",
      "timestamp": "2025-01-15T10:00:00Z",
      "data": {
        "document_id": 1,
        "old_status": "ontvangen",
        "new_status": "in_behandeling"
      }
    }
  ]
}
```

#### POST /api/simulation/start
Start automatische status progressie.

**Response:**
```json
{
  "message": "Simulation started"
}
```

#### POST /api/simulation/stop
Stop automatische status progressie.

#### GET /api/search
Full-text search in documenten.

**Query Parameters:**
- `q` - Zoekterm (required)
- `limit` - Max resultaten (default: 20)

**Response:**
```json
{
  "results": [
    {
      "id": 1,
      "title": "...",
      "rank": 0.95,
      "headline": "Highlighted <b>search</b> terms..."
    }
  ]
}
```

## 🔧 Development

### Scripts

```bash
# Development mode met hot reload
npm run dev

# Build TypeScript
npm run build

# Production mode
npm start

# Run migrations
npm run migrate

# Seed database
npm run seed

# Type checking
npm run type-check
```

### Project Structure

```
postgres-backend/
├── src/
│   ├── actors/
│   │   ├── ActorSystem.ts          # Actor implementation
│   │   └── DocumentManagerActor.ts  # Document manager behavior
│   ├── database/
│   │   ├── schema.sql              # Database schema
│   │   ├── migrate.ts              # Migration script
│   │   └── seed.ts                 # Seed data
│   ├── server.ts                   # Fastify REST API
│   └── index.ts                    # Entry point
├── package.json
├── tsconfig.json
├── .env.example
└── README.md
```

## 🏗️ Architecture

### Message Flow

```
Client Request
    ↓
Fastify Handler
    ↓
Actor.send(message)
    ↓
Transaction BEGIN
    ↓
Behavior(state, message)
    ↓
PostgreSQL Query
    ↓
Transaction COMMIT
    ↓
NOTIFY status_change
    ↓
Actor.on('postgres_notify')
    ↓
Event to Subscribers
    ↓
Response to Client
```

### Real-time Events

PostgreSQL triggers zorgen voor automatische notificaties:

```sql
-- Trigger functie
CREATE FUNCTION notify_status_change()
RETURNS trigger AS $$
BEGIN
  PERFORM pg_notify('status_change', 
    json_build_object(
      'document_id', NEW.id,
      'old_status', OLD.status,
      'new_status', NEW.status
    )::text
  );
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

-- Trigger
CREATE TRIGGER woo_request_status_change
  AFTER UPDATE ON woo_requests
  FOR EACH ROW
  WHEN (OLD.status IS DISTINCT FROM NEW.status)
  EXECUTE FUNCTION notify_status_change();
```

### Full-text Search

Automatische search vector updates:

```sql
-- Generated column
search_vector tsvector GENERATED ALWAYS AS (
  setweight(to_tsvector('dutch', coalesce(title, '')), 'A') ||
  setweight(to_tsvector('dutch', coalesce(subject, '')), 'B')
) STORED
```

Gebruik in queries:

```sql
SELECT *, ts_rank(search_vector, query) as rank
FROM woo_requests, 
     websearch_to_tsquery('dutch', 'bestemmingsplan') query
WHERE search_vector @@ query
ORDER BY rank DESC;
```

## 🔒 Environment Variables

```bash
# Database
DATABASE_URL=postgresql://user:password@localhost:5432/woo_dashboard

# Server
PORT=8081
HOST=0.0.0.0
NODE_ENV=development

# Features
SIMULATION_ENABLED=true
SIMULATION_INTERVAL_MS=2000
```

## 🐛 Troubleshooting

### Connection Error

```
Error: connect ECONNREFUSED 127.0.0.1:5432
```

**Oplossing**: Check of PostgreSQL draait:
```bash
# macOS
brew services start postgresql@14

# Linux
sudo systemctl start postgresql
```

### Migration Error

```
Error: relation "woo_requests" already exists
```

**Oplossing**: Drop en recreate database:
```bash
dropdb woo_dashboard
createdb woo_dashboard
npm run migrate
npm run seed
```

### LISTEN/NOTIFY Not Working

**Check**:
1. PostgreSQL versie >= 9.0
2. Trigger is correct aangemaakt
3. Client is verbonden (niet in transaction mode)

## 📚 Resources

- [PostgreSQL LISTEN/NOTIFY](https://www.postgresql.org/docs/current/sql-notify.html)
- [JSONB Type](https://www.postgresql.org/docs/current/datatype-json.html)
- [Full-text Search](https://www.postgresql.org/docs/current/textsearch.html)
- [Materialized Views](https://www.postgresql.org/docs/current/sql-creatematerializedview.html)
- [Erlang OTP Patterns](https://www.erlang.org/doc/design_principles/des_princ.html)

## 📄 License

MIT
