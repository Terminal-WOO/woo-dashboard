# PostgreSQL 18 + MinIO Integration

Deze setup gebruikt **PostgreSQL 18** als metadata store voor MinIO objecten, wat een krachtige combinatie biedt voor document management.

## Architectuur

```
┌─────────────────────────────────────────────────────────────┐
│                     Paperless-ngx DMS                        │
└─────────────────────────────────────────────────────────────┘
           │                                    │
           │ Documents                          │ Metadata
           ▼                                    ▼
    ┌─────────────┐                    ┌──────────────────┐
    │   MinIO     │                    │  PostgreSQL 18   │
    │   Storage   │                    │   (Metadata)     │
    │             │◄───────────────────┤                  │
    │  Buckets:   │    Sync Service    │  Schema:         │
    │  - docs     │                    │  - buckets       │
    │  - media    │                    │  - objects       │
    └─────────────┘                    │  - access_logs   │
                                       │  - usage_stats   │
                                       └──────────────────┘
```

## PostgreSQL 18 Features

### Waarom PostgreSQL 18?

PostgreSQL 18 (released in 2025) biedt significante verbeteringen:

1. **Performance Improvements**
   - Verbeterde query planning voor complexe queries
   - Betere indexing voor JSONB metadata
   - Snellere aggregaties voor usage statistics

2. **Better JSON Support**
   - Enhanced JSONB operations
   - SQL/JSON functionaliteit uitgebreid
   - Ideaal voor MinIO metadata opslag

3. **Enhanced Monitoring**
   - Betere statistics views
   - Improved query observability
   - Perfect voor storage analytics

4. **Security Enhancements**
   - Verbeterde role-based access control
   - Enhanced encryption options

## Database Schema

### Minio Metadata Schema

De database bevat een dedicated `minio_metadata` schema met de volgende tabellen:

#### 1. `buckets`
Houdt MinIO buckets bij met statistieken:
- Naam, creation date
- Totaal aantal objecten
- Totale storage grootte
- Versioning en encryption status
- Bucket policies en tags

#### 2. `objects`
Metadata voor elk object in MinIO:
- Object key (path)
- ETag (checksum)
- Grootte in bytes
- Content type
- Last modified timestamp
- Custom metadata (JSONB)
- Tags (JSONB)
- Version ID (voor versioning)

#### 3. `access_logs`
Logging van alle MinIO operaties:
- GET, PUT, DELETE, LIST operations
- Status codes
- IP addresses
- Response times
- Data transfer volumes

#### 4. `usage_stats`
Dagelijkse statistieken per bucket:
- Totaal aantal requests
- Breakdown per operation type
- Upload/download volumes
- Average response times
- Error rates

#### 5. `paperless_document_mapping`
Koppelt Paperless documenten aan MinIO objecten:
- Document ID
- Object ID
- Document type (original, thumbnail, preview, etc.)

## MinIO Metadata Sync Service

Een dedicated Node.js service synchroniseert MinIO naar PostgreSQL:

### Features

1. **Automatic Discovery**
   - Vindt alle buckets in MinIO
   - Indexeert alle objecten
   - Tracked metadata changes

2. **Continuous Sync**
   - Draait elke 5 minuten (configureerbaar)
   - Updates object statistics
   - Genereert usage reports

3. **Smart Indexing**
   - Gebruikt GIN indexes voor JSONB metadata
   - Optimized queries voor snelle lookups
   - Efficient pagination

### Configuration

Via environment variables in docker-compose.yml:
```yaml
SYNC_MODE: continuous          # 'once' of 'continuous'
SYNC_INTERVAL_MINUTES: 5       # Hoe vaak te synchroniseren
```

## Database Views

### v_bucket_overview
Overzicht van alle buckets met human-readable statistieken:
```sql
SELECT * FROM minio_metadata.v_bucket_overview;
```

Toont:
- Bucket naam en ID
- Aantal objecten
- Totale grootte (in B/KB/MB/GB)
- Versioning status
- Laatste activiteit

### v_recent_activity
Recentste uploads/wijzigingen:
```sql
SELECT * FROM minio_metadata.v_recent_activity LIMIT 20;
```

### v_paperless_storage
Details over Paperless document storage:
```sql
SELECT * FROM minio_metadata.v_paperless_storage
WHERE paperless_document_id = 123;
```

## SQL Queries - Voorbeelden

### Storage Analytics

**Totale storage per bucket:**
```sql
SELECT 
    name,
    total_objects,
    pg_size_pretty(total_size_bytes::bigint) as storage_used
FROM minio_metadata.buckets
ORDER BY total_size_bytes DESC;
```

**Growth over time:**
```sql
SELECT 
    b.name,
    u.stat_date,
    u.unique_objects,
    pg_size_pretty(
        (SELECT SUM(size_bytes) 
         FROM minio_metadata.objects o 
         WHERE o.bucket_id = b.id 
         AND DATE(o.last_modified) <= u.stat_date)
    ) as cumulative_size
FROM minio_metadata.usage_stats u
JOIN minio_metadata.buckets b ON b.id = u.bucket_id
ORDER BY b.name, u.stat_date;
```

### Content Type Analytics

**Meest voorkomende file types:**
```sql
SELECT 
    content_type,
    COUNT(*) as count,
    pg_size_pretty(SUM(size_bytes)::bigint) as total_size,
    AVG(size_bytes)::bigint as avg_size_bytes
FROM minio_metadata.objects
WHERE is_latest = TRUE
GROUP BY content_type
ORDER BY count DESC
LIMIT 10;
```

### Search in Metadata

**Zoek objecten met specifieke metadata:**
```sql
SELECT 
    b.name as bucket,
    o.object_key,
    o.metadata
FROM minio_metadata.objects o
JOIN minio_metadata.buckets b ON b.id = o.bucket_id
WHERE o.metadata @> '{"author": "John Doe"}'::jsonb;
```

### Performance Metrics

**Upload/download statistieken per dag:**
```sql
SELECT 
    b.name,
    u.stat_date,
    u.put_requests as uploads,
    u.get_requests as downloads,
    pg_size_pretty(u.total_bytes_uploaded) as uploaded,
    pg_size_pretty(u.total_bytes_downloaded) as downloaded
FROM minio_metadata.usage_stats u
JOIN minio_metadata.buckets b ON b.id = u.bucket_id
WHERE u.stat_date >= CURRENT_DATE - INTERVAL '30 days'
ORDER BY u.stat_date DESC, b.name;
```

## Database Toegang

### Via Docker

```bash
# Verbind met PostgreSQL container
docker-compose exec db psql -U paperless -d paperless

# Of vanaf host (port 5433)
psql -h localhost -p 5433 -U paperless -d paperless
```

### Handige Commands

```sql
-- Schakel naar minio schema
SET search_path TO minio_metadata, public;

-- Lijst alle tabellen
\dt

-- Beschrijf een tabel
\d objects

-- Toon alle views
\dv

-- Query uitvoeren
SELECT * FROM v_bucket_overview;
```

## Triggers & Functions

### Automatische Statistieken

De database heeft triggers die automatisch bucket statistieken updaten:

```sql
-- Manual trigger:
SELECT update_bucket_stats('paperless-documents');
```

Dit update automatisch:
- `total_objects`
- `total_size_bytes`
- `updated_at`

## Monitoring & Maintenance

### Check Sync Status

```bash
# Bekijk sync service logs
docker-compose logs -f minio-sync

# Check laatste sync tijd
docker-compose exec db psql -U paperless -d paperless -c \
  "SELECT name, updated_at FROM minio_metadata.buckets;"
```

### Database Size

```sql
-- Grootte van minio_metadata schema
SELECT 
    schemaname,
    pg_size_pretty(SUM(pg_total_relation_size(schemaname||'.'||tablename))::bigint) 
FROM pg_tables 
WHERE schemaname = 'minio_metadata'
GROUP BY schemaname;
```

### Cleanup Old Logs

```sql
-- Verwijder access logs ouder dan 90 dagen
DELETE FROM minio_metadata.access_logs 
WHERE timestamp < CURRENT_DATE - INTERVAL '90 days';

-- Vacuum om ruimte vrij te maken
VACUUM ANALYZE minio_metadata.access_logs;
```

## Backup & Restore

### Backup MinIO Metadata

```bash
# Backup alleen minio_metadata schema
docker-compose exec db pg_dump \
  -U paperless \
  -d paperless \
  -n minio_metadata \
  --clean \
  --if-exists \
  > minio_metadata_backup.sql
```

### Restore

```bash
# Restore metadata
docker-compose exec -T db psql \
  -U paperless \
  -d paperless \
  < minio_metadata_backup.sql
```

## Integration met Bestaande postgres-backend

Deze MinIO setup kan geïntegreerd worden met je bestaande `postgres-backend`:

### Option 1: Shared Database

Beide services gebruiken dezelfde PostgreSQL instance:
- Paperless DMS data in `public` schema
- MinIO metadata in `minio_metadata` schema
- Woo-dashboard data in eigen schema

### Option 2: Separate Databases

Elk project eigen database, maar op dezelfde PostgreSQL 18 server.

### Extend Backend API

Je kunt de postgres-backend uitbreiden met MinIO endpoints:

```typescript
// Voorbeeld endpoint
app.get('/api/storage/stats', async (req, res) => {
  const result = await pool.query(`
    SELECT * FROM minio_metadata.v_bucket_overview
  `);
  res.json(result.rows);
});
```

## Performance Tips

1. **Indexing**
   - GIN indexes zijn al aanwezig voor JSONB columns
   - Voeg custom indexes toe voor specifieke queries

2. **Partitioning**
   - Voor `access_logs`: partition per maand
   - Voor `usage_stats`: partition per jaar

3. **Connection Pooling**
   - Sync service gebruikt pool (max 10)
   - Verhoog indien nodig

4. **Query Optimization**
   - Gebruik EXPLAIN ANALYZE
   - Monitor met pg_stat_statements

## Troubleshooting

### Sync Service Start Niet

```bash
# Check logs
docker-compose logs minio-sync

# Rebuild
docker-compose build minio-sync
docker-compose up -d minio-sync
```

### Geen Data in Database

```bash
# Manual sync
docker-compose exec minio-sync npm run sync
```

### Performance Issues

```sql
-- Check slow queries
SELECT query, mean_exec_time, calls 
FROM pg_stat_statements 
ORDER BY mean_exec_time DESC 
LIMIT 10;

-- Reindex if needed
REINDEX SCHEMA minio_metadata;
```

## Resources

- [PostgreSQL 18 Documentation](https://www.postgresql.org/docs/18/)
- [MinIO SDK Documentation](https://min.io/docs/minio/linux/developers/minio-drivers.html)
- [JSONB Performance](https://www.postgresql.org/docs/current/datatype-json.html)
