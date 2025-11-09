# Alfresco Community Edition DMS

Een complete Document Management Systeem setup gebaseerd op **Alfresco Community Edition 23.2** met PostgreSQL 18 en MinIO object storage.

## Over Alfresco

Alfresco is een enterprise-grade, open-source Document Management Systeem (DMS) en Enterprise Content Management (ECM) platform. Het wordt wereldwijd gebruikt door grote organisaties voor:

- Document management en versioning
- Records management en compliance
- Collaboration en workflows
- Content services en APIs
- Digital asset management

## Architectuur

Deze setup bevat:

```
┌──────────────────────────────────────────────────────────┐
│                  Alfresco Community DMS                   │
├──────────────────────────────────────────────────────────┤
│  Web Interfaces:                                          │
│  - Alfresco Share (Classic UI)         :8081             │
│  - Digital Workspace (Modern UI)       :8082             │
│  - Repository API                      :8080             │
├──────────────────────────────────────────────────────────┤
│  Core Services:                                           │
│  - Content Repository                                     │
│  - Search Services (Solr 6)                               │
│  - Transform Services                                     │
│  - ActiveMQ Message Broker                                │
├──────────────────────────────────────────────────────────┤
│  Storage Backend:                                         │
│  - PostgreSQL 18 (Metadata)            :5434             │
│  - MinIO (Content Storage)             :9002, :9003      │
│  - MinIO Metadata Sync                                   │
└──────────────────────────────────────────────────────────┘
```

## Belangrijkste Features

### Alfresco Features

- 📁 **Document Management**: Organiseer, beheer en beveilig documenten
- 🔄 **Versioning**: Automatische versiebeheer en audit trail
- 🔍 **Full-Text Search**: Krachtige zoekfunctionaliteit met Solr
- 👥 **Collaboration**: Delen, comments, workflows
- 📋 **Metadata & Taxonomie**: Uitgebreide metadata ondersteuning
- 🔐 **Permissions**: Granular access control en roles
- 🔌 **RESTful API**: Complete API voor integraties
- 📱 **Mobile Ready**: Responsive interfaces
- 🗂️ **Records Management**: Compliance en archivering
- 🔄 **Document Workflows**: Business process automation

### Technische Features

- 💾 **MinIO Storage**: S3-compatibele object storage voor content
- 🗄️ **PostgreSQL 18**: Moderne database voor metadata
- 📊 **Metadata Tracking**: Alle MinIO objecten in PostgreSQL
- 📈 **Usage Analytics**: Storage statistieken en analytics
- 🔄 **Auto-Sync**: Continuous synchronisatie MinIO ↔ PostgreSQL

## Installatie

### Vereisten

- Docker (24.0+)
- Docker Compose (2.0+)
- Minimaal 8GB RAM beschikbaar voor Docker
- 20GB vrije schijfruimte

### Hardware Aanbevelingen

- **Development**: 8GB RAM, 4 CPU cores
- **Production**: 16GB+ RAM, 8+ CPU cores, SSD storage

### Snelstart

```bash
cd alfresco-dms

# Start alle services
docker-compose up -d

# Volg de logs
docker-compose logs -f alfresco

# Wacht tot je ziet: "Server startup in [XXXXX] milliseconds"
```

**Eerste keer opstarten kan 5-10 minuten duren!**

### Toegang

Na succesvol opstarten:

| Service | URL | Credentials |
|---------|-----|-------------|
| **Alfresco Share** | http://localhost:8081/share | admin / admin |
| **Digital Workspace** | http://localhost:8082 | admin / admin |
| **Repository API** | http://localhost:8080/alfresco | admin / admin |
| **MinIO Console** | http://localhost:9003 | minioadmin / minioadmin |
| **PostgreSQL** | localhost:5434 | alfresco / alfresco |
| **Solr Admin** | http://localhost:8083/solr | - |
| **ActiveMQ Console** | http://localhost:8161/admin | admin / admin |

## Gebruik

### Upload Documenten

**Via Alfresco Share:**
1. Login op http://localhost:8081/share
2. Ga naar "Repository" → "User Homes" → "admin"
3. Click "Upload" en selecteer bestanden
4. Documenten worden automatisch geïndexeerd en in MinIO opgeslagen

**Via Digital Workspace:**
1. Login op http://localhost:8082
2. Click op "Personal Files"
3. Drag & drop bestanden
4. Modern, responsive interface

### Document Organizatie

**Folders & Structuur:**
- Maak folders aan voor organisatie
- Gebruik "Sites" voor team collaboration
- "Data Lists" voor gestructureerde data

**Metadata:**
- Add properties aan documenten
- Gebruik tags voor categorisatie
- Custom content types voor specifieke document types

**Versioning:**
- Automatisch bij elke update
- View version history
- Revert naar vorige versies

### Zoeken

**Quick Search:**
- Gebruik search bar bovenaan
- Full-text search in alle documenten
- Filter op type, datum, locatie

**Advanced Search:**
- Zoek op metadata
- Complexe queries
- Saved searches

### Workflows

**Document Approval:**
1. Select document
2. "Start Workflow"
3. Kies "Review and Approve"
4. Assign reviewers
5. Track progress

## PostgreSQL + MinIO Integratie

### Database Schema

De PostgreSQL database bevat:

1. **Alfresco Schema** (standaard): Nodes, properties, permissions, etc.
2. **MinIO Metadata Schema**: Bucket tracking, object metadata, Alfresco mappings

### SQL Queries

**Alfresco content storage overzicht:**
```sql
-- Verbind met database
psql -h localhost -p 5434 -U alfresco -d alfresco

-- Schakel naar minio schema
SET search_path TO minio_metadata;

-- Bekijk storage statistieken
SELECT * FROM v_bucket_overview;

-- Alfresco nodes met MinIO mapping
SELECT * FROM v_alfresco_storage LIMIT 20;

-- Storage per content type
SELECT 
    mime_type,
    COUNT(*) as documents,
    pg_size_pretty(SUM(size_bytes)::bigint) as total_size
FROM alfresco_node_mapping
GROUP BY mime_type
ORDER BY SUM(size_bytes) DESC;
```

### MinIO Content Store

Alfresco is geconfigureerd om content in MinIO op te slaan:

**Bucket: `alfresco-content`**
- Alle document content
- Versioned content
- Thumbnails en renditions

**Bucket: `alfresco-deleted`**
- Soft-deleted content
- Archived content

**Bekijk in MinIO Console:**
- http://localhost:9003
- Login: minioadmin / minioadmin
- Browse buckets en objecten

## Configuratie

### Alfresco Properties

Custom configuratie via `./config/alfresco-global.properties`:

```properties
# Database
db.driver=org.postgresql.Driver
db.url=jdbc:postgresql://postgres:5432/alfresco

# Content Store (MinIO S3)
connector.s3.bucketName=alfresco-content
connector.s3.endpoint=http://minio:9000

# Search
index.subsystem.name=solr6
solr.host=solr6

# Caching
cache.memoryStoreEvictionPolicy=LRU
cache.maxElementsInMemory=10000
```

### Memory Tuning

Pas geheugen aan in `docker-compose.yml`:

```yaml
environment:
  JAVA_OPTS: >
    -Xms2g -Xmx2g  # Verhoog indien nodig
```

### SSL/HTTPS

Voor productie: gebruik reverse proxy (Nginx/Traefik) met SSL certificaten.

## Beheer

### Logs Bekijken

```bash
# Alle services
docker-compose logs -f

# Specifieke service
docker-compose logs -f alfresco
docker-compose logs -f minio-sync
```

### Database Backup

```bash
# PostgreSQL backup
docker-compose exec postgres pg_dump -U alfresco alfresco > backup.sql

# MinIO metadata schema
docker-compose exec postgres pg_dump \
  -U alfresco \
  -d alfresco \
  -n minio_metadata \
  > minio_metadata_backup.sql
```

### Content Backup (MinIO)

```bash
# Backup MinIO buckets
docker run --rm \
  --network alfresco-network \
  -v $(pwd)/backup:/backup \
  minio/mc \
  mirror alfminio/alfresco-content /backup/content
```

### Health Checks

```bash
# Check service status
docker-compose ps

# Check Alfresco repository
curl http://localhost:8080/alfresco/api/-default-/public/alfresco/versions/1/probes/-ready-

# Check Solr
curl http://localhost:8083/solr/admin/cores?action=STATUS

# Check MinIO
curl http://localhost:9002/minio/health/live
```

### Performance Monitoring

**PostgreSQL:**
```sql
-- Top queries
SELECT query, mean_exec_time, calls
FROM pg_stat_statements
ORDER BY mean_exec_time DESC
LIMIT 10;

-- Database size
SELECT pg_size_pretty(pg_database_size('alfresco'));
```

**Alfresco JMX:**
- Enable JMX in JAVA_OPTS
- Use JConsole/VisualVM voor monitoring

## Troubleshooting

### Service Start Niet

```bash
# Check logs voor errors
docker-compose logs alfresco

# Restart service
docker-compose restart alfresco

# Rebuild indien nodig
docker-compose down
docker-compose up -d --build
```

### Out of Memory

```bash
# Verhoog geheugen in docker-compose.yml
# Voor Alfresco:
-Xms2g -Xmx2g  # of hoger

# Check Docker memory limit
docker stats
```

### Search Werkt Niet

```bash
# Rebuild Solr index
# Via Admin Console: http://localhost:8081/share/page/console/admin-console/search-manager
# Of via API:
curl -u admin:admin \
  http://localhost:8080/alfresco/service/api/solr/admin/reindex
```

### Database Connection Errors

```bash
# Check PostgreSQL
docker-compose exec postgres psql -U alfresco -d alfresco -c "SELECT version();"

# Check connections
docker-compose exec postgres psql -U alfresco -d alfresco -c \
  "SELECT count(*) FROM pg_stat_activity WHERE datname='alfresco';"
```

### MinIO Connection Issues

```bash
# Test MinIO connectivity
docker-compose exec alfresco curl http://minio:9000/minio/health/live

# Check buckets
docker-compose exec minio mc ls local/
```

## Beveiliging

### ⚠️ PRODUCTIE CHECKLIST

1. **Wijzig Wachtwoorden:**
   - Alfresco admin: Via Share UI
   - PostgreSQL: In docker-compose.yml
   - MinIO: In docker-compose.yml
   - ActiveMQ: In container config

2. **SSL/TLS:**
   - Gebruik reverse proxy
   - Certificaten voor alle endpoints

3. **Firewall:**
   - Expose alleen noodzakelijke ports
   - Gebruik Docker networks

4. **Backups:**
   - Dagelijkse database backups
   - Content store replicatie
   - Test restore procedures

5. **Updates:**
   - Monitor security advisories
   - Regular updates
   - Test in staging eerst

## Advanced Features

### CMIS API

Alfresco ondersteunt CMIS (Content Management Interoperability Services):

```bash
# CMIS Browser Binding
http://localhost:8080/alfresco/api/-default-/public/cmis/versions/1.1/browser

# CMIS AtomPub Binding
http://localhost:8080/alfresco/api/-default-/public/cmis/versions/1.1/atom
```

### REST API

```bash
# Get repository info
curl -u admin:admin \
  http://localhost:8080/alfresco/api/discovery

# List sites
curl -u admin:admin \
  http://localhost:8080/alfresco/api/-default-/public/alfresco/versions/1/sites

# Upload document
curl -u admin:admin \
  -X POST \
  -F filedata=@document.pdf \
  http://localhost:8080/alfresco/api/upload
```

### Extensibility

**Custom Extensions:**
- Place JARs in `./config/modules/platform/`
- Place AMPs in containers
- Restart Alfresco

**Share Customizations:**
- Custom dashlets
- Custom actions
- Custom forms

## Resources

- [Alfresco Documentation](https://docs.alfresco.com/content-services/community/)
- [Alfresco Hub](https://hub.alfresco.com/)
- [GitHub Repository](https://github.com/Alfresco/alfresco-community-repo)
- [CMIS Specification](https://www.oasis-open.org/committees/cmis/)
- [REST API Explorer](http://localhost:8080/api-explorer)

## Directory Structuur

```
alfresco-dms/
├── docker-compose.yml           # Main configuration
├── init-db/                     # PostgreSQL init scripts
│   └── 01-create-minio-schema.sql
├── minio-metadata-sync/         # MinIO → PostgreSQL sync
│   ├── src/
│   ├── package.json
│   └── Dockerfile
├── config/                      # Alfresco extensions
│   └── alfresco-global.properties
├── data/                        # Runtime data
├── logs/                        # Application logs
└── README.md                    # This file
```

## Licentie

Alfresco Community Edition is gelicenseerd onder de GNU Lesser General Public License v3.0.

Deze setup is gemaakt voor educatieve en productie doeleinden.

## Support

Voor vragen en issues:
- Check de logs
- Raadpleeg Alfresco documentatie
- Alfresco Community Forums
- GitHub Issues
