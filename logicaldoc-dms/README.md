# LogicalDOC Community Edition met MinIO

LogicalDOC is een lightweight, open-source Document Management System met native S3/MinIO integratie.

## Features

- ✅ Native MinIO/S3 storage support
- ✅ PostgreSQL database backend
- ✅ Modern web interface
- ✅ REST API voor integratie
- ✅ OCR ondersteuning
- ✅ Versie beheer
- ✅ Full-text search
- ✅ Workflow management
- ✅ Docker-ready

## Quick Start

```bash
# Start alle services
docker-compose up -d

# Check status
docker-compose ps

# Bekijk logs
docker-compose logs -f logicaldoc

# Stop alle services
docker-compose down

# Stop en verwijder volumes (clean slate)
docker-compose down -v
```

## Toegang

- **LogicalDOC Web UI**: http://localhost:8082
  - Username: `admin`
  - Password: `admin`

- **MinIO Console**: http://localhost:9003
  - Username: `minioadmin`
  - Password: `minioadmin`

- **MinIO API**: http://localhost:9002

## Eerste Keer Opstarten

1. Start de services: `docker-compose up -d`
2. Wacht 2-3 minuten voor initialisatie
3. Open http://localhost:8082
4. Login met admin/admin
5. Ga naar Settings > Repositories om S3 configuratie te verifiëren

## Architectuur

```
┌─────────────────┐
│   Web Browser   │
└────────┬────────┘
         │ :8082
         ▼
┌─────────────────┐      ┌──────────────┐
│   LogicalDOC    │─────▶│  PostgreSQL  │
│                 │      │   Database   │
└────────┬────────┘      └──────────────┘
         │
         │ S3 Protocol
         ▼
┌─────────────────┐
│      MinIO      │
│   Object Store  │
│  (:9002, :9003) │
└─────────────────┘
```

## Storage Locaties

- **Database data**: Docker volume `postgres_data`
- **MinIO documents**: Docker volume `minio_data`
- **LogicalDOC config**: Docker volume `logicaldoc_conf`
- **LogicalDOC logs**: Docker volume `logicaldoc_logs`

## REST API

LogicalDOC heeft een RESTful API op:
- Base URL: `http://localhost:8082/services/rest`
- Documentatie: http://localhost:8082/api-docs (na login)

Voorbeeld endpoints:
- `GET /documents/{id}` - Get document info
- `POST /documents` - Upload document
- `GET /folders` - List folders
- `POST /folders` - Create folder

## DMS Simulator Integratie

De DMS Simulator kan documenten uploaden naar LogicalDOC:

```bash
cd dms-simulator
# Set LOGICALDOC_ENABLED=true in .env
npm start
```

## Troubleshooting

### LogicalDOC start niet op
```bash
# Check logs
docker logs logicaldoc-app

# Check database connectie
docker exec logicaldoc-postgres pg_isready -U logicaldoc
```

### MinIO bucket niet gevonden
```bash
# Recreate bucket
docker-compose restart minio-init
```

### Performance problemen
- Verhoog LDOC_MEMORY in docker-compose.yml
- Check disk space: `docker system df`

## Configuratie Aanpassen

Bewerk `docker-compose.yml` environment variabelen:

```yaml
environment:
  # Database
  LDOC_DB_TYPE: postgres
  LDOC_DB_HOST: postgres
  
  # Memory (MB)
  LDOC_MEMORY: 2048
  
  # S3/MinIO
  LDOC_REPOSITORY_TYPE: s3
  LDOC_S3_ENDPOINT: http://minio:9000
  LDOC_S3_BUCKET: logicaldoc-docs
```

## Vergelijking met Andere DMS

| Feature | LogicalDOC CE | Paperless-ngx | Alfresco CE |
|---------|---------------|---------------|-------------|
| Setup Tijd | ⚡ 5 min | ⚡ 5 min | ⏰ 30+ min |
| MinIO Support | ✅ Native | ⚠️ Complex | ⚠️ Enterprise only |
| Web UI | ✅ Modern | ✅ Excellent | ✅ Professional |
| REST API | ✅ Good | ✅ Excellent | ✅ Comprehensive |
| OCR | ✅ | ✅ Excellent | ✅ |
| Complexiteit | 🟢 Low | 🟢 Low | 🔴 High |

## Resources

- [LogicalDOC Documentation](https://docs.logicaldoc.com/)
- [Docker Hub](https://hub.docker.com/r/logicaldoc/logicaldoc-ce)
- [REST API Guide](https://docs.logicaldoc.com/en/web-services)
- [MinIO Documentation](https://min.io/docs/minio/linux/index.html)

## Licentie

LogicalDOC Community Edition is open-source onder LGPL v3 licentie.
