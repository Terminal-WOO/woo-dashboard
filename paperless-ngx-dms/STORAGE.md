# Paperless-ngx Storage Configuratie

## Huidige Situatie: Lokale File Storage

Paperless-ngx gebruikt momenteel **lokale file storage** in plaats van MinIO/S3. Dit is de standaard configuratie en werkt out-of-the-box.

### Waar worden documenten opgeslagen?

**In de container:**
- Documents: `/usr/src/paperless/media/documents/`
- Thumbnails: `/usr/src/paperless/media/thumbnails/`
- Data: `/usr/src/paperless/data/`

**Docker volumes:**
```yaml
volumes:
  - data:/usr/src/paperless/data
  - ./export:/usr/src/paperless/export
  - ./consume:/usr/src/paperless/consume
```

### Documenten bekijken

**Via Paperless Web UI:**
```bash
open http://localhost:8000
# Login: admin / admin
```

**Via Docker exec:**
```bash
# Lijst documenten
docker exec paperless-ngx-dms-webserver-1 ls -lh /usr/src/paperless/media/documents/

# Kopieer document naar lokale machine
docker cp paperless-ngx-dms-webserver-1:/usr/src/paperless/media/documents/originals/ ./documents/
```

**Via Volume inspect:**
```bash
# Vind volume locatie
docker volume inspect paperless-ngx-dms_data

# Volume locatie (macOS):
# /var/lib/docker/volumes/paperless-ngx-dms_data/_data
```

## MinIO S3 Storage (Experimenteel)

MinIO integratie is **complex** en vereist:

1. ✅ Custom Docker image met `django-storages[s3]` en `boto3`
2. ❌ Extra Python configuratie voor S3 backend
3. ❌ Custom `settings.py` override
4. ❌ Data migratie van lokale storage naar S3

### Wat werkt al:

- ✅ Custom Dockerfile met S3 packages (`paperless-ngx-dms/Dockerfile`)
- ✅ MinIO server draait (http://localhost:9001)
- ✅ Buckets worden aangemaakt (`paperless-documents`, `paperless-media`)
- ✅ Environment variables geconfigureerd

### Wat nog nodig is:

Om MinIO te gebruiken moet je:

1. **Custom settings.py maken:**
```python
# paperless-ngx-dms/custom_settings.py
from storages.backends.s3boto3 import S3Boto3Storage

class PaperlessS3Storage(S3Boto3Storage):
    bucket_name = 'paperless-documents'
    endpoint_url = 'http://minio:9000'
    access_key = 'minioadmin'
    secret_key = 'minioadmin'
    use_ssl = False
    file_overwrite = False
    default_acl = 'private'

DEFAULT_FILE_STORAGE = 'custom_settings.PaperlessS3Storage'
```

2. **Settings mounten in container:**
```yaml
volumes:
  - ./custom_settings.py:/usr/src/paperless/src/paperless/custom_settings.py
```

3. **Environment variable toevoegen:**
```yaml
environment:
  DJANGO_SETTINGS_MODULE: paperless.settings_with_custom
```

## Aanbeveling

**Gebruik de standaard lokale storage** voor deze POC/demo. Redenen:

1. ✅ Werkt out-of-the-box zonder extra configuratie
2. ✅ Sneller (geen netwerk overhead)
3. ✅ Eenvoudiger te debuggen
4. ✅ Perfecte backup via Docker volumes

MinIO wordt WEL gebruikt voor:
- ✅ Metadata tracking (PostgreSQL)
- ✅ Direct upload testing (MinIO console)
- ✅ S3-compatible API demonstratie

## MinIO Direct Access

Je kunt MinIO ook **direct** gebruiken zonder Paperless:

**MinIO Console:**
```bash
open http://localhost:9001
# Login: minioadmin / minioadmin
```

**Upload via MinIO Client:**
```bash
docker exec paperless-ngx-dms-minio-1 mc cp /tmp/test.pdf local/paperless-documents/
```

**Python SDK:**
```python
from minio import Minio

client = Minio(
    "localhost:9000",
    access_key="minioadmin",
    secret_key="minioadmin",
    secure=False
)

client.fput_object(
    "paperless-documents",
    "test.pdf",
    "/path/to/test.pdf"
)
```

## Conclusie

- **Paperless documenten**: Lokale file storage (Docker volume)
- **MinIO**: Beschikbaar voor direct gebruik via console/API
- **DMS Simulator**: Upload naar Paperless (lokale storage)
- **NATS Events**: Tracked alle uploads met metadata

Voor productie met S3/MinIO: overweeg managed Paperless hosting of volledige custom image met S3 configuratie.
