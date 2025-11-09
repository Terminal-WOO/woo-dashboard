# MinIO Object Storage - Technische Uitleg

## Wat is MinIO?

MinIO is een **high-performance, S3-compatibele object storage** oplossing die je **lokaal** kunt draaien. Het is **100% open-source** en wordt veel gebruikt als alternatief voor cloud storage services.

## Waarom MinIO voor Document Storage?

### Voordelen

1. **Lokale Controle**: Al je documenten blijven op je eigen infrastructuur
2. **S3-Compatible**: Gebruikt hetzelfde protocol als Amazon S3, maar dan lokaal
3. **Schaalbaar**: Eenvoudig uit te breiden naar meerdere servers/disks
4. **Hoge Performance**: Geoptimaliseerd voor snelle I/O operaties
5. **Geen Cloud Kosten**: Geen uitgaande data transfer kosten
6. **Privacy**: Documenten verlaten nooit je eigen netwerk

### Gebruik Cases

- **Development**: Test S3-integraties zonder cloud account
- **On-Premise**: Bedrijven die data lokaal willen houden
- **Hybrid Cloud**: Combinatie van lokale en cloud storage
- **Backup**: Lokale backup voordat je naar cloud sync't

## Architectuur in deze Setup

```
┌─────────────────┐
│  Paperless-ngx  │
│   (Webserver)   │
└────────┬────────┘
         │
         │ S3 Protocol
         │
┌────────▼────────┐      ┌──────────────┐
│     MinIO       │◄────►│ MinIO Console│
│  Object Storage │      │   (Web UI)   │
└────────┬────────┘      └──────────────┘
         │
         │
    ┌────▼─────┐
    │  Buckets │
    │──────────│
    │ paperless-documents  │  ← Originele documenten
    │ paperless-media      │  ← Thumbnails, previews
    └──────────┘
```

## Buckets Uitleg

Een **bucket** is zoals een "top-level folder" in object storage:

- `paperless-documents`: Bevat alle geüploade documenten (PDF, DOCX, etc.)
- `paperless-media`: Bevat gegenereerde thumbnails en previews

## S3 Protocol vs AWS Cloud

### Belangrijk: Dit is GEEN AWS!

De variabelen in de configuratie beginnen met `AWS_` maar dat betekent **NIET** dat we Amazon Web Services gebruiken. Hier is waarom:

1. **Django's S3 Storage Library** gebruikt deze naam
2. Het **S3 protocol** is de industry standard voor object storage
3. MinIO **implementeert** het S3 protocol, maar draait **lokaal**

### Analogie

Denk aan HTTP:
- **HTTP** = het protocol
- **Apache/Nginx** = servers die HTTP implementeren
- Je zegt niet "ik gebruik Apache-protocol", je zegt "ik gebruik HTTP"

Hetzelfde geldt hier:
- **S3** = het protocol
- **AWS/MinIO** = services die S3 implementeren
- De library heet `boto3` (AWS SDK), maar werkt met elke S3-compatible service

## MinIO Console

De MinIO Console is een web interface waar je:

- Buckets kunt bekijken
- Bestanden kunt uploaden/downloaden
- Access policies kunt beheren
- Monitoring/metrics kunt zien
- Users/Access Keys kunt beheren

**Toegang**: http://localhost:9001

## Data Persistentie

Data wordt opgeslagen in een Docker volume: `minio_data`

Dit betekent:
- ✅ Data blijft bewaard na container restart
- ✅ Onafhankelijk van container lifecycle
- ⚠️ Bij `docker-compose down -v` wordt data VERWIJDERD

## Security Overwegingen

### Development Setup (huidige configuratie)

- Credentials: `minioadmin` / `minioadmin` (DEFAULTS!)
- Geen SSL/TLS
- Buckets zijn publiek leesbaar (voor Paperless toegang)

### Productie Aanbevelingen

1. **Wijzig Credentials**:
   ```yaml
   MINIO_ROOT_USER: jouw-veilige-username
   MINIO_ROOT_PASSWORD: jouw-sterke-wachtwoord-min-8-chars
   ```

2. **Enable SSL**:
   - Genereer certificaten
   - Mount ze in MinIO container
   - Update `MINIO_USE_SSL=true`

3. **Access Policies**:
   - Maak dedicated access keys (niet root user)
   - Beperk bucket permissions
   - Gebruik bucket policies voor fine-grained control

4. **Network Isolation**:
   - Expose alleen noodzakelijke ports
   - Gebruik Docker networks voor service isolatie
   - Overweeg reverse proxy (Nginx/Traefik)

## Monitoring

### Check MinIO Status

```bash
docker-compose logs minio
```

### Check Buckets

Via MinIO Console (http://localhost:9001) of via CLI:

```bash
docker-compose exec minio mc ls local/
```

### Storage Usage

In MinIO Console → Buckets → klik op bucket naam → zie usage stats

## Backup Strategie

### Option 1: Volume Backup

```bash
docker run --rm \
  -v paperless-ngx-dms_minio_data:/data \
  -v $(pwd)/backup:/backup \
  ubuntu tar czf /backup/minio-backup.tar.gz -C /data .
```

### Option 2: MinIO Mirror

```bash
# Installeer mc (MinIO Client)
docker run --rm -it --network paperless-ngx-dms_default \
  minio/mc \
  mirror local/paperless-documents /backup/documents
```

### Option 3: Replicatie naar Andere MinIO

Configureer bucket replication naar een tweede MinIO instance (off-site backup).

## Troubleshooting

### Buckets worden niet aangemaakt

Check `minio-init` service logs:
```bash
docker-compose logs minio-init
```

Herstart indien nodig:
```bash
docker-compose restart minio-init
```

### Connection refused errors

1. Check of MinIO draait: `docker-compose ps minio`
2. Check health: `docker-compose exec minio curl http://localhost:9000/minio/health/live`
3. Check network: `docker-compose exec webserver ping minio`

### Kan bestanden niet uploaden

1. Check bucket permissions in MinIO Console
2. Verify credentials in docker-compose.yml
3. Check Paperless logs: `docker-compose logs webserver`

## Alternatieve Storage Backends

Als MinIO niet past bij je use-case:

- **Local Filesystem**: Verwijder MinIO, gebruik Docker volumes
- **NFS/SMB**: Mount network storage in container
- **AWS S3**: Wijzig `AWS_S3_ENDPOINT_URL` naar AWS
- **Google Cloud Storage**: Gebruik `django-storages[google]`
- **Azure Blob**: Gebruik `django-storages[azure]`

## Resources

- [MinIO Documentation](https://min.io/docs/)
- [MinIO Docker Hub](https://hub.docker.com/r/minio/minio)
- [S3 Protocol Specification](https://docs.aws.amazon.com/AmazonS3/latest/API/)
- [Django Storages](https://django-storages.readthedocs.io/)
