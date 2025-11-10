# Alfresco with MinIO S3 Storage

Complete Alfresco Community Edition setup with MinIO S3-compatible object storage.

## Architecture

```
Alfresco Repository
    ↓
MinIO S3 Storage (alfresco-content bucket)
    ↓
PostgreSQL (metadata)
```

## Quick Start

```bash
cd alfresco-dms
docker-compose -f docker-compose-minio.yml up -d
```

## Services

| Service | URL | Credentials | Description |
|---------|-----|-------------|-------------|
| **Alfresco Share** | http://localhost:8081/share | admin/admin | Web UI |
| **Alfresco Repository** | http://localhost:8080/alfresco | admin/admin | REST API |
| **MinIO Console** | http://localhost:9001 | minioadmin/minioadmin | S3 Storage UI |
| **ActiveMQ Console** | http://localhost:8161/admin | admin/admin | Message Queue |
| **PostgreSQL** | localhost:5434 | alfresco/alfresco | Database |

## MinIO Configuration

### S3 Connector Settings

Alfresco is configured with the S3 Connector:

```properties
connector.s3.enabled=true
connector.s3.bucketName=alfresco-content
connector.s3.endpoint=http://minio:9000
connector.s3.accessKey=minioadmin
connector.s3.secretKey=minioadmin
connector.s3.pathStyleAccess=true
```

### Bucket Structure

**Bucket**: `alfresco-content`
- Contains all document binaries
- Metadata stored in PostgreSQL
- Accessible via MinIO Console

### Verify MinIO Storage

**Via MinIO Console:**
```bash
open http://localhost:9001
# Login: minioadmin / minioadmin
# Navigate to Buckets → alfresco-content
```

**Via MinIO Client:**
```bash
docker exec alfresco-minio mc ls local/alfresco-content/
```

**Via Alfresco API:**
```bash
# Upload test document
curl -u admin:admin \
  -F filedata=@test.pdf \
  -F filename=test.pdf \
  http://localhost:8080/alfresco/api/-default-/public/alfresco/versions/1/nodes/-my-/children

# Check MinIO - document should appear in bucket
```

## Usage

### Upload Document to Alfresco

**Via Share UI:**
1. Open http://localhost:8081/share
2. Login: admin/admin
3. Go to "Repository" → "Shared"
4. Click "Upload" and select file
5. Check MinIO Console - file appears in `alfresco-content` bucket

**Via REST API:**
```bash
curl -u admin:admin \
  -X POST \
  -F filedata=@document.pdf \
  -F name=document.pdf \
  "http://localhost:8080/alfresco/api/-default-/public/alfresco/versions/1/nodes/-my-/children"
```

**Via DMS Simulator:**
```typescript
// Configure in dms-simulator/.env
ALFRESCO_ENABLED=true
ALFRESCO_URL=http://localhost:8080
ALFRESCO_USERNAME=admin
ALFRESCO_PASSWORD=admin
```

### Check Storage Location

After uploading, verify document is in MinIO:

```bash
# List bucket contents
docker exec alfresco-minio mc ls local/alfresco-content/

# Check bucket statistics
docker exec alfresco-minio mc du local/alfresco-content/
```

## Storage Path

Documents in MinIO follow this structure:
```
alfresco-content/
├── 2024/
│   ├── 11/
│   │   ├── 10/
│   │   │   ├── 14/
│   │   │   │   ├── 30/
│   │   │   │   │   └── [uuid].bin
```

Alfresco stores content-addressed binaries with UUIDs.

## Benefits of MinIO Storage

1. ✅ **S3-Compatible**: Works with AWS S3 SDK
2. ✅ **Scalable**: Unlimited storage capacity
3. ✅ **Cost-Effective**: Free, open-source
4. ✅ **Local**: No cloud costs, full control
5. ✅ **Fast**: Object storage optimized for large files
6. ✅ **Production-Ready**: Used by major enterprises

## Comparison: Paperless vs Alfresco

| Feature | Paperless-ngx | Alfresco |
|---------|---------------|----------|
| S3 Support | ❌ Complex (requires custom image) | ✅ Built-in S3 Connector |
| Storage | Local filesystem | MinIO S3 buckets |
| Setup | Simple | Enterprise (complex) |
| Use Case | Personal/Small team | Enterprise ECM |

## Monitoring

### Check Alfresco Storage Stats

```bash
# Via Admin Console
open http://localhost:8080/alfresco/s/enterprise/admin/admin-systemsummary

# Via API
curl -u admin:admin \
  http://localhost:8080/alfresco/api/-default-/public/alfresco/versions/1/probes/-ready-
```

### Check MinIO Stats

```bash
# Via MinIO Client
docker exec alfresco-minio mc admin info local/

# Via Console
open http://localhost:9001
# Navigate to Dashboard
```

## Troubleshooting

### Documents not appearing in MinIO

```bash
# Check Alfresco logs
docker logs alfresco-repository | grep -i "s3\|minio"

# Verify S3 connector is enabled
docker exec alfresco-repository \
  grep "connector.s3.enabled" /usr/local/tomcat/shared/classes/alfresco-global.properties
```

### MinIO connection issues

```bash
# Test MinIO from Alfresco container
docker exec alfresco-repository curl -I http://minio:9000

# Check network
docker network inspect alfresco-dms_alfresco-network
```

### Reset and start fresh

```bash
# Stop and remove everything
docker-compose -f docker-compose-minio.yml down -v

# Start clean
docker-compose -f docker-compose-minio.yml up -d
```

## System Requirements

- **RAM**: Minimum 4GB, Recommended 8GB
- **Disk**: 20GB for Docker images + storage
- **CPU**: 2+ cores recommended
- **Startup Time**: ~5-10 minutes for full initialization

## Integration with DMS Simulator

The DMS Simulator can upload to Alfresco with MinIO:

1. Start Alfresco: `docker-compose -f docker-compose-minio.yml up -d`
2. Wait 5-10 minutes for initialization
3. Configure simulator: Set `ALFRESCO_ENABLED=true` in `dms-simulator/.env`
4. Run simulation from dashboard
5. Documents appear in:
   - Alfresco Share UI
   - MinIO Console (`alfresco-content` bucket)
   - PostgreSQL metadata

## Production Considerations

For production use:

1. Change default passwords
2. Enable HTTPS/TLS
3. Configure backup strategy
4. Set up monitoring (Prometheus/Grafana)
5. Configure retention policies
6. Enable MinIO versioning
7. Set up MinIO replication

## Resources

- [Alfresco S3 Connector Docs](https://docs.alfresco.com/content-services/latest/admin/content-stores/)
- [MinIO Documentation](https://min.io/docs/minio/linux/index.html)
- [Alfresco REST API](https://api-explorer.alfresco.com/)
