# Custom Paperless-ngx with S3/MinIO support
FROM ghcr.io/paperless-ngx/paperless-ngx:latest

# Install S3 storage dependencies
USER root
RUN pip install --no-cache-dir \
    django-storages[s3]==1.14.2 \
    boto3==1.34.34

USER paperless

# Note: Configuration is done via environment variables in docker-compose.yml
# PAPERLESS_ENABLE_S3=true
# PAPERLESS_S3_ENDPOINT=http://minio:9000
# PAPERLESS_S3_ACCESS_KEY=minioadmin
# PAPERLESS_S3_SECRET_KEY=minioadmin
# PAPERLESS_S3_BUCKET_NAME=paperless-documents
