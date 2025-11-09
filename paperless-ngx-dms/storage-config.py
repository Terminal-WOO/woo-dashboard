# Custom storage configuration for MinIO (local S3-compatible storage)
# This file configures Paperless-ngx to use local MinIO instead of AWS

import os
from storages.backends.s3boto3 import S3Boto3Storage

class MinIODocumentStorage(S3Boto3Storage):
    """Custom storage class for MinIO document storage"""
    bucket_name = os.getenv('MINIO_DOCUMENTS_BUCKET', 'paperless-documents')
    endpoint_url = os.getenv('MINIO_ENDPOINT_URL', 'http://minio:9000')
    access_key = os.getenv('MINIO_ACCESS_KEY', 'minioadmin')
    secret_key = os.getenv('MINIO_SECRET_KEY', 'minioadmin')
    region_name = None
    use_ssl = False
    verify = False
    file_overwrite = False
    default_acl = 'private'

class MinIOMediaStorage(S3Boto3Storage):
    """Custom storage class for MinIO media storage"""
    bucket_name = os.getenv('MINIO_MEDIA_BUCKET', 'paperless-media')
    endpoint_url = os.getenv('MINIO_ENDPOINT_URL', 'http://minio:9000')
    access_key = os.getenv('MINIO_ACCESS_KEY', 'minioadmin')
    secret_key = os.getenv('MINIO_SECRET_KEY', 'minioadmin')
    region_name = None
    use_ssl = False
    verify = False
    file_overwrite = False
    default_acl = 'private'
