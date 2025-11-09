import { Client as MinioClient } from 'minio';
import { Pool } from 'pg';
import pino from 'pino';
import dotenv from 'dotenv';

dotenv.config();

const logger = pino({
  transport: {
    target: 'pino-pretty',
    options: {
      colorize: true,
      translateTime: 'SYS:standard',
    },
  },
});

// MinIO client configuration
const minioClient = new MinioClient({
  endPoint: process.env.MINIO_ENDPOINT || 'localhost',
  port: parseInt(process.env.MINIO_PORT || '9000'),
  useSSL: process.env.MINIO_USE_SSL === 'true',
  accessKey: process.env.MINIO_ACCESS_KEY || 'minioadmin',
  secretKey: process.env.MINIO_SECRET_KEY || 'minioadmin',
});

// PostgreSQL pool configuration
const pool = new Pool({
  host: process.env.POSTGRES_HOST || 'localhost',
  port: parseInt(process.env.POSTGRES_PORT || '5433'),
  database: process.env.POSTGRES_DB || 'paperless',
  user: process.env.POSTGRES_USER || 'paperless',
  password: process.env.POSTGRES_PASSWORD || 'paperless',
  max: 10,
});

interface BucketInfo {
  name: string;
  creationDate: Date;
}

interface ObjectInfo {
  name: string;
  lastModified: Date;
  etag: string;
  size: number;
  versionId?: string;
  isDeleteMarker?: boolean;
  metadata?: Record<string, string>;
}

/**
 * Sync bucket information to PostgreSQL
 */
async function syncBucket(bucketName: string): Promise<void> {
  const client = await pool.connect();
  try {
    logger.info({ bucket: bucketName }, 'Syncing bucket metadata');

    // Get bucket statistics from MinIO
    const exists = await minioClient.bucketExists(bucketName);
    if (!exists) {
      logger.warn({ bucket: bucketName }, 'Bucket does not exist in MinIO');
      return;
    }

    // Insert or update bucket in database
    await client.query(
      `INSERT INTO minio_metadata.buckets (name, updated_at)
       VALUES ($1, CURRENT_TIMESTAMP)
       ON CONFLICT (name)
       DO UPDATE SET updated_at = CURRENT_TIMESTAMP`,
      [bucketName]
    );

    logger.info({ bucket: bucketName }, 'Bucket metadata synced');
  } catch (error) {
    logger.error({ error, bucket: bucketName }, 'Failed to sync bucket');
    throw error;
  } finally {
    client.release();
  }
}

/**
 * Sync all objects in a bucket to PostgreSQL
 */
async function syncBucketObjects(bucketName: string): Promise<void> {
  const client = await pool.connect();
  try {
    logger.info({ bucket: bucketName }, 'Syncing bucket objects');

    // Get bucket ID from database
    const bucketResult = await client.query(
      'SELECT id FROM minio_metadata.buckets WHERE name = $1',
      [bucketName]
    );

    if (bucketResult.rows.length === 0) {
      logger.warn({ bucket: bucketName }, 'Bucket not found in database');
      return;
    }

    const bucketId = bucketResult.rows[0].id;
    let objectCount = 0;

    // List all objects in bucket
    const stream = minioClient.listObjectsV2(bucketName, '', true);

    for await (const obj of stream) {
      if (!obj.name) continue;

      // Get object metadata
      let contentType = 'application/octet-stream';
      let metadata: Record<string, any> = {};

      try {
        const stat = await minioClient.statObject(bucketName, obj.name);
        contentType = stat.metaData['content-type'] || contentType;
        metadata = stat.metaData || {};
      } catch (error) {
        logger.warn({ object: obj.name }, 'Failed to get object metadata');
      }

      // Insert or update object in database
      await client.query(
        `INSERT INTO minio_metadata.objects
         (bucket_id, object_key, etag, size_bytes, content_type, last_modified, metadata, is_latest)
         VALUES ($1, $2, $3, $4, $5, $6, $7, TRUE)
         ON CONFLICT (bucket_id, object_key, version_id)
         DO UPDATE SET
           etag = EXCLUDED.etag,
           size_bytes = EXCLUDED.size_bytes,
           content_type = EXCLUDED.content_type,
           last_modified = EXCLUDED.last_modified,
           metadata = EXCLUDED.metadata,
           is_latest = TRUE`,
        [
          bucketId,
          obj.name,
          obj.etag,
          obj.size,
          contentType,
          obj.lastModified,
          JSON.stringify(metadata),
        ]
      );

      objectCount++;

      if (objectCount % 100 === 0) {
        logger.info({ bucket: bucketName, count: objectCount }, 'Synced objects...');
      }
    }

    logger.info({ bucket: bucketName, total: objectCount }, 'All objects synced');
  } catch (error) {
    logger.error({ error, bucket: bucketName }, 'Failed to sync bucket objects');
    throw error;
  } finally {
    client.release();
  }
}

/**
 * Generate usage statistics
 */
async function generateUsageStats(): Promise<void> {
  const client = await pool.connect();
  try {
    logger.info('Generating usage statistics');

    // This would normally aggregate from access_logs
    // For now, we just update based on current state
    const result = await client.query(`
      INSERT INTO minio_metadata.usage_stats
        (bucket_id, stat_date, unique_objects)
      SELECT
        bucket_id,
        CURRENT_DATE,
        COUNT(*)
      FROM minio_metadata.objects
      WHERE is_latest = TRUE AND is_delete_marker = FALSE
      GROUP BY bucket_id
      ON CONFLICT (bucket_id, stat_date)
      DO UPDATE SET
        unique_objects = EXCLUDED.unique_objects
    `);

    logger.info({ rows: result.rowCount }, 'Usage stats updated');
  } catch (error) {
    logger.error({ error }, 'Failed to generate usage stats');
    throw error;
  } finally {
    client.release();
  }
}

/**
 * Main sync function
 */
async function sync(): Promise<void> {
  try {
    logger.info('Starting MinIO metadata sync');

    // Test database connection
    const client = await pool.connect();
    logger.info('Connected to PostgreSQL');
    client.release();

    // Get list of buckets from MinIO
    const buckets = await minioClient.listBuckets();
    logger.info({ count: buckets.length }, 'Found buckets in MinIO');

    // Sync each bucket
    for (const bucket of buckets) {
      await syncBucket(bucket.name);
      await syncBucketObjects(bucket.name);
    }

    // Generate usage statistics
    await generateUsageStats();

    logger.info('Sync completed successfully');
  } catch (error) {
    logger.error({ error }, 'Sync failed');
    throw error;
  }
}

/**
 * Start continuous sync with interval
 */
async function startContinuousSync(intervalMinutes: number = 5): Promise<void> {
  logger.info({ intervalMinutes }, 'Starting continuous sync');

  // Initial sync
  await sync();

  // Schedule periodic sync
  setInterval(async () => {
    try {
      await sync();
    } catch (error) {
      logger.error({ error }, 'Scheduled sync failed');
    }
  }, intervalMinutes * 60 * 1000);

  logger.info('Continuous sync started');
}

// Run sync based on mode
const mode = process.env.SYNC_MODE || 'continuous';
const interval = parseInt(process.env.SYNC_INTERVAL_MINUTES || '5');

if (mode === 'once') {
  sync()
    .then(() => {
      logger.info('One-time sync completed');
      process.exit(0);
    })
    .catch((error) => {
      logger.error({ error }, 'One-time sync failed');
      process.exit(1);
    });
} else {
  startContinuousSync(interval).catch((error) => {
    logger.error({ error }, 'Failed to start continuous sync');
    process.exit(1);
  });
}

// Graceful shutdown
process.on('SIGTERM', async () => {
  logger.info('SIGTERM received, shutting down');
  await pool.end();
  process.exit(0);
});

process.on('SIGINT', async () => {
  logger.info('SIGINT received, shutting down');
  await pool.end();
  process.exit(0);
});
