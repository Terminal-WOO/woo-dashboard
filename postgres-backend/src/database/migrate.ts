import { Pool } from 'pg';
import { readFileSync } from 'fs';
import { join } from 'path';

export async function migrate(pool: Pool): Promise<void> {
  const client = await pool.connect();

  try {
    console.log('Running database migrations...');

    // Read schema.sql
    const schemaPath = join(__dirname, 'schema.sql');
    const schemaSql = readFileSync(schemaPath, 'utf-8');

    // Execute schema in a transaction
    await client.query('BEGIN');
    await client.query(schemaSql);
    await client.query('COMMIT');

    console.log('✓ Database migrations completed successfully');
  } catch (error) {
    await client.query('ROLLBACK');
    console.error('✗ Migration failed:', error);
    throw error;
  } finally {
    client.release();
  }
}
