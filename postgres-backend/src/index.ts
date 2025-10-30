import { Pool } from 'pg';
import dotenv from 'dotenv';
import { migrate } from './database/migrate.js';
import { seed } from './database/seed.js';
import { createServer } from './server.js';

// Load environment variables
dotenv.config();

async function main() {
  console.log('🚀 Starting PostgreSQL WOO Backend...\n');

  // Create PostgreSQL connection pool
  const pool = new Pool({
    connectionString: process.env.DATABASE_URL,
    max: 20,
    idleTimeoutMillis: 30000,
    connectionTimeoutMillis: 2000,
  });

  try {
    // Test database connection
    const client = await pool.connect();
    console.log('✓ Connected to PostgreSQL database');
    client.release();

    // Run migrations
    await migrate(pool);

    // Seed initial data
    await seed(pool);

    // Start Fastify server
    const server = await createServer(pool);
    const port = parseInt(process.env.PORT || '8081', 10);
    const host = process.env.HOST || '0.0.0.0';

    await server.listen({ port, host });
    console.log(`\n✓ Server listening on http://${host}:${port}`);
    console.log(`✓ API available at http://${host}:${port}/api`);
    console.log('\nPress Ctrl+C to stop the server\n');

  } catch (error) {
    console.error('✗ Failed to start server:', error);
    process.exit(1);
  }

  // Graceful shutdown
  const shutdown = async (signal: string) => {
    console.log(`\n${signal} received, shutting down gracefully...`);
    await pool.end();
    console.log('✓ Database connections closed');
    process.exit(0);
  };

  process.on('SIGTERM', () => shutdown('SIGTERM'));
  process.on('SIGINT', () => shutdown('SIGINT'));
}

main();
