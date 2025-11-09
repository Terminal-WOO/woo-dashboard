import Fastify from 'fastify';
import cors from '@fastify/cors';
import dotenv from 'dotenv';
import { NATSEventClient, DocumentEvent } from './nats-client.js';

dotenv.config();

const fastify = Fastify({
  logger: {
    transport: {
      target: 'pino-pretty',
      options: {
        colorize: true,
        translateTime: 'SYS:standard',
      },
    },
  },
});

await fastify.register(cors, {
  origin: true,
});

const natsClient = new NATSEventClient(process.env.NATS_URL || 'nats://localhost:4222');

// Connect to NATS and subscribe
await natsClient.connect();
await natsClient.subscribe((event: DocumentEvent) => {
  fastify.log.info({ event }, 'Received document event');
});

// Health check
fastify.get('/health', async () => {
  return { status: 'ok', timestamp: new Date().toISOString() };
});

// Get all events
fastify.get<{
  Querystring: { limit?: number };
}>('/events', async (request) => {
  const limit = request.query.limit || 100;
  const events = natsClient.getEvents(limit);
  return { events, count: events.length };
});

// Get events by system
fastify.get<{
  Params: { system: 'paperless' | 'alfresco' };
  Querystring: { limit?: number };
}>('/events/system/:system', async (request) => {
  const { system } = request.params;
  const limit = request.query.limit || 100;
  const events = natsClient.getEventsBySystem(system, limit);
  return { events, count: events.length, system };
});

// Get events by type
fastify.get<{
  Params: { type: string };
  Querystring: { limit?: number };
}>('/events/type/:type', async (request) => {
  const type = `document.${request.params.type}` as DocumentEvent['eventType'];
  const limit = request.query.limit || 100;
  const events = natsClient.getEventsByType(type, limit);
  return { events, count: events.length, type };
});

// Get statistics
fastify.get('/stats', async () => {
  return natsClient.getStats();
});

// Server-Sent Events stream for real-time updates
fastify.get('/events/stream', async (request, reply) => {
  reply.raw.writeHead(200, {
    'Content-Type': 'text/event-stream',
    'Cache-Control': 'no-cache',
    'Connection': 'keep-alive',
  });

  const interval = setInterval(() => {
    const events = natsClient.getEvents(10);
    const data = JSON.stringify({ events, stats: natsClient.getStats() });
    reply.raw.write(`data: ${data}\n\n`);
  }, 2000);

  request.raw.on('close', () => {
    clearInterval(interval);
  });
});

// Start server
const start = async () => {
  try {
    const port = parseInt(process.env.PORT || '3000', 10);
    const host = process.env.HOST || '0.0.0.0';

    await fastify.listen({ port, host });

    fastify.log.info(`NATS Event Consumer running on http://${host}:${port}`);
    fastify.log.info(`NATS URL: ${process.env.NATS_URL || 'nats://localhost:4222'}`);
  } catch (err) {
    fastify.log.error(err);
    process.exit(1);
  }
};

// Graceful shutdown
process.on('SIGTERM', async () => {
  fastify.log.info('SIGTERM received, shutting down');
  await natsClient.close();
  await fastify.close();
  process.exit(0);
});

process.on('SIGINT', async () => {
  fastify.log.info('SIGINT received, shutting down');
  await natsClient.close();
  await fastify.close();
  process.exit(0);
});

start();
