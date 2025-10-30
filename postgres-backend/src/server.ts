/**
 * Fastify REST API Server
 *
 * Exposes PostgreSQL Actor System via REST API
 */

import Fastify, { FastifyRequest, FastifyReply } from "fastify";
import cors from "@fastify/cors";
import { Pool } from "pg";
import { Application } from "./actors/ActorSystem.js";
import {
  createDocumentManagerBehavior,
  createSimulationBehavior,
} from "./actors/DocumentManagerActor.js";

export async function createServer(pool: Pool) {
  const fastify = Fastify({
    logger: {
      level: "info",
      transport: {
        target: "pino-pretty",
        options: {
          translateTime: "HH:MM:ss Z",
          ignore: "pid,hostname",
        },
      },
    },
  });

  // Enable CORS
  await fastify.register(cors, {
    origin: "*",
  });

  // Create Actor Application
  const app = new Application(pool);

  // Spawn Document Manager Actor
  const documentManager = app.spawnSupervised(
    createDocumentManagerBehavior(pool),
    { pool, cache: new Map(), subscribers: new Set() },
    "permanent",
  );
  app.getRegistry().register("document_manager", documentManager.pid);

  // Listen to PostgreSQL NOTIFY channels
  await documentManager.listen("status_change");
  await documentManager.listen("new_document");

  // Spawn Simulation Actor
  const simulationActor = app.spawnSupervised(
    createSimulationBehavior(pool, documentManager.pid),
    { running: false, interval: null },
    "permanent",
  );
  app.getRegistry().register("simulation", simulationActor.pid);

  // Store app in fastify for access in routes
  fastify.decorate("actorApp", app);
  fastify.decorate("documentManager", documentManager);
  fastify.decorate("simulationActor", simulationActor);

  // Health check
  fastify.get("/api/health", async () => {
    return {
      status: "healthy",
      timestamp: new Date().toISOString(),
      actors: app.getRegistry().registered(),
      database: "connected",
    };
  });

  // Get all documents
  fastify.get(
    "/api/documents",
    async (request: FastifyRequest, reply: FastifyReply) => {
      const result = await pool.query(`
      SELECT * FROM woo_requests_view
      ORDER BY updated_at DESC
    `);

      return { documents: result.rows };
    },
  );

  // Get document by ID
  fastify.get<{ Params: { id: string } }>(
    "/api/documents/:id",
    async (
      request: FastifyRequest<{ Params: { id: string } }>,
      reply: FastifyReply,
    ) => {
      const { id } = request.params;

      const result = await pool.query(
        `
      SELECT * FROM woo_requests_view
      WHERE document_id = $1
    `,
        [id],
      );

      if (result.rows.length === 0) {
        return reply.status(404).send({ error: "Document not found" });
      }

      return result.rows[0];
    },
  );

  // Update document status
  fastify.post<{
    Params: { id: string };
    Body: { status: string };
  }>(
    "/api/documents/:id/status",
    async (
      request: FastifyRequest<{
        Params: { id: string };
        Body: { status: string };
      }>,
      reply: FastifyReply,
    ) => {
      const { id } = request.params;
      const { status } = request.body;

      await documentManager.send({
        type: "update_status",
        data: { document_id: id, new_status: status },
      });

      // Wait a bit for processing
      await new Promise((resolve) => setTimeout(resolve, 50));

      const result = await pool.query(
        `
      SELECT * FROM woo_requests_view
      WHERE document_id = $1
    `,
        [id],
      );

      if (result.rows.length === 0) {
        return reply.status(404).send({ error: "Document not found" });
      }

      return result.rows[0];
    },
  );

  // Get statistics
  fastify.get("/api/statistics", async () => {
    const result = await pool.query(`
      SELECT * FROM woo_statistics
    `);

    if (result.rows.length === 0) {
      return {
        total_requests: 0,
        received: 0,
        in_progress: 0,
        completed: 0,
        avg_handling_days: 0,
        by_organization: {},
      };
    }

    return result.rows[0];
  });

  // Get organization statistics
  fastify.get<{ Params: { org: string } }>(
    "/api/statistics/:org",
    async (
      request: FastifyRequest<{ Params: { org: string } }>,
      reply: FastifyReply,
    ) => {
      const org =
        request.params.org === "utrecht"
          ? "Gemeente Utrecht"
          : request.params.org === "flevoland"
            ? "Provincie Flevoland"
            : request.params.org;

      const result = await pool.query(
        `
      SELECT
        o.name as organization,
        COUNT(*) as count,
        json_agg(w.*) as documents
      FROM woo_requests_view w
      JOIN organizations o ON w.organization = o.name
      WHERE o.name = $1
      GROUP BY o.name
    `,
        [org],
      );

      if (result.rows.length === 0) {
        return { organization: org, count: 0, documents: [] };
      }

      return result.rows[0];
    },
  );

  // Search documents (full-text search)
  fastify.get<{ Querystring: { q: string } }>(
    "/api/search",
    async (
      request: FastifyRequest<{ Querystring: { q: string } }>,
      reply: FastifyReply,
    ) => {
      const { q } = request.query;

      if (!q) {
        return reply
          .status(400)
          .send({ error: 'Query parameter "q" is required' });
      }

      const result = await pool.query(
        `
      SELECT * FROM search_documents($1)
    `,
        [q],
      );

      return { query: q, results: result.rows };
    },
  );

  // Start simulation
  fastify.post("/api/simulation/start", async () => {
    const state = simulationActor.getState();

    if (state.running) {
      return { status: "already_running" };
    }

    // Start interval
    const interval = setInterval(async () => {
      await simulationActor.send({ type: "tick" });
    }, 2000);

    simulationActor.setState({ running: true, interval });

    return { status: "started", message: "Simulation started" };
  });

  // Stop simulation
  fastify.post("/api/simulation/stop", async () => {
    const state = simulationActor.getState();

    if (!state.running) {
      return { status: "not_running" };
    }

    clearInterval(state.interval);
    simulationActor.setState({ running: false, interval: null });

    return { status: "stopped", message: "Simulation stopped" };
  });

  // Get status history
  fastify.get<{ Params: { id: string } }>(
    "/api/documents/:id/history",
    async (
      request: FastifyRequest<{ Params: { id: string } }>,
      reply: FastifyReply,
    ) => {
      const { id } = request.params;

      const result = await pool.query(
        `
      SELECT
        sh.*,
        w.title,
        w.organization_id
      FROM status_history sh
      JOIN woo_requests w ON sh.request_id = w.id
      WHERE w.document_id = $1
      ORDER BY sh.changed_at DESC
    `,
        [id],
      );

      return { document_id: id, history: result.rows };
    },
  );

  // Refresh statistics materialized view
  fastify.post("/api/statistics/refresh", async () => {
    await pool.query(`SELECT refresh_statistics()`);
    return { status: "refreshed" };
  });

  // Get all actors (debugging)
  fastify.get("/api/actors", async () => {
    return {
      registered: app.getRegistry().registered(),
      supervisor_children: app.getSupervisor().getChildren(),
    };
  });

  // Graceful shutdown
  fastify.addHook("onClose", async () => {
    console.log("Shutting down actor system...");
    await app.shutdown();
  });

  return fastify;
}
