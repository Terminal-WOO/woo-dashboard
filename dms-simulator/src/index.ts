import Fastify from "fastify";
import cors from "@fastify/cors";
import dotenv from "dotenv";
import { generateRandomMetadata, generatePDF } from "./document-generator.js";
import { PaperlessClient } from "./paperless-client.js";
import { AlfrescoClient } from "./alfresco-client.js";
import { NATSEventClient, DocumentEvent } from "./nats-client.js";

dotenv.config();

const fastify = Fastify({
  logger: {
    transport: {
      target: "pino-pretty",
      options: {
        colorize: true,
        translateTime: "SYS:standard",
      },
    },
  },
});

await fastify.register(cors, {
  origin: true,
});

// Initialize NATS Event Client
let natsClient: NATSEventClient | null = null;
if (process.env.NATS_ENABLED === "true") {
  natsClient = new NATSEventClient(
    process.env.NATS_URL || "nats://localhost:4222",
  );
  try {
    await natsClient.connect();
    fastify.log.info("Connected to NATS JetStream");
  } catch (error) {
    fastify.log.error("Failed to connect to NATS:", error);
    natsClient = null;
  }
}

// Store active simulations
const activeSimulations = new Map<
  string,
  {
    total: number;
    completed: number;
    failed: number;
    status: "running" | "completed" | "failed";
    startTime: Date;
    documents: Array<{
      id: string;
      title: string;
      system: string;
      status: string;
    }>;
  }
>();

// Health check
fastify.get("/health", async () => {
  return { status: "ok", timestamp: new Date().toISOString() };
});

// Test connections
fastify.get("/test-connections", async (request, reply) => {
  const results = {
    paperless: false,
    alfresco: false,
  };

  if (process.env.PAPERLESS_ENABLED === "true") {
    const paperless = new PaperlessClient(
      process.env.PAPERLESS_URL || "http://localhost:8000",
      process.env.PAPERLESS_API_KEY || "",
    );
    results.paperless = await paperless.testConnection();
  }

  if (process.env.ALFRESCO_ENABLED === "true") {
    const alfresco = new AlfrescoClient(
      process.env.ALFRESCO_URL || "http://localhost:8080",
      process.env.ALFRESCO_USERNAME || "admin",
      process.env.ALFRESCO_PASSWORD || "admin",
    );
    results.alfresco = await alfresco.testConnection();
  }

  return results;
});

// Start simulation
fastify.post<{
  Body: {
    count: number;
    systems: Array<"paperless" | "alfresco">;
  };
}>("/simulate", async (request, reply) => {
  const { count, systems } = request.body;

  if (!count || count < 1 || count > 100) {
    return reply.code(400).send({ error: "Count must be between 1 and 100" });
  }

  if (!systems || systems.length === 0) {
    return reply
      .code(400)
      .send({ error: "At least one system must be selected" });
  }

  const simulationId = `sim-${Date.now()}`;

  activeSimulations.set(simulationId, {
    total: count * systems.length,
    completed: 0,
    failed: 0,
    status: "running",
    startTime: new Date(),
    documents: [],
  });

  // Run simulation in background
  runSimulation(simulationId, count, systems);

  return { simulationId, status: "started" };
});

// Get simulation status
fastify.get<{
  Params: { id: string };
}>("/simulate/:id", async (request, reply) => {
  const simulation = activeSimulations.get(request.params.id);

  if (!simulation) {
    return reply.code(404).send({ error: "Simulation not found" });
  }

  return {
    ...simulation,
    progress: Math.round((simulation.completed / simulation.total) * 100),
  };
});

// List all simulations
fastify.get("/simulations", async () => {
  return Array.from(activeSimulations.entries()).map(([id, sim]) => ({
    id,
    ...sim,
    progress: Math.round((sim.completed / sim.total) * 100),
  }));
});

async function runSimulation(
  simulationId: string,
  count: number,
  systems: Array<"paperless" | "alfresco">,
) {
  const simulation = activeSimulations.get(simulationId);
  if (!simulation) return;

  const paperlessClient =
    systems.includes("paperless") && process.env.PAPERLESS_ENABLED === "true"
      ? new PaperlessClient(
          process.env.PAPERLESS_URL || "http://localhost:8000",
          process.env.PAPERLESS_API_KEY || "",
        )
      : null;

  const alfrescoClient =
    systems.includes("alfresco") && process.env.ALFRESCO_ENABLED === "true"
      ? new AlfrescoClient(
          process.env.ALFRESCO_URL || "http://localhost:8080",
          process.env.ALFRESCO_USERNAME || "admin",
          process.env.ALFRESCO_PASSWORD || "admin",
        )
      : null;

  for (let i = 0; i < count; i++) {
    try {
      // Generate document
      const metadata = generateRandomMetadata();
      const pdfBuffer = await generatePDF(metadata);

      // Upload to Paperless
      if (paperlessClient) {
        try {
          const uploadStart = Date.now();
          const result = await paperlessClient.uploadDocument(
            pdfBuffer,
            metadata,
          );
          const uploadDuration = Date.now() - uploadStart;

          simulation.documents.push({
            id: String(result.id),
            title: metadata.title,
            system: "paperless",
            status: "success",
          });
          simulation.completed++;
          fastify.log.info(`Uploaded to Paperless: ${metadata.title}`);

          // Publish NATS event
          if (natsClient) {
            const event: DocumentEvent = {
              eventId: `evt-${Date.now()}-${Math.random().toString(36).substr(2, 9)}`,
              eventType: "document.uploaded",
              timestamp: new Date().toISOString(),
              system: "paperless",
              document: {
                id: String(result.id),
                title: metadata.title,
                type: metadata.type,
                category: metadata.category,
                author: metadata.author,
                date: metadata.date.toISOString(),
                tags: metadata.tags,
                size: pdfBuffer.length,
                contentType: "application/pdf",
              },
              metadata: {
                simulationId,
                uploadDuration,
                source: "dms-simulator",
              },
            };
            await natsClient.publishDocumentEvent(event);
          }
        } catch (error) {
          fastify.log.error(`Failed to upload to Paperless: ${error}`);
          simulation.documents.push({
            id: `error-${Date.now()}`,
            title: metadata.title,
            system: "paperless",
            status: "failed",
          });
          simulation.failed++;
        }
      }

      // Upload to Alfresco
      if (alfrescoClient) {
        try {
          const uploadStart = Date.now();
          const result = await alfrescoClient.uploadDocument(
            pdfBuffer,
            metadata,
          );
          const uploadDuration = Date.now() - uploadStart;

          simulation.documents.push({
            id: result.id,
            title: metadata.title,
            system: "alfresco",
            status: "success",
          });
          simulation.completed++;
          fastify.log.info(`Uploaded to Alfresco: ${metadata.title}`);

          // Publish NATS event
          if (natsClient) {
            const event: DocumentEvent = {
              eventId: `evt-${Date.now()}-${Math.random().toString(36).substr(2, 9)}`,
              eventType: "document.uploaded",
              timestamp: new Date().toISOString(),
              system: "alfresco",
              document: {
                id: result.id,
                title: metadata.title,
                type: metadata.type,
                category: metadata.category,
                author: metadata.author,
                date: metadata.date.toISOString(),
                tags: metadata.tags,
                size: pdfBuffer.length,
                contentType: "application/pdf",
              },
              metadata: {
                simulationId,
                uploadDuration,
                source: "dms-simulator",
              },
            };
            await natsClient.publishDocumentEvent(event);
          }
        } catch (error) {
          fastify.log.error(`Failed to upload to Alfresco: ${error}`);
          simulation.documents.push({
            id: `error-${Date.now()}`,
            title: metadata.title,
            system: "alfresco",
            status: "failed",
          });
          simulation.failed++;
        }
      }

      // Small delay to avoid overwhelming the systems
      await new Promise((resolve) => setTimeout(resolve, 500));
    } catch (error) {
      fastify.log.error(`Document generation error: ${error}`);
      simulation.failed++;
    }
  }

  simulation.status = simulation.failed > 0 ? "completed" : "completed";
  fastify.log.info(
    `Simulation ${simulationId} completed. Success: ${simulation.completed}, Failed: ${simulation.failed}`,
  );
}

// Start server
const start = async () => {
  try {
    const port = parseInt(process.env.PORT || "3001", 10);
    const host = process.env.HOST || "0.0.0.0";

    await fastify.listen({ port, host });

    fastify.log.info(`DMS Simulator running on http://${host}:${port}`);
    fastify.log.info(`Paperless enabled: ${process.env.PAPERLESS_ENABLED}`);
    fastify.log.info(`Alfresco enabled: ${process.env.ALFRESCO_ENABLED}`);
  } catch (err) {
    fastify.log.error(err);
    process.exit(1);
  }
};

start();
