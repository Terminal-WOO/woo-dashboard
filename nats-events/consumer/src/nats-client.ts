import {
  connect,
  NatsConnection,
  JetStreamClient,
  JetStreamManager,
  StreamConfig,
  ConsumerConfig,
  JsMsg,
  RetentionPolicy,
  StorageType,
  AckPolicy,
  DeliverPolicy,
} from "nats";

export interface DocumentEvent {
  eventId: string;
  eventType: "document.uploaded" | "document.updated" | "document.deleted";
  timestamp: string;
  system: "paperless" | "alfresco";
  document: {
    id: string;
    title: string;
    type: string;
    category: string;
    author: string;
    date: string;
    tags: string[];
    size: number;
    contentType: string;
  };
  metadata: {
    simulationId?: string;
    uploadDuration?: number;
    source: string;
  };
}

export class NATSEventClient {
  private nc?: NatsConnection;
  private js?: JetStreamClient;
  private jsm?: JetStreamManager;
  private events: DocumentEvent[] = [];
  private maxEvents = 1000;

  constructor(private natsUrl: string = "nats://localhost:4222") {}

  async connect(): Promise<void> {
    this.nc = await connect({ servers: this.natsUrl });
    this.js = this.nc.jetstream();
    this.jsm = await this.nc.jetstreamManager();

    console.log("Connected to NATS");
    await this.setupStream();
  }

  private async setupStream(): Promise<void> {
    const streamConfig: Partial<StreamConfig> = {
      name: "DOCUMENT_EVENTS",
      subjects: ["document.*"],
      retention: RetentionPolicy.Limits,
      max_msgs: 100000,
      max_bytes: 1024 * 1024 * 1024, // 1GB
      max_age: 7 * 24 * 60 * 60 * 1_000_000_000, // 7 days in nanoseconds
      storage: StorageType.File,
      num_replicas: 1,
    };

    try {
      await this.jsm!.streams.add(streamConfig);
      console.log("Stream DOCUMENT_EVENTS created");
    } catch (error: any) {
      if (error.message?.includes("already in use")) {
        console.log("Stream DOCUMENT_EVENTS already exists");
      } else {
        throw error;
      }
    }
  }

  async publishDocumentEvent(event: DocumentEvent): Promise<void> {
    if (!this.js) {
      throw new Error("Not connected to NATS");
    }

    const subject = `document.${event.eventType.split(".")[1]}`;
    const data = new TextEncoder().encode(JSON.stringify(event));

    await this.js.publish(subject, data);
    console.log(`Published event: ${event.eventId} to ${subject}`);
  }

  async subscribe(callback: (event: DocumentEvent) => void): Promise<void> {
    if (!this.js) {
      throw new Error("Not connected to NATS");
    }

    // Create consumer
    const consumerConfig: Partial<ConsumerConfig> = {
      durable_name: "document-event-consumer",
      ack_policy: AckPolicy.Explicit,
      deliver_policy: DeliverPolicy.All,
      filter_subject: "document.*",
    };

    try {
      await this.jsm!.consumers.add("DOCUMENT_EVENTS", consumerConfig);
    } catch (error: any) {
      if (!error.message?.includes("already in use")) {
        throw error;
      }
    }

    // Subscribe to messages
    const consumer = await this.js.consumers.get(
      "DOCUMENT_EVENTS",
      "document-event-consumer",
    );
    const messages = await consumer.consume();

    (async () => {
      for await (const msg of messages) {
        try {
          const event = JSON.parse(
            new TextDecoder().decode(msg.data),
          ) as DocumentEvent;

          // Store in memory
          this.events.unshift(event);
          if (this.events.length > this.maxEvents) {
            this.events.pop();
          }

          callback(event);
          msg.ack();
        } catch (error) {
          console.error("Error processing message:", error);
          msg.nak();
        }
      }
    })();

    console.log("Subscribed to document events");
  }

  getEvents(limit: number = 100): DocumentEvent[] {
    return this.events.slice(0, limit);
  }

  getEventsBySystem(
    system: "paperless" | "alfresco",
    limit: number = 100,
  ): DocumentEvent[] {
    return this.events.filter((e) => e.system === system).slice(0, limit);
  }

  getEventsByType(
    eventType: DocumentEvent["eventType"],
    limit: number = 100,
  ): DocumentEvent[] {
    return this.events.filter((e) => e.eventType === eventType).slice(0, limit);
  }

  getStats() {
    const now = Date.now();
    const last24h = this.events.filter(
      (e) => now - new Date(e.timestamp).getTime() < 24 * 60 * 60 * 1000,
    );

    return {
      total: this.events.length,
      last24h: last24h.length,
      bySystem: {
        paperless: this.events.filter((e) => e.system === "paperless").length,
        alfresco: this.events.filter((e) => e.system === "alfresco").length,
      },
      byType: {
        uploaded: this.events.filter((e) => e.eventType === "document.uploaded")
          .length,
        updated: this.events.filter((e) => e.eventType === "document.updated")
          .length,
        deleted: this.events.filter((e) => e.eventType === "document.deleted")
          .length,
      },
    };
  }

  async close(): Promise<void> {
    await this.nc?.close();
    console.log("Disconnected from NATS");
  }
}
