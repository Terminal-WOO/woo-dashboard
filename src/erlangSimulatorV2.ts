/**
 * Erlang-style WOO Document Simulator V2
 * Integrated with SQLite database for Utrecht and Flevoland
 */

import { WOORequest, Organization } from "./types";
import { erlangSystem, Message } from "./erlangActorSystem";
import { mockDatabaseService as databaseService } from "./mockDatabase";

// Organization-specific status workflows
// Workflow: 9 distinct statuses showing full document lifecycle
const ORGANIZATIONS: Organization[] = [
  {
    id: "gemeente-utrecht",
    name: "Gemeente Utrecht",
    type: "gemeente",
    statusWorkflow: [
      "Ontvangen", // 1. Document komt binnen
      "In behandeling", // 2. Eerste beoordeling
      "1e Concept", // 3. Eerste concept klaar
      "2e Concept", // 4. Verbeterde versie
      "Definitief", // 5. Definitieve versie
      "Gepubliceerd", // 6. Gepubliceerd! Dan weer terug naar Ontvangen
    ],
  },
  {
    id: "provincie-flevoland",
    name: "Provincie Flevoland",
    type: "provincie",
    statusWorkflow: [
      "Ontvangen", // 1. Document komt binnen
      "In behandeling", // 2. Eerste beoordeling
      "1e Concept", // 3. Eerste concept klaar
      "2e Concept", // 4. Verbeterde versie
      "Definitief", // 5. Definitieve versie
      "Gepubliceerd", // 6. Gepubliceerd! Dan weer terug naar Ontvangen
    ],
  },
];

// MAX_DOCUMENTS reserved for future pagination feature

/**
 * Database Actor Behavior - Handles all database operations
 */
function createDatabaseActorBehavior() {
  return async (message: Message, state: any) => {
    if (message.type === "restart") {
      // Initialize database
      await databaseService.init();
      return { ...state, initialized: true };
    }

    return state;
  };
}

/**
 * Ticker Actor - Generates periodic tick messages
 */
function createTickerBehavior(intervalMs: number, targetPid: string) {
  return (message: Message, state: any) => {
    if (message.type === "restart") {
      if (state.intervalId) {
        clearInterval(state.intervalId);
      }

      const intervalId = setInterval(() => {
        const target = erlangSystem.getRegistry().whereis(targetPid);
        if (target) {
          target.send({
            type: "tick",
            data: { timestamp: Date.now() },
          });
        }
      }, intervalMs);

      return { ...state, intervalId };
    }

    if (message.type === "shutdown") {
      if (state.intervalId) {
        clearInterval(state.intervalId);
      }
      return { ...state, intervalId: null };
    }

    return state;
  };
}

/**
 * Document Manager Actor V2 - Manages documents with database persistence
 */
function createDocumentManagerV2Behavior() {
  return (message: Message, state: any) => {
    if (message.type === "tick") {
      // Load all documents from database
      const allRequests = databaseService.queryAll();

      // Process a random document update
      const updatedRequests = updateRandomDocumentV2(allRequests);

      // Notify callback
      if (state.callback && updatedRequests) {
        state.callback(updatedRequests);
      }

      return state;
    }

    if (message.type === "restart") {
      return {
        callback: state.callback,
      };
    }

    return state;
  };
}

/**
 * Update random document with organization-aware status progression
 */
function updateRandomDocumentV2(requests: WOORequest[]): WOORequest[] | null {
  // Find documents not yet published
  let updatableRequests = requests.filter((r) => r.status !== "Gepubliceerd");

  if (updatableRequests.length === 0) {
    // All documents published - reset a random one to restart cycle
    console.log("[Simulator V2] All documents published, restarting cycle...");
    const randomDoc = requests[Math.floor(Math.random() * requests.length)];
    databaseService.update(randomDoc.id, "Ontvangen");

    // Publish reset event
    erlangSystem.getEventManager().notify({
      type: "status_change",
      data: {
        documentId: randomDoc.id,
        title: randomDoc.title,
        oldStatus: "Gepubliceerd",
        newStatus: "Ontvangen",
        organization: randomDoc.organization,
        timestamp: Date.now(),
      },
    });

    return databaseService.queryAll();
  }

  // Pick random document
  const randomIndex = Math.floor(Math.random() * updatableRequests.length);
  const requestToUpdate = updatableRequests[randomIndex];

  // Find organization
  const org = ORGANIZATIONS.find(
    (o) => o.name === requestToUpdate.organization,
  );
  if (!org) {
    console.error(
      `[Simulator V2] Organization not found: ${requestToUpdate.organization}`,
    );
    return null;
  }

  // Get current position in workflow
  const currentIndex = org.statusWorkflow.indexOf(requestToUpdate.status);
  if (currentIndex === -1 || currentIndex >= org.statusWorkflow.length - 1) {
    return null;
  }

  // Get next status
  const nextStatus = org.statusWorkflow[currentIndex + 1];

  // IMPORTANT: Save old status BEFORE updating the database
  const oldStatus = requestToUpdate.status;

  console.log(
    `[Simulator V2] ${requestToUpdate.organization} - ${requestToUpdate.id}: ${oldStatus} → ${nextStatus}`,
  );

  // Update in database
  databaseService.update(requestToUpdate.id, nextStatus);

  // Publish event with the SAVED old status
  erlangSystem.getEventManager().notify({
    type: "status_change",
    data: {
      documentId: requestToUpdate.id,
      title: requestToUpdate.title,
      oldStatus: oldStatus,
      newStatus: nextStatus,
      organization: requestToUpdate.organization,
      timestamp: Date.now(),
    },
  });

  // Return updated list from database
  return databaseService.queryAll();
}

/**
 * Event Publisher Actor - Publishes events to subscribers
 */
function createEventPublisherBehavior() {
  return (message: Message, state: any) => {
    const subscribers: Function[] = state.subscribers || [];

    if (message.type === "subscribe") {
      return {
        ...state,
        subscribers: [...subscribers, message.data.callback],
      };
    }

    if (message.type === "unsubscribe") {
      return {
        ...state,
        subscribers: subscribers.filter(
          (s: Function) => s !== message.data.callback,
        ),
      };
    }

    if (message.type === "status_change" || message.type === "new_document") {
      subscribers.forEach((callback: Function) => {
        callback(message);
      });
    }

    return state;
  };
}

/**
 * Erlang Simulator V2 with Database Integration
 */
export class ErlangSimulatorV2 {
  private isRunning = false;
  private isInitialized = false;
  private tickerPid: string | null = null;
  private managerPid: string = "document_manager_v2";
  private publisherPid: string = "event_publisher_v2";
  private databasePid: string = "database_actor";

  async initialize(): Promise<void> {
    if (this.isInitialized) return;

    console.log("[Simulator V2] Initializing actors and database...");

    // Spawn database actor
    const dbActor = erlangSystem.spawnRegister(
      this.databasePid,
      createDatabaseActorBehavior(),
      { initialized: false },
    );

    // Initialize database
    dbActor.send({ type: "restart" });
    await databaseService.init();

    // Spawn document manager
    erlangSystem.spawnRegister(
      this.managerPid,
      createDocumentManagerV2Behavior(),
      { callback: null },
    );

    // Spawn event publisher
    erlangSystem.spawnRegister(
      this.publisherPid,
      createEventPublisherBehavior(),
      { subscribers: [] },
    );

    this.isInitialized = true;

    // Log statistics
    const stats = databaseService.getStatistics();
    console.log("[Simulator V2] Database loaded:", stats);
  }

  async start(
    onUpdate: (requests: WOORequest[]) => void,
    intervalMs: number = 3000,
  ): Promise<void> {
    if (this.isRunning) return;

    if (!this.isInitialized) {
      await this.initialize();
    }

    // Set callback on manager
    const manager = erlangSystem.getRegistry().whereis(this.managerPid);
    if (manager) {
      manager.setState({ callback: onUpdate });
    }

    // Load initial data
    const initialData = databaseService.queryAll();
    onUpdate(initialData);

    // Spawn ticker
    const ticker = erlangSystem.spawn(
      createTickerBehavior(intervalMs, this.managerPid),
      {},
    );

    erlangSystem.getRegistry().register("ticker_v2", ticker);
    this.tickerPid = ticker.pid;

    ticker.send({ type: "restart" });

    this.isRunning = true;

    console.log("[Simulator V2] Started with actors:", {
      ticker: this.tickerPid,
      manager: this.managerPid,
      publisher: this.publisherPid,
      database: this.databasePid,
    });
  }

  stop(): void {
    if (!this.isRunning) return;

    const ticker = erlangSystem.getRegistry().whereis("ticker_v2");
    if (ticker) {
      ticker.send({ type: "shutdown" });
      erlangSystem.getRegistry().unregister("ticker_v2");
    }

    this.isRunning = false;
    this.tickerPid = null;

    console.log("[Simulator V2] Stopped");
  }

  subscribe(callback: (message: Message) => void): void {
    const publisher = erlangSystem.getRegistry().whereis(this.publisherPid);
    if (publisher) {
      publisher.send({
        type: "subscribe",
        data: { callback },
      });
    }
  }

  unsubscribe(callback: (message: Message) => void): void {
    const publisher = erlangSystem.getRegistry().whereis(this.publisherPid);
    if (publisher) {
      publisher.send({
        type: "unsubscribe",
        data: { callback },
      });
    }
  }

  getStatistics() {
    return databaseService.getStatistics();
  }

  getProcesses(): string[] {
    return erlangSystem.getRegistry().registered();
  }

  getIsRunning(): boolean {
    return this.isRunning;
  }

  getOrganizations(): Organization[] {
    return ORGANIZATIONS;
  }
}

// Singleton instance
export const erlangSimulatorV2 = new ErlangSimulatorV2();
