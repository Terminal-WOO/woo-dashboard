/**
 * Erlang-style WOO Document Simulator
 * Uses actor model for event-driven simulation
 */

import { WOORequest, WOOStatus } from "./types";
import { erlangSystem, Message } from "./erlangActorSystem";
import { generateNewDocument } from "./documentGenerator";

const statusProgression: WOOStatus[] = [
  "Ontvangen",
  "In behandeling",
  "Afgerond",
];

const MAX_DOCUMENTS = 12;

/**
 * Ticker Actor - Generates periodic tick messages (like Erlang's timer:send_interval)
 */
function createTickerBehavior(intervalMs: number, targetPid: string) {
  return (message: Message, state: any) => {
    if (message.type === "restart") {
      // Clear old interval if exists
      if (state.intervalId) {
        clearInterval(state.intervalId);
      }

      // Start new interval
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
 * Document Manager Actor - Manages WOO document state
 * Like a gen_server with handle_cast for state updates
 */
function createDocumentManagerBehavior() {
  return (message: Message, state: any) => {
    const requests: WOORequest[] = state.requests || [];

    if (message.type === "tick") {
      // Process a random document update
      const updatedRequests = updateRandomDocument(requests);

      // Notify callback if exists
      if (state.callback && updatedRequests) {
        state.callback(updatedRequests);
      }

      return { ...state, requests: updatedRequests || requests };
    }

    if (message.type === "restart") {
      // Initialize with fresh state
      return {
        requests: state.requests || [],
        callback: state.callback,
      };
    }

    return state;
  };
}

/**
 * Event Publisher Actor - Publishes events to subscribers
 * Like gen_event behavior in Erlang/OTP
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
      // Broadcast to all subscribers
      subscribers.forEach((callback: Function) => {
        callback(message);
      });
    }

    return state;
  };
}

/**
 * Update a random document (pure function for immutability)
 */
function updateRandomDocument(requests: WOORequest[]): WOORequest[] | null {
  const updatableRequests = requests.filter((r) => r.status !== "Afgerond");

  // If no updatable requests, add a new document
  if (updatableRequests.length === 0) {
    const newDocument = generateNewDocument();

    // Publish new document event
    erlangSystem.getEventManager().notify({
      type: "new_document",
      data: {
        id: newDocument.id,
        title: newDocument.title,
        organization: newDocument.organization,
        status: newDocument.status,
        timestamp: Date.now(),
      },
    });

    const updatedList = [...requests, newDocument];
    return updatedList.slice(-MAX_DOCUMENTS);
  }

  // Pick random document to update
  const randomIndex = Math.floor(Math.random() * updatableRequests.length);
  const requestToUpdate = updatableRequests[randomIndex];

  const currentStatusIndex = statusProgression.indexOf(requestToUpdate.status);
  const nextStatus = statusProgression[currentStatusIndex + 1];

  if (!nextStatus) return null;

  // Publish status change event
  erlangSystem.getEventManager().notify({
    type: "status_change",
    data: {
      documentId: requestToUpdate.id,
      title: requestToUpdate.title,
      oldStatus: requestToUpdate.status,
      newStatus: nextStatus,
      organization: requestToUpdate.organization,
      timestamp: Date.now(),
    },
  });

  let updatedRequests: WOORequest[];

  // If document is completed, replace it with a new one
  if (nextStatus === "Afgerond") {
    updatedRequests = requests.filter((req) => req.id !== requestToUpdate.id);

    const newDocument = generateNewDocument();

    erlangSystem.getEventManager().notify({
      type: "new_document",
      data: {
        id: newDocument.id,
        title: newDocument.title,
        organization: newDocument.organization,
        status: newDocument.status,
        timestamp: Date.now(),
      },
    });

    updatedRequests = [...updatedRequests, newDocument];
  } else {
    // Update status immutably
    updatedRequests = requests.map((req) =>
      req.id === requestToUpdate.id ? { ...req, status: nextStatus } : req,
    );
  }

  return updatedRequests;
}

/**
 * Erlang-style Simulator using Actor Model
 */
export class ErlangSimulator {
  private isRunning = false;
  private tickerPid: string | null = null;
  private managerPid: string = "document_manager";
  private publisherPid: string = "event_publisher";

  constructor() {
    // Spawn permanent actors (supervised)
    erlangSystem.spawnRegister(
      this.managerPid,
      createDocumentManagerBehavior(),
      { requests: [], callback: null },
    );

    erlangSystem.spawnRegister(
      this.publisherPid,
      createEventPublisherBehavior(),
      { subscribers: [] },
    );
  }

  /**
   * Start simulation (spawn ticker process)
   */
  start(
    requests: WOORequest[],
    onUpdate: (requests: WOORequest[]) => void,
    intervalMs: number = 5000,
  ): void {
    if (this.isRunning) return;

    // Update document manager state
    const manager = erlangSystem.getRegistry().whereis(this.managerPid);
    if (manager) {
      manager.setState({ requests, callback: onUpdate });
    }

    // Spawn ticker actor
    const ticker = erlangSystem.spawn(
      createTickerBehavior(intervalMs, this.managerPid),
      {},
    );

    // Register ticker
    erlangSystem.getRegistry().register("ticker", ticker);
    this.tickerPid = ticker.pid;

    // Start ticker
    ticker.send({ type: "restart" });

    this.isRunning = true;

    console.log("[Erlang System] Simulation started with actors:", {
      ticker: this.tickerPid,
      manager: this.managerPid,
      publisher: this.publisherPid,
    });
  }

  /**
   * Stop simulation (shutdown ticker)
   */
  stop(): void {
    if (!this.isRunning) return;

    const ticker = erlangSystem.getRegistry().whereis("ticker");
    if (ticker) {
      ticker.send({ type: "shutdown" });
      erlangSystem.getRegistry().unregister("ticker");
    }

    this.isRunning = false;
    this.tickerPid = null;

    console.log("[Erlang System] Simulation stopped");
  }

  /**
   * Subscribe to events (add event handler)
   */
  subscribe(callback: (message: Message) => void): void {
    const publisher = erlangSystem.getRegistry().whereis(this.publisherPid);
    if (publisher) {
      publisher.send({
        type: "subscribe",
        data: { callback },
      });
    }
  }

  /**
   * Unsubscribe from events
   */
  unsubscribe(callback: (message: Message) => void): void {
    const publisher = erlangSystem.getRegistry().whereis(this.publisherPid);
    if (publisher) {
      publisher.send({
        type: "unsubscribe",
        data: { callback },
      });
    }
  }

  /**
   * Get registered processes (like registered() in Erlang)
   */
  getProcesses(): string[] {
    return erlangSystem.getRegistry().registered();
  }

  getIsRunning(): boolean {
    return this.isRunning;
  }
}

// Singleton instance
export const erlangSimulator = new ErlangSimulator();
