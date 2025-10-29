/**
 * Erlang-inspired Actor System for WOO Dashboard
 *
 * Implements core Erlang/OTP concepts:
 * - Actor Model: Each actor has a mailbox and processes messages sequentially
 * - Message Passing: Actors communicate only through immutable messages
 * - Process Isolation: Each actor maintains its own state
 * - Supervision: Supervisor can monitor and restart actors
 * - Let it crash: Actors can fail safely and be restarted
 */

// Message types (like Erlang tuples/atoms)
export type Message =
  | { type: "status_change"; data: StatusChangeData }
  | { type: "new_document"; data: DocumentData }
  | { type: "tick"; data: { timestamp: number } }
  | { type: "subscribe"; data: { callback: MessageHandler } }
  | { type: "unsubscribe"; data: { callback: MessageHandler } }
  | { type: "shutdown" }
  | { type: "restart" };

export type StatusChangeData = {
  documentId: string;
  title: string;
  oldStatus: string | null;
  newStatus: string;
  organization: string;
  timestamp: number;
};

export type DocumentData = {
  id: string;
  title: string;
  organization: string;
  status: string;
  timestamp: number;
};

type MessageHandler = (message: Message) => void;
type ActorRef = string;

/**
 * Actor - Erlang process equivalent
 * Each actor has a unique PID, mailbox, and processes messages sequentially
 */
export class Actor {
  private mailbox: Message[] = [];
  private isProcessing = false;
  private state: any = {};
  private messageHandler: ((message: Message, state: any) => any) | null = null;

  constructor(
    public readonly pid: ActorRef,
    private supervisor?: Supervisor,
  ) {}

  /**
   * Set the behavior function (like gen_server handle_call/handle_cast)
   */
  setBehavior(handler: (message: Message, state: any) => any): void {
    this.messageHandler = handler;
  }

  /**
   * Send message to actor (! operator in Erlang)
   */
  send(message: Message): void {
    this.mailbox.push(message);
    this.processMailbox();
  }

  /**
   * Process messages sequentially from mailbox
   * Similar to Erlang's receive loop
   */
  private async processMailbox(): Promise<void> {
    if (this.isProcessing || this.mailbox.length === 0) return;

    this.isProcessing = true;

    while (this.mailbox.length > 0) {
      const message = this.mailbox.shift()!;

      try {
        if (this.messageHandler) {
          // Process message and update state
          this.state = await this.messageHandler(message, this.state);
        }

        // Handle shutdown message
        if (message.type === "shutdown") {
          this.shutdown();
          break;
        }
      } catch (error) {
        // Let it crash philosophy - report to supervisor
        console.error(`Actor ${this.pid} crashed:`, error);
        if (this.supervisor) {
          this.supervisor.handleActorCrash(this);
        }
      }
    }

    this.isProcessing = false;
  }

  getState(): any {
    return this.state;
  }

  setState(newState: any): void {
    this.state = newState;
  }

  shutdown(): void {
    this.mailbox = [];
    this.isProcessing = false;
    this.messageHandler = null;
  }
}

/**
 * Supervisor - OTP Supervisor pattern
 * Monitors child actors and restarts them on failure
 */
export class Supervisor {
  private children: Map<ActorRef, Actor> = new Map();
  private restartStrategies: Map<
    ActorRef,
    "permanent" | "temporary" | "transient"
  > = new Map();

  constructor(
    _strategy: "one_for_one" | "one_for_all" | "rest_for_one" = "one_for_one",
  ) {
    // Strategy reserved for future implementation of different supervision strategies
    void _strategy;
  }

  /**
   * Add child actor under supervision
   */
  addChild(
    actor: Actor,
    restartStrategy: "permanent" | "temporary" | "transient" = "permanent",
  ): void {
    this.children.set(actor.pid, actor);
    this.restartStrategies.set(actor.pid, restartStrategy);
  }

  /**
   * Handle actor crash (like OTP supervisor behavior)
   */
  handleActorCrash(actor: Actor): void {
    const strategy = this.restartStrategies.get(actor.pid);

    if (strategy === "permanent") {
      // Always restart
      console.log(`Supervisor: Restarting permanent actor ${actor.pid}`);
      actor.send({ type: "restart" });
    } else if (strategy === "transient") {
      // Restart only if terminated abnormally
      console.log(`Supervisor: Actor ${actor.pid} terminated abnormally`);
      actor.send({ type: "restart" });
    } else {
      // Temporary - never restart
      console.log(`Supervisor: Actor ${actor.pid} terminated (temporary)`);
      this.children.delete(actor.pid);
    }
  }

  /**
   * Broadcast message to all children
   */
  broadcast(message: Message): void {
    for (const child of this.children.values()) {
      child.send(message);
    }
  }

  shutdown(): void {
    for (const child of this.children.values()) {
      child.send({ type: "shutdown" });
    }
    this.children.clear();
  }
}

/**
 * GenServer-style Event Manager
 * Like Erlang's gen_event behavior
 */
export class EventManager {
  private subscribers: Actor[] = [];
  private eventHistory: Message[] = [];
  private maxHistorySize = 50;

  /**
   * Register an actor to receive events
   */
  addHandler(actor: Actor): void {
    this.subscribers.push(actor);
  }

  /**
   * Remove an actor from event notifications
   */
  removeHandler(actor: Actor): void {
    this.subscribers = this.subscribers.filter((s) => s.pid !== actor.pid);
  }

  /**
   * Notify all handlers (like gen_event:notify)
   */
  notify(message: Message): void {
    // Store in history with automatic cleanup
    this.eventHistory.push(message);
    if (this.eventHistory.length > this.maxHistorySize) {
      this.eventHistory.shift();
    }

    // Send to all subscribers (concurrent message passing)
    for (const subscriber of this.subscribers) {
      subscriber.send(message);
    }
  }

  /**
   * Get event history (like Erlang's message buffer inspection)
   */
  getHistory(): Message[] {
    return [...this.eventHistory];
  }

  shutdown(): void {
    this.subscribers = [];
    this.eventHistory = [];
  }
}

/**
 * Process Registry - like Erlang's process registry
 * Maps names to PIDs for location transparency
 */
export class ProcessRegistry {
  private registry: Map<string, Actor> = new Map();

  register(name: string, actor: Actor): void {
    this.registry.set(name, actor);
  }

  whereis(name: string): Actor | undefined {
    return this.registry.get(name);
  }

  unregister(name: string): void {
    this.registry.delete(name);
  }

  registered(): string[] {
    return Array.from(this.registry.keys());
  }
}

/**
 * Application - OTP Application behavior
 * Manages the entire actor system lifecycle
 */
export class Application {
  private supervisor: Supervisor;
  private eventManager: EventManager;
  private registry: ProcessRegistry;
  private pidCounter = 0;

  constructor() {
    this.supervisor = new Supervisor("one_for_one");
    this.eventManager = new EventManager();
    this.registry = new ProcessRegistry();
  }

  /**
   * Spawn a new actor (like Erlang's spawn)
   */
  spawn(
    behavior: (message: Message, state: any) => any,
    initialState: any = {},
  ): Actor {
    const pid = `<0.${this.pidCounter++}.0>`; // Erlang-style PID format
    const actor = new Actor(pid, this.supervisor);
    actor.setBehavior(behavior);
    actor.setState(initialState);
    this.supervisor.addChild(actor);
    return actor;
  }

  /**
   * Spawn and register actor with a name
   */
  spawnRegister(
    name: string,
    behavior: (message: Message, state: any) => any,
    initialState: any = {},
  ): Actor {
    const actor = this.spawn(behavior, initialState);
    this.registry.register(name, actor);
    return actor;
  }

  /**
   * Send message to named process
   */
  send(name: string, message: Message): boolean {
    const actor = this.registry.whereis(name);
    if (actor) {
      actor.send(message);
      return true;
    }
    return false;
  }

  /**
   * Get event manager for pub/sub
   */
  getEventManager(): EventManager {
    return this.eventManager;
  }

  /**
   * Get process registry
   */
  getRegistry(): ProcessRegistry {
    return this.registry;
  }

  /**
   * Graceful shutdown of entire application
   */
  shutdown(): void {
    this.supervisor.shutdown();
    this.eventManager.shutdown();
  }
}

// Singleton instance (like Erlang's application controller)
export const erlangSystem = new Application();
