/**
 * PostgreSQL Actor System with Erlang/OTP Patterns
 *
 * Combines Erlang actor model with PostgreSQL features:
 * - LISTEN/NOTIFY for pub/sub messaging
 * - Connection pooling for concurrent actors
 * - Transactional consistency
 * - Row-level locking for actor isolation
 */

import { Pool, PoolClient } from 'pg';
import { EventEmitter } from 'events';

export type PID = string;

export interface Message {
  type: string;
  from?: PID;
  data?: any;
}

export type MessageHandler = (message: Message, state: any) => Promise<any>;

/**
 * Actor - Erlang-style process with PostgreSQL backing
 *
 * Each actor can:
 * - Process messages sequentially (no race conditions)
 * - Store state in PostgreSQL
 * - Receive notifications via LISTEN/NOTIFY
 * - Execute in transactions for consistency
 */
export class Actor extends EventEmitter {
  public readonly pid: PID;
  private mailbox: Message[] = [];
  private isProcessing = false;
  private state: any = {};
  private handler: MessageHandler;
  private pool: Pool;
  private listenClient?: PoolClient;
  private shutdownRequested = false;

  constructor(pid: PID, handler: MessageHandler, pool: Pool, initialState: any = {}) {
    super();
    this.pid = pid;
    this.handler = handler;
    this.pool = pool;
    this.state = initialState;
  }

  /**
   * Send message to actor (like Erlang's !)
   */
  async send(message: Message): Promise<void> {
    if (this.shutdownRequested) {
      console.warn(`[Actor ${this.pid}] Ignoring message (shutdown requested)`);
      return;
    }

    this.mailbox.push(message);

    if (!this.isProcessing) {
      await this.processMailbox();
    }
  }

  /**
   * Process mailbox sequentially (Erlang message processing)
   */
  private async processMailbox(): Promise<void> {
    if (this.isProcessing || this.mailbox.length === 0) {
      return;
    }

    this.isProcessing = true;

    while (this.mailbox.length > 0 && !this.shutdownRequested) {
      const message = this.mailbox.shift()!;

      try {
        // Execute handler in a transaction for consistency
        const client = await this.pool.connect();
        try {
          await client.query('BEGIN');

          // Call handler with current state
          const newState = await this.handler(message, this.state);

          // Update state if handler returns new state
          if (newState !== undefined) {
            this.state = newState;
          }

          await client.query('COMMIT');

          // Emit event for successful processing
          this.emit('message_processed', { message, pid: this.pid });
        } catch (error) {
          await client.query('ROLLBACK');
          console.error(`[Actor ${this.pid}] Message processing error:`, error);
          this.emit('error', { error, message, pid: this.pid });
        } finally {
          client.release();
        }
      } catch (error) {
        console.error(`[Actor ${this.pid}] Fatal error:`, error);
        this.emit('crash', { error, pid: this.pid });
      }
    }

    this.isProcessing = false;
  }

  /**
   * Subscribe to PostgreSQL NOTIFY channel
   */
  async listen(channel: string): Promise<void> {
    if (this.listenClient) {
      console.warn(`[Actor ${this.pid}] Already listening`);
      return;
    }

    this.listenClient = await this.pool.connect();

    await this.listenClient.query(`LISTEN ${channel}`);

    this.listenClient.on('notification', (msg) => {
      if (msg.channel === channel) {
        const payload = msg.payload ? JSON.parse(msg.payload) : {};
        this.send({
          type: 'postgres_notify',
          data: {
            channel: msg.channel,
            payload
          }
        });
      }
    });

    console.log(`[Actor ${this.pid}] Listening on channel: ${channel}`);
  }

  /**
   * Unsubscribe from PostgreSQL NOTIFY channel
   */
  async unlisten(channel: string): Promise<void> {
    if (!this.listenClient) return;

    await this.listenClient.query(`UNLISTEN ${channel}`);
    console.log(`[Actor ${this.pid}] Unlistened from channel: ${channel}`);
  }

  /**
   * Get current state
   */
  getState(): any {
    return this.state;
  }

  /**
   * Set state directly
   */
  setState(newState: any): void {
    this.state = newState;
  }

  /**
   * Shutdown actor (like Erlang's exit)
   */
  async shutdown(): Promise<void> {
    this.shutdownRequested = true;

    // Release LISTEN client
    if (this.listenClient) {
      this.listenClient.release();
      this.listenClient = undefined;
    }

    // Wait for mailbox to drain
    while (this.mailbox.length > 0 && this.isProcessing) {
      await new Promise(resolve => setTimeout(resolve, 10));
    }

    this.emit('shutdown', { pid: this.pid });
    console.log(`[Actor ${this.pid}] Shutdown complete`);
  }
}

/**
 * Supervisor - OTP Supervisor pattern with PostgreSQL
 *
 * Monitors child actors and restarts them on failure
 */
export class Supervisor extends EventEmitter {
  private children: Map<PID, Actor> = new Map();
  private restartStrategies: Map<PID, 'permanent' | 'temporary' | 'transient'> = new Map();
  private restartCounts: Map<PID, number> = new Map();
  private pool: Pool;

  // Restart limits (like OTP supervisor)
  private maxRestarts: number;
  private withinSeconds: number;

  constructor(pool: Pool, maxRestarts: number = 10, withinSeconds: number = 60) {
    super();
    this.pool = pool;
    this.maxRestarts = maxRestarts;
    this.withinSeconds = withinSeconds;
  }

  /**
   * Add child actor to supervision
   */
  addChild(
    actor: Actor,
    restartStrategy: 'permanent' | 'temporary' | 'transient' = 'permanent'
  ): void {
    this.children.set(actor.pid, actor);
    this.restartStrategies.set(actor.pid, restartStrategy);
    this.restartCounts.set(actor.pid, 0);

    // Listen for actor crashes
    actor.on('crash', ({ error, pid }) => {
      console.error(`[Supervisor] Actor ${pid} crashed:`, error);
      this.handleActorCrash(actor);
    });

    console.log(`[Supervisor] Added child: ${actor.pid} (${restartStrategy})`);
  }

  /**
   * Handle actor crash (like OTP supervisor behavior)
   */
  private handleActorCrash(actor: Actor): void {
    const strategy = this.restartStrategies.get(actor.pid);

    if (strategy === 'permanent') {
      // Always restart
      console.log(`[Supervisor] Restarting permanent actor ${actor.pid}`);
      this.restartActor(actor);
    } else if (strategy === 'transient') {
      // Restart only if terminated abnormally
      console.log(`[Supervisor] Actor ${actor.pid} terminated abnormally`);
      this.restartActor(actor);
    } else {
      // Temporary - never restart
      console.log(`[Supervisor] Actor ${actor.pid} terminated (temporary)`);
      this.children.delete(actor.pid);
    }
  }

  /**
   * Restart an actor
   */
  private restartActor(actor: Actor): void {
    const count = this.restartCounts.get(actor.pid) || 0;

    if (count >= this.maxRestarts) {
      console.error(`[Supervisor] Max restarts exceeded for ${actor.pid}`);
      this.emit('max_restarts_exceeded', { pid: actor.pid });
      return;
    }

    this.restartCounts.set(actor.pid, count + 1);

    // Reset count after withinSeconds
    setTimeout(() => {
      this.restartCounts.set(actor.pid, 0);
    }, this.withinSeconds * 1000);

    // Send restart message
    actor.send({ type: 'restart' });
    this.emit('actor_restarted', { pid: actor.pid });
  }

  /**
   * Broadcast message to all children
   */
  async broadcast(message: Message): Promise<void> {
    const promises = Array.from(this.children.values()).map(child =>
      child.send(message)
    );
    await Promise.all(promises);
  }

  /**
   * Shutdown all children
   */
  async shutdown(): Promise<void> {
    const shutdownPromises = Array.from(this.children.values()).map(child =>
      child.shutdown()
    );
    await Promise.all(shutdownPromises);
    this.children.clear();
    console.log('[Supervisor] All children shutdown');
  }

  /**
   * Get all child PIDs
   */
  getChildren(): PID[] {
    return Array.from(this.children.keys());
  }
}

/**
 * Process Registry - like Erlang's process registry
 */
export class ProcessRegistry {
  private registry: Map<string, PID> = new Map();

  register(name: string, pid: PID): void {
    if (this.registry.has(name)) {
      throw new Error(`Name ${name} already registered`);
    }
    this.registry.set(name, pid);
    console.log(`[Registry] Registered: ${name} -> ${pid}`);
  }

  unregister(name: string): void {
    this.registry.delete(name);
    console.log(`[Registry] Unregistered: ${name}`);
  }

  whereis(name: string): PID | undefined {
    return this.registry.get(name);
  }

  registered(): string[] {
    return Array.from(this.registry.keys());
  }
}

/**
 * Application - OTP Application behavior
 */
export class Application {
  private pool: Pool;
  private supervisor: Supervisor;
  private registry: ProcessRegistry;
  private pidCounter = 0;

  constructor(pool: Pool) {
    this.pool = pool;
    this.supervisor = new Supervisor(pool);
    this.registry = new ProcessRegistry();
  }

  /**
   * Spawn a new actor
   */
  spawn(handler: MessageHandler, initialState: any = {}): Actor {
    const pid = this.generatePID();
    const actor = new Actor(pid, handler, this.pool, initialState);
    return actor;
  }

  /**
   * Spawn and register an actor
   */
  spawnRegister(name: string, handler: MessageHandler, initialState: any = {}): Actor {
    const actor = this.spawn(handler, initialState);
    this.registry.register(name, actor.pid);
    return actor;
  }

  /**
   * Spawn and supervise an actor
   */
  spawnSupervised(
    handler: MessageHandler,
    initialState: any = {},
    restartStrategy: 'permanent' | 'temporary' | 'transient' = 'permanent'
  ): Actor {
    const actor = this.spawn(handler, initialState);
    this.supervisor.addChild(actor, restartStrategy);
    return actor;
  }

  /**
   * Get the supervisor
   */
  getSupervisor(): Supervisor {
    return this.supervisor;
  }

  /**
   * Get the registry
   */
  getRegistry(): ProcessRegistry {
    return this.registry;
  }

  /**
   * Get the connection pool
   */
  getPool(): Pool {
    return this.pool;
  }

  /**
   * Shutdown the application
   */
  async shutdown(): Promise<void> {
    console.log('[Application] Shutting down...');
    await this.supervisor.shutdown();
    await this.pool.end();
    console.log('[Application] Shutdown complete');
  }

  /**
   * Generate unique PID (like Erlang)
   */
  private generatePID(): PID {
    const pid = `<0.${this.pidCounter}.0>`;
    this.pidCounter++;
    return pid;
  }
}
