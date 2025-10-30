/**
 * Backend Service Abstraction Layer
 *
 * Provides a unified interface to switch between:
 * - Mock database (TypeScript in-memory)
 * - Erlang/OTP backend (real backend server)
 *
 * Toggle via environment variable or runtime config
 */

import { mockDatabaseService } from "./mockDatabase";
import { erlangBackendService } from "./erlangBackendService";
import { postgresBackendService } from "./postgresBackendService";
import { WOORequest, WOOStats, WOOStatus } from "./types";

// Adapter for mockDatabaseService to match BackendService interface
class MockBackendAdapter implements BackendService {
  async getAll(): Promise<WOORequest[]> {
    return mockDatabaseService.queryAll();
  }

  async getById(id: string): Promise<WOORequest | null> {
    const all = mockDatabaseService.queryAll();
    return all.find((req) => req.id === id) || null;
  }

  async getByOrganization(org: string): Promise<WOORequest[]> {
    return mockDatabaseService.queryByOrganization(org);
  }

  async getByStatus(status: WOOStatus): Promise<WOORequest[]> {
    return mockDatabaseService
      .queryAll()
      .filter((req) => req.status === status);
  }

  async update(id: string, newStatus: WOOStatus): Promise<void> {
    mockDatabaseService.update(id, newStatus);
  }

  async getStatistics(): Promise<WOOStats> {
    const stats = mockDatabaseService.getStatistics();
    const all = mockDatabaseService.queryAll();

    const received = all.filter((r) => r.status === "Ontvangen").length;
    const inProgress = all.filter(
      (r) =>
        r.status === "In behandeling" ||
        r.status === "1e Concept" ||
        r.status === "2e Concept" ||
        r.status === "Definitief",
    ).length;
    const completed = all.filter((r) => r.status === "Gepubliceerd").length;

    return {
      totalRequests: stats.total,
      received,
      inProgress,
      completed,
      averageHandlingDays: 0,
    };
  }
}

const mockBackendAdapter = new MockBackendAdapter();

export type BackendType = "mock" | "erlang" | "postgres";

export interface BackendService {
  getAll(): Promise<WOORequest[]>;
  getById(id: string): Promise<WOORequest | null>;
  getByOrganization(org: string): Promise<WOORequest[]>;
  getByStatus(status: WOOStatus): Promise<WOORequest[]>;
  update(id: string, newStatus: WOOStatus): Promise<void>;
  getStatistics(): Promise<WOOStats>;
  startSimulation?(): Promise<void>;
  stopSimulation?(): Promise<void>;
}

class BackendServiceManager {
  private currentBackend: BackendType = "mock";
  private backend: BackendService;

  constructor() {
    // Check environment variable or localStorage for backend preference
    const storedBackend = localStorage.getItem(
      "woo_backend_type",
    ) as BackendType;
    const envBackend = import.meta.env.VITE_BACKEND_TYPE as BackendType;

    this.currentBackend = storedBackend || envBackend || "mock";
    this.backend = this.getBackendImplementation(this.currentBackend);

    console.log(`[BackendService] Using ${this.currentBackend} backend`);
  }

  /**
   * Get the current backend type
   */
  getBackendType(): BackendType {
    return this.currentBackend;
  }

  /**
   * Switch to a different backend
   */
  switchBackend(type: BackendType): void {
    if (type === this.currentBackend) {
      console.log(`[BackendService] Already using ${type} backend`);
      return;
    }

    console.log(
      `[BackendService] Switching from ${this.currentBackend} to ${type} backend`,
    );
    this.currentBackend = type;
    this.backend = this.getBackendImplementation(type);
    localStorage.setItem("woo_backend_type", type);
  }

  /**
   * Check if Erlang backend is available
   */
  async checkErlangBackendAvailable(): Promise<boolean> {
    try {
      await erlangBackendService.healthCheck();
      return true;
    } catch (error) {
      console.warn("[BackendService] Erlang backend not available:", error);
      return false;
    }
  }

  /**
   * Check if PostgreSQL backend is available
   */
  async checkPostgresBackendAvailable(): Promise<boolean> {
    try {
      return await postgresBackendService.checkHealth();
    } catch (error) {
      console.warn("[BackendService] PostgreSQL backend not available:", error);
      return false;
    }
  }

  /**
   * Get backend implementation based on type
   */
  private getBackendImplementation(type: BackendType): BackendService {
    switch (type) {
      case "erlang":
        return erlangBackendService;
      case "postgres":
        return postgresBackendService;
      case "mock":
      default:
        return mockBackendAdapter;
    }
  }

  /**
   * Proxy all methods to current backend
   */
  async getAll(): Promise<WOORequest[]> {
    return this.backend.getAll();
  }

  async getById(id: string): Promise<WOORequest | null> {
    return this.backend.getById(id);
  }

  async getByOrganization(org: string): Promise<WOORequest[]> {
    return this.backend.getByOrganization(org);
  }

  async getByStatus(status: WOOStatus): Promise<WOORequest[]> {
    return this.backend.getByStatus(status);
  }

  async update(id: string, newStatus: WOOStatus): Promise<void> {
    return this.backend.update(id, newStatus);
  }

  async getStatistics(): Promise<WOOStats> {
    return this.backend.getStatistics();
  }

  /**
   * Start simulation (if supported by backend)
   */
  async startSimulation(): Promise<void> {
    if (this.backend.startSimulation) {
      return this.backend.startSimulation();
    }
    console.warn(
      "[BackendService] Simulation not supported by current backend",
    );
  }

  /**
   * Stop simulation (if supported by backend)
   */
  async stopSimulation(): Promise<void> {
    if (this.backend.stopSimulation) {
      return this.backend.stopSimulation();
    }
    console.warn(
      "[BackendService] Simulation not supported by current backend",
    );
  }
}

// Export singleton instance
export const backendService = new BackendServiceManager();
