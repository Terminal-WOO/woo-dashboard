/**
 * Erlang Backend Service
 *
 * Service layer for communicating with the real Erlang/OTP backend
 * running on localhost:8080
 */

import { WOORequest, WOOStats, WOOStatus } from "./types";

const API_BASE = "http://localhost:8080/api";

export interface ErlangBackendService {
  getAll(): Promise<WOORequest[]>;
  getById(id: string): Promise<WOORequest | null>;
  getByOrganization(org: string): Promise<WOORequest[]>;
  getByStatus(status: WOOStatus): Promise<WOORequest[]>;
  update(id: string, newStatus: WOOStatus): Promise<void>;
  getStatistics(): Promise<WOOStats>;
  startSimulation(): Promise<void>;
  stopSimulation(): Promise<void>;
  healthCheck(): Promise<{ status: string; simulation_running: boolean }>;
}

class ErlangBackendServiceImpl implements ErlangBackendService {
  /**
   * Get all documents from Erlang backend
   */
  async getAll(): Promise<WOORequest[]> {
    try {
      const response = await fetch(`${API_BASE}/documents`);
      if (!response.ok) {
        throw new Error(`HTTP error! status: ${response.status}`);
      }
      const data = await response.json();
      return this.mapDocuments(data.documents || []);
    } catch (error) {
      console.error("[ErlangBackend] Failed to fetch documents:", error);
      throw error;
    }
  }

  /**
   * Get a specific document by ID
   */
  async getById(id: string): Promise<WOORequest | null> {
    try {
      const response = await fetch(`${API_BASE}/documents/${id}`);
      if (response.status === 404) {
        return null;
      }
      if (!response.ok) {
        throw new Error(`HTTP error! status: ${response.status}`);
      }
      const data = await response.json();
      return this.mapDocument(data);
    } catch (error) {
      console.error(`[ErlangBackend] Failed to fetch document ${id}:`, error);
      throw error;
    }
  }

  /**
   * Get documents by organization
   */
  async getByOrganization(org: string): Promise<WOORequest[]> {
    try {
      const response = await fetch(`${API_BASE}/statistics/${org}`);
      if (!response.ok) {
        throw new Error(`HTTP error! status: ${response.status}`);
      }
      const data = await response.json();
      return this.mapDocuments(data.documents || []);
    } catch (error) {
      console.error(
        `[ErlangBackend] Failed to fetch documents for ${org}:`,
        error,
      );
      throw error;
    }
  }

  /**
   * Get documents by status (client-side filter)
   */
  async getByStatus(status: WOOStatus): Promise<WOORequest[]> {
    const allDocs = await this.getAll();
    return allDocs.filter((doc) => doc.status === status);
  }

  /**
   * Update document status
   */
  async update(id: string, newStatus: WOOStatus): Promise<void> {
    try {
      const response = await fetch(`${API_BASE}/documents/${id}/status`, {
        method: "POST",
        headers: {
          "Content-Type": "application/json",
        },
        body: JSON.stringify({ status: newStatus }),
      });

      if (!response.ok) {
        throw new Error(`HTTP error! status: ${response.status}`);
      }

      console.log(`[ErlangBackend] Updated ${id} to ${newStatus}`);
    } catch (error) {
      console.error(
        `[ErlangBackend] Failed to update document ${id}:`,
        error,
      );
      throw error;
    }
  }

  /**
   * Get statistics from backend
   */
  async getStatistics(): Promise<WOOStats> {
    try {
      const response = await fetch(`${API_BASE}/statistics`);
      if (!response.ok) {
        throw new Error(`HTTP error! status: ${response.status}`);
      }
      const data = await response.json();

      return {
        totalRequests: data.total_requests || 0,
        received: data.received || 0,
        inProgress: data.in_progress || 0,
        completed: data.completed || 0,
        averageHandlingDays: data.average_handling_days || 0,
      };
    } catch (error) {
      console.error("[ErlangBackend] Failed to fetch statistics:", error);
      throw error;
    }
  }

  /**
   * Start the simulation on backend
   */
  async startSimulation(): Promise<void> {
    try {
      const response = await fetch(`${API_BASE}/simulation/start`, {
        method: "POST",
      });

      if (!response.ok) {
        const error = await response.json();
        throw new Error(error.error || "Failed to start simulation");
      }

      console.log("[ErlangBackend] Simulation started");
    } catch (error) {
      console.error("[ErlangBackend] Failed to start simulation:", error);
      throw error;
    }
  }

  /**
   * Stop the simulation on backend
   */
  async stopSimulation(): Promise<void> {
    try {
      const response = await fetch(`${API_BASE}/simulation/stop`, {
        method: "POST",
      });

      if (!response.ok) {
        const error = await response.json();
        throw new Error(error.error || "Failed to stop simulation");
      }

      console.log("[ErlangBackend] Simulation stopped");
    } catch (error) {
      console.error("[ErlangBackend] Failed to stop simulation:", error);
      throw error;
    }
  }

  /**
   * Health check - verify backend is running
   */
  async healthCheck(): Promise<{
    status: string;
    simulation_running: boolean;
  }> {
    try {
      const response = await fetch(`${API_BASE}/health`);
      if (!response.ok) {
        throw new Error(`HTTP error! status: ${response.status}`);
      }
      return await response.json();
    } catch (error) {
      console.error("[ErlangBackend] Health check failed:", error);
      throw error;
    }
  }

  /**
   * Map Erlang backend document format to frontend format
   */
  private mapDocument(doc: any): WOORequest {
    return {
      id: doc.id,
      title: doc.title,
      status: doc.status as WOOStatus,
      submittedDate: doc.submittedDate,
      organization: doc.organization,
      organizationType: doc.organizationType as "gemeente" | "provincie",
      category: doc.category,
      subject: doc.subject,
      requester: doc.requester,
      handler: doc.handler,
      decidedDate: doc.decidedDate,
      lastModified: doc.lastModified,
    };
  }

  /**
   * Map array of documents
   */
  private mapDocuments(docs: any[]): WOORequest[] {
    return docs.map((doc) => this.mapDocument(doc));
  }
}

// Export singleton instance
export const erlangBackendService: ErlangBackendService =
  new ErlangBackendServiceImpl();
