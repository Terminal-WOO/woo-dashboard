import { WOORequest, WOOStatistics } from "./types";
import { BackendService } from "./backendService";

const API_BASE = "http://localhost:8081/api";

interface PostgresDocument {
  id: number;
  title: string;
  subject: string;
  organization: string;
  status: string;
  priority: string;
  metadata: Record<string, any>;
  created_at: string;
  updated_at: string;
}

export class PostgresBackendService implements BackendService {
  private mapDocument(doc: PostgresDocument): WOORequest {
    return {
      id: doc.id.toString(),
      title: doc.title,
      subject: doc.subject,
      organization: doc.organization,
      status: doc.status as WOORequest["status"],
      priority: doc.priority as WOORequest["priority"],
      submittedDate: doc.created_at,
      lastModified: doc.updated_at,
      metadata: doc.metadata,
    };
  }

  private mapDocuments(docs: PostgresDocument[]): WOORequest[] {
    return docs.map((doc) => this.mapDocument(doc));
  }

  async getAll(): Promise<WOORequest[]> {
    try {
      const response = await fetch(`${API_BASE}/documents`);
      if (!response.ok) {
        throw new Error(`HTTP error! status: ${response.status}`);
      }
      const data = await response.json();
      return this.mapDocuments(data.documents || []);
    } catch (error) {
      console.error("Failed to fetch documents from PostgreSQL backend:", error);
      return [];
    }
  }

  async getById(id: string): Promise<WOORequest | undefined> {
    try {
      const response = await fetch(`${API_BASE}/documents/${id}`);
      if (!response.ok) {
        if (response.status === 404) return undefined;
        throw new Error(`HTTP error! status: ${response.status}`);
      }
      const data = await response.json();
      return data.document ? this.mapDocument(data.document) : undefined;
    } catch (error) {
      console.error(`Failed to fetch document ${id}:`, error);
      return undefined;
    }
  }

  async update(id: string, status: WOORequest["status"]): Promise<void> {
    try {
      const response = await fetch(`${API_BASE}/documents/${id}/status`, {
        method: "PUT",
        headers: {
          "Content-Type": "application/json",
        },
        body: JSON.stringify({ status }),
      });
      if (!response.ok) {
        throw new Error(`HTTP error! status: ${response.status}`);
      }
    } catch (error) {
      console.error(`Failed to update document ${id}:`, error);
      throw error;
    }
  }

  async getStatistics(): Promise<WOOStatistics> {
    try {
      const response = await fetch(`${API_BASE}/statistics`);
      if (!response.ok) {
        throw new Error(`HTTP error! status: ${response.status}`);
      }
      const data = await response.json();
      const stats = data.statistics || [];

      // Map PostgreSQL statistics to WOOStatistics format
      const statusCounts: Record<string, number> = {};
      stats.forEach((stat: any) => {
        statusCounts[stat.status] = parseInt(stat.count);
      });

      return {
        totalRequests: stats.reduce((sum: number, s: any) => sum + parseInt(s.count), 0),
        byStatus: statusCounts,
        averageProcessingTime: stats.reduce((sum: number, s: any) =>
          sum + (parseFloat(s.avg_days_in_status) || 0), 0) / (stats.length || 1),
      };
    } catch (error) {
      console.error("Failed to fetch statistics:", error);
      return {
        totalRequests: 0,
        byStatus: {},
        averageProcessingTime: 0,
      };
    }
  }

  async search(query: string): Promise<WOORequest[]> {
    try {
      const response = await fetch(`${API_BASE}/search?q=${encodeURIComponent(query)}`);
      if (!response.ok) {
        throw new Error(`HTTP error! status: ${response.status}`);
      }
      const data = await response.json();
      return this.mapDocuments(data.results || []);
    } catch (error) {
      console.error("Search failed:", error);
      return [];
    }
  }

  async startSimulation(): Promise<void> {
    try {
      const response = await fetch(`${API_BASE}/simulation/start`, {
        method: "POST",
      });
      if (!response.ok) {
        throw new Error(`HTTP error! status: ${response.status}`);
      }
    } catch (error) {
      console.error("Failed to start simulation:", error);
      throw error;
    }
  }

  async stopSimulation(): Promise<void> {
    try {
      const response = await fetch(`${API_BASE}/simulation/stop`, {
        method: "POST",
      });
      if (!response.ok) {
        throw new Error(`HTTP error! status: ${response.status}`);
      }
    } catch (error) {
      console.error("Failed to stop simulation:", error);
      throw error;
    }
  }

  async getRecentEvents(limit: number = 50): Promise<any[]> {
    try {
      const response = await fetch(`${API_BASE}/events?limit=${limit}`);
      if (!response.ok) {
        throw new Error(`HTTP error! status: ${response.status}`);
      }
      const data = await response.json();
      return data.events || [];
    } catch (error) {
      console.error("Failed to fetch events:", error);
      return [];
    }
  }

  async checkHealth(): Promise<boolean> {
    try {
      const response = await fetch(`${API_BASE}/documents`, {
        method: "HEAD",
      });
      return response.ok;
    } catch (error) {
      return false;
    }
  }
}

export const postgresBackendService = new PostgresBackendService();
