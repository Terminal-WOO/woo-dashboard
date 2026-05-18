import { WOORequest, WOOStats, WOOStatus, OrganizationType } from "./types";
import { BackendService } from "./backendService";

const API_BASE = "http://localhost:8081/api";

interface PostgresDocument {
  id: number;
  title: string;
  subject: string;
  organization: string;
  organization_type: string;
  category: string;
  status: string;
  metadata: Record<string, any>;
  created_at: string;
  updated_at: string;
  decided_date?: string;
  requester?: string;
  handler?: string;
}

export class PostgresBackendService implements BackendService {
  private mapDocument(doc: PostgresDocument): WOORequest {
    return {
      id: doc.id.toString(),
      title: doc.title,
      subject: doc.subject,
      organization: doc.organization,
      organizationType: (doc.organization_type ||
        "gemeente") as OrganizationType,
      category: doc.category || "Algemeen",
      status: doc.status as WOOStatus,
      submittedDate: doc.created_at,
      lastModified: doc.updated_at,
      decidedDate: doc.decided_date,
      requester: doc.requester,
      handler: doc.handler,
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
      console.error(
        "Failed to fetch documents from PostgreSQL backend:",
        error,
      );
      return [];
    }
  }

  async getById(id: string): Promise<WOORequest | null> {
    try {
      const response = await fetch(`${API_BASE}/documents/${id}`);
      if (!response.ok) {
        if (response.status === 404) return null;
        throw new Error(`HTTP error! status: ${response.status}`);
      }
      const data = await response.json();
      return data.document ? this.mapDocument(data.document) : null;
    } catch (error) {
      console.error(`Failed to fetch document ${id}:`, error);
      return null;
    }
  }

  async getByOrganization(org: string): Promise<WOORequest[]> {
    try {
      const allDocs = await this.getAll();
      return allDocs.filter((doc) => doc.organization === org);
    } catch (error) {
      console.error(
        `Failed to fetch documents for organization ${org}:`,
        error,
      );
      return [];
    }
  }

  async getByStatus(status: WOOStatus): Promise<WOORequest[]> {
    try {
      const allDocs = await this.getAll();
      return allDocs.filter((doc) => doc.status === status);
    } catch (error) {
      console.error(`Failed to fetch documents with status ${status}:`, error);
      return [];
    }
  }

  async update(id: string, status: WOOStatus): Promise<void> {
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

  async getStatistics(): Promise<WOOStats> {
    try {
      const allDocs = await this.getAll();
      const received = allDocs.filter(
        (doc) => doc.status === "Ontvangen",
      ).length;
      const inProgress = allDocs.filter(
        (doc) =>
          doc.status === "In behandeling" ||
          doc.status === "1e Concept" ||
          doc.status === "2e Concept" ||
          doc.status === "Definitief",
      ).length;
      const completed = allDocs.filter(
        (doc) => doc.status === "Gepubliceerd" || doc.status === "Afgerond",
      ).length;

      return {
        totalRequests: allDocs.length,
        received,
        inProgress,
        completed,
        averageHandlingDays: 0,
      };
    } catch (error) {
      console.error("Failed to fetch statistics:", error);
      return {
        totalRequests: 0,
        received: 0,
        inProgress: 0,
        completed: 0,
        averageHandlingDays: 0,
      };
    }
  }

  async search(query: string): Promise<WOORequest[]> {
    try {
      const response = await fetch(
        `${API_BASE}/search?q=${encodeURIComponent(query)}`,
      );
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
