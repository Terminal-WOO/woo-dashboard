/**
 * Mock Database - In-memory data store that simulates SQLite
 * Werkt 100% in de browser zonder externe dependencies
 */

import { WOORequest, WOOStatus } from "./types";

class MockDatabaseService {
  private requests: Map<string, WOORequest> = new Map();
  private initialized = false;

  async init(): Promise<void> {
    if (this.initialized) return;

    console.log("[Mock Database] Initializing in-memory database...");

    this.insertInitialData();

    this.initialized = true;
    console.log(
      "[Mock Database] Database initialized with Utrecht and Flevoland data",
    );
  }

  private insertInitialData(): void {
    const initialRequests: WOORequest[] = [
      // Gemeente Utrecht
      {
        id: "WOO-UTR-2024-001",
        title: "Informatie over nieuwbouwproject Merwedekanaal",
        status: "1e Concept",
        submittedDate: "2024-01-15",
        organization: "Gemeente Utrecht",
        organizationType: "gemeente",
        category: "Ruimtelijke ordening",
        subject:
          "Verzoek om alle documenten m.b.t. het nieuwbouwproject Merwedekanaal",
        requester: "Jan de Vries",
        handler: "Afdeling Ruimtelijke Ordening",
        lastModified: new Date().toISOString(),
      },
      {
        id: "WOO-UTR-2024-002",
        title: "Correspondentie over verkeersplan binnenstad",
        status: "In behandeling",
        submittedDate: "2024-02-20",
        organization: "Gemeente Utrecht",
        organizationType: "gemeente",
        category: "Verkeer",
        subject:
          "Alle e-mails en nota's over het nieuwe verkeersplan voor de binnenstad",
        requester: "Stadsbelang Utrecht",
        handler: "Afdeling Mobiliteit",
        lastModified: new Date().toISOString(),
      },
      {
        id: "WOO-UTR-2024-003",
        title: "Subsidieverlening culturele instellingen 2024",
        status: "Definitief",
        submittedDate: "2024-03-10",
        organization: "Gemeente Utrecht",
        organizationType: "gemeente",
        category: "Cultuur",
        subject: "Besluitvorming en criteria subsidies culturele sector",
        handler: "Afdeling Cultuur",
        lastModified: new Date().toISOString(),
      },
      {
        id: "WOO-UTR-2024-004",
        title: "Contracten afvalverwerking",
        status: "Ontvangen",
        submittedDate: "2024-10-25",
        organization: "Gemeente Utrecht",
        organizationType: "gemeente",
        category: "Milieu",
        subject: "Contracten met afvalverwerkers en milieu-effectrapportages",
        requester: "Milieudefensie Utrecht",
        lastModified: new Date().toISOString(),
      },

      // Provincie Flevoland
      {
        id: "WOO-FLE-2024-001",
        title: "Stikstofrapportage landbouw",
        status: "2e Concept",
        submittedDate: "2024-01-20",
        organization: "Provincie Flevoland",
        organizationType: "provincie",
        category: "Landbouw en Natuur",
        subject:
          "Rapportages en correspondentie over stikstofuitstoot landbouw",
        requester: "Natuur & Milieu",
        handler: "Afdeling Omgevingsbeleid",
        lastModified: new Date().toISOString(),
      },
      {
        id: "WOO-FLE-2024-002",
        title: "Windmolenpark Noordoostpolder besluitvorming",
        status: "In behandeling",
        submittedDate: "2024-03-15",
        organization: "Provincie Flevoland",
        organizationType: "provincie",
        category: "Energie",
        subject: "Besluitvorming en adviezen windmolenpark Noordoostpolder",
        requester: "Bewonerscomité NOP",
        handler: "Afdeling Energie",
        lastModified: new Date().toISOString(),
      },
      {
        id: "WOO-FLE-2024-003",
        title: "N23 reconstructie projectplan",
        status: "Gepubliceerd",
        submittedDate: "2024-02-01",
        decidedDate: "2024-09-15",
        organization: "Provincie Flevoland",
        organizationType: "provincie",
        category: "Infrastructuur",
        subject: "Projectplannen en MER voor reconstructie N23",
        handler: "Afdeling Infrastructuur",
        lastModified: new Date().toISOString(),
      },
      {
        id: "WOO-FLE-2024-004",
        title: "Subsidieregeling duurzame landbouw",
        status: "1e Concept",
        submittedDate: "2024-09-20",
        organization: "Provincie Flevoland",
        organizationType: "provincie",
        category: "Landbouw",
        subject: "Criteria en toekenningen subsidie duurzame landbouw",
        requester: "LTO Flevoland",
        handler: "Afdeling Economie",
        lastModified: new Date().toISOString(),
      },
    ];

    initialRequests.forEach((req) => {
      this.requests.set(req.id, req);
    });
  }

  insert(request: WOORequest): void {
    this.requests.set(request.id, request);
    console.log(`[Mock Database] Inserted document: ${request.id}`);
  }

  update(id: string, status: WOOStatus): void {
    const request = this.requests.get(id);
    if (request) {
      request.status = status;
      request.lastModified = new Date().toISOString();
      this.requests.set(id, request);
      console.log(`[Mock Database] Updated document ${id}: -> ${status}`);
    }
  }

  queryAll(): WOORequest[] {
    return Array.from(this.requests.values()).sort(
      (a, b) =>
        new Date(b.lastModified).getTime() - new Date(a.lastModified).getTime(),
    );
  }

  queryByOrganization(organization: string): WOORequest[] {
    return this.queryAll().filter((req) => req.organization === organization);
  }

  getStatistics(): {
    total: number;
    byOrg: Record<string, number>;
    byStatus: Record<string, number>;
  } {
    const all = this.queryAll();

    const byOrg: Record<string, number> = {};
    const byStatus: Record<string, number> = {};

    all.forEach((req) => {
      byOrg[req.organization] = (byOrg[req.organization] || 0) + 1;
      byStatus[req.status] = (byStatus[req.status] || 0) + 1;
    });

    return {
      total: all.length,
      byOrg,
      byStatus,
    };
  }
}

// Singleton instance
export const mockDatabaseService = new MockDatabaseService();
