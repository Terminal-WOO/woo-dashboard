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
      "[Mock Database] Database initialized with 24 documents from Utrecht and Flevoland",
    );
  }

  private insertInitialData(): void {
    const initialRequests: WOORequest[] = [
      // Gemeente Utrecht (12 documenten) - verschillende statussen
      {
        id: "WOO-UTR-2024-001",
        title: "Informatie over nieuwbouwproject Merwedekanaal",
        status: "Ontvangen",
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
        status: "1e Concept",
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
        status: "2e Concept",
        submittedDate: "2024-10-25",
        organization: "Gemeente Utrecht",
        organizationType: "gemeente",
        category: "Milieu",
        subject: "Contracten met afvalverwerkers en milieu-effectrapportages",
        requester: "Milieudefensie Utrecht",
        handler: "Afdeling Milieu",
        lastModified: new Date().toISOString(),
      },
      {
        id: "WOO-UTR-2024-005",
        title: "Begroting gemeentelijke sportvelden 2024",
        status: "Definitief",
        submittedDate: "2024-04-12",
        organization: "Gemeente Utrecht",
        organizationType: "gemeente",
        category: "Sport",
        subject: "Budgettering en planning sportfaciliteiten",
        requester: "Sportvereniging Utrecht",
        handler: "Afdeling Sport",
        lastModified: new Date().toISOString(),
      },
      {
        id: "WOO-UTR-2024-006",
        title: "Horecavergunningen binnenstad",
        status: "Gepubliceerd",
        submittedDate: "2024-01-08",
        decidedDate: "2024-09-20",
        organization: "Gemeente Utrecht",
        organizationType: "gemeente",
        category: "Horeca",
        subject: "Overzicht verleende horecavergunningen afgelopen jaar",
        handler: "Afdeling Vergunningen",
        lastModified: new Date().toISOString(),
      },
      {
        id: "WOO-UTR-2024-007",
        title: "Parkeerbeleid historische binnenstad",
        status: "Ontvangen",
        submittedDate: "2024-08-15",
        organization: "Gemeente Utrecht",
        organizationType: "gemeente",
        category: "Verkeer",
        subject: "Nota's en rapporten over parkeerbeleid",
        requester: "Bewonersvereniging Binnenstad",
        handler: "Afdeling Mobiliteit",
        lastModified: new Date().toISOString(),
      },
      {
        id: "WOO-UTR-2024-008",
        title: "Onderhoud openbare gebouwen",
        status: "In behandeling",
        submittedDate: "2024-06-22",
        organization: "Gemeente Utrecht",
        organizationType: "gemeente",
        category: "Vastgoed",
        subject: "Onderhoudscontracten gemeentelijke panden",
        requester: "Wijkraad Oost",
        handler: "Afdeling Vastgoed",
        lastModified: new Date().toISOString(),
      },
      {
        id: "WOO-UTR-2024-009",
        title: "Groenbeleid Wilhelminapark",
        status: "1e Concept",
        submittedDate: "2024-05-18",
        organization: "Gemeente Utrecht",
        organizationType: "gemeente",
        category: "Groen",
        subject: "Herinrichtingsplannen Wilhelminapark",
        requester: "Stichting Groen Utrecht",
        handler: "Afdeling Groen",
        lastModified: new Date().toISOString(),
      },
      {
        id: "WOO-UTR-2024-010",
        title: "Duurzaamheidssubsidies woningisolatie",
        status: "2e Concept",
        submittedDate: "2024-07-10",
        organization: "Gemeente Utrecht",
        organizationType: "gemeente",
        category: "Duurzaamheid",
        subject: "Subsidieregelingen verduurzaming woningen",
        handler: "Afdeling Duurzaamheid",
        lastModified: new Date().toISOString(),
      },
      {
        id: "WOO-UTR-2024-011",
        title: "Rampenplan Utrecht 2024",
        status: "Definitief",
        submittedDate: "2024-03-05",
        organization: "Gemeente Utrecht",
        organizationType: "gemeente",
        category: "Veiligheid",
        subject: "Crisisbeheersingsplannen en rampenbestrijding",
        handler: "Afdeling Veiligheid",
        lastModified: new Date().toISOString(),
      },
      {
        id: "WOO-UTR-2024-012",
        title: "Aanbesteding fietsenstallingen station",
        status: "Gepubliceerd",
        submittedDate: "2024-02-14",
        decidedDate: "2024-08-30",
        organization: "Gemeente Utrecht",
        organizationType: "gemeente",
        category: "Mobiliteit",
        subject: "Offertes en gunningsbesluit fietsenstallingen",
        handler: "Afdeling Mobiliteit",
        lastModified: new Date().toISOString(),
      },

      // Provincie Flevoland (12 documenten) - verschillende statussen
      {
        id: "WOO-FLE-2024-001",
        title: "Stikstofrapportage landbouw",
        status: "Ontvangen",
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
        status: "1e Concept",
        submittedDate: "2024-02-01",
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
        status: "2e Concept",
        submittedDate: "2024-09-20",
        organization: "Provincie Flevoland",
        organizationType: "provincie",
        category: "Landbouw",
        subject: "Criteria en toekenningen subsidie duurzame landbouw",
        requester: "LTO Flevoland",
        handler: "Afdeling Economie",
        lastModified: new Date().toISOString(),
      },
      {
        id: "WOO-FLE-2024-005",
        title: "Waterhuishouding IJsselmeerpolders",
        status: "Definitief",
        submittedDate: "2024-04-22",
        organization: "Provincie Flevoland",
        organizationType: "provincie",
        category: "Water",
        subject: "Beheerplannen waterhuishouding en drainage",
        requester: "Waterschap Zuiderzeeland",
        handler: "Afdeling Water",
        lastModified: new Date().toISOString(),
      },
      {
        id: "WOO-FLE-2024-006",
        title: "Natuurontwikkeling Oostvaardersplassen",
        status: "Gepubliceerd",
        submittedDate: "2024-01-30",
        decidedDate: "2024-09-10",
        organization: "Provincie Flevoland",
        organizationType: "provincie",
        category: "Natuur",
        subject: "Beheerplan en toekomstvisie Oostvaardersplassen",
        handler: "Afdeling Natuur",
        lastModified: new Date().toISOString(),
      },
      {
        id: "WOO-FLE-2024-007",
        title: "Economisch actieplan Flevoland 2025",
        status: "Ontvangen",
        submittedDate: "2024-08-05",
        organization: "Provincie Flevoland",
        organizationType: "provincie",
        category: "Economie",
        subject: "Strategische plannen economische ontwikkeling",
        requester: "Ondernemersfederatie Flevoland",
        handler: "Afdeling Economie",
        lastModified: new Date().toISOString(),
      },
      {
        id: "WOO-FLE-2024-008",
        title: "Fietsnetwerk Flevoland uitbreiding",
        status: "In behandeling",
        submittedDate: "2024-06-12",
        organization: "Provincie Flevoland",
        organizationType: "provincie",
        category: "Mobiliteit",
        subject: "Plannen uitbreiding provinciaal fietsroutenetwerk",
        requester: "Fietsersbond Flevoland",
        handler: "Afdeling Mobiliteit",
        lastModified: new Date().toISOString(),
      },
      {
        id: "WOO-FLE-2024-009",
        title: "Cultuursubsidies 2024-2028",
        status: "1e Concept",
        submittedDate: "2024-05-08",
        organization: "Provincie Flevoland",
        organizationType: "provincie",
        category: "Cultuur",
        subject: "Meerjarenaanvraag cultuursubsidies",
        requester: "Kunstencentrum Flevoland",
        handler: "Afdeling Cultuur",
        lastModified: new Date().toISOString(),
      },
      {
        id: "WOO-FLE-2024-010",
        title: "Archeologische onderzoeken Swifterbant",
        status: "2e Concept",
        submittedDate: "2024-07-25",
        organization: "Provincie Flevoland",
        organizationType: "provincie",
        category: "Archeologie",
        subject: "Rapporten archeologische opgravingen",
        handler: "Afdeling Erfgoed",
        lastModified: new Date().toISOString(),
      },
      {
        id: "WOO-FLE-2024-011",
        title: "Verkeersveiligheidsplan N307",
        status: "Definitief",
        submittedDate: "2024-03-18",
        organization: "Provincie Flevoland",
        organizationType: "provincie",
        category: "Verkeersveiligheid",
        subject: "Maatregelen verkeersveiligheid provinciale weg",
        handler: "Afdeling Infrastructuur",
        lastModified: new Date().toISOString(),
      },
      {
        id: "WOO-FLE-2024-012",
        title: "Toerismevisie Flevoland 2030",
        status: "Gepubliceerd",
        submittedDate: "2024-02-28",
        decidedDate: "2024-09-05",
        organization: "Provincie Flevoland",
        organizationType: "provincie",
        category: "Toerisme",
        subject: "Strategische toerismeontwikkeling komende jaren",
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
