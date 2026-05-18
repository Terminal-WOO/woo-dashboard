/**
 * Mock Database Service
 * Simuleert een in-memory database voor WOO requests
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
      "[Mock Database] Database initialized with 24 documents (open.overheid.nl)",
    );
  }

  private insertInitialData(): void {
    const now = Date.now();

    // Helper function to create timestamps spread over the last 24 minutes
    const getTimestamp = (minutesAgo: number) =>
      new Date(now - minutesAgo * 60000).toISOString();

    const initialRequests: WOORequest[] = [
      // Ministerie van Justitie en Veiligheid (6 documenten)
      {
        id: "WOO-JENV-2025-001",
        title:
          "Besluit op Woo-verzoek over correspondentie asielopvangcapaciteit",
        status: "Ontvangen",
        submittedDate: "2025-01-14",
        organization: "Ministerie van Justitie en Veiligheid",
        organizationType: "ministerie",
        category: "Asiel en migratie",
        subject:
          "Gepubliceerd op open.overheid.nl — interne correspondentie over asielopvangcapaciteit 2024-2025",
        requester: "VluchtelingenWerk Nederland",
        handler: "Directie Wetgeving en Juridische Zaken",
        lastModified: getTimestamp(24),
      },
      {
        id: "WOO-JENV-2025-002",
        title:
          "Besluit op Woo-verzoek over capaciteitsproblematiek gevangeniswezen",
        status: "In behandeling",
        submittedDate: "2025-02-03",
        organization: "Ministerie van Justitie en Veiligheid",
        organizationType: "ministerie",
        category: "Rechtspleging",
        subject:
          "Gepubliceerd op open.overheid.nl — beleidsnota's en Kamerbrieven gevangeniswezen",
        requester: "Nederlandse Orde van Advocaten",
        handler: "Directie Sancties en Rechtsbescherming",
        lastModified: getTimestamp(23),
      },
      {
        id: "WOO-JENV-2025-003",
        title:
          "Besluit op Woo-verzoek over onderzoek naar online kindermisbruik",
        status: "1e Concept",
        submittedDate: "2025-02-18",
        organization: "Ministerie van Justitie en Veiligheid",
        organizationType: "ministerie",
        category: "Veiligheid",
        subject:
          "Gepubliceerd op open.overheid.nl — onderzoeksrapporten en beleidsreacties",
        handler: "Directie Veiligheid en Criminaliteitsbestrijding",
        lastModified: getTimestamp(22),
      },
      {
        id: "WOO-JENV-2025-004",
        title:
          "Besluit op Woo-verzoek over samenwerking Europese strafrechtelijke autoriteiten",
        status: "2e Concept",
        submittedDate: "2025-03-05",
        organization: "Ministerie van Justitie en Veiligheid",
        organizationType: "ministerie",
        category: "Internationale samenwerking",
        subject:
          "Gepubliceerd op open.overheid.nl — verdragen en uitwisselingsprotocollen",
        requester: "European Criminal Bar Association",
        handler: "Directie Internationale Zaken",
        lastModified: getTimestamp(21),
      },
      {
        id: "WOO-JENV-2025-005",
        title:
          "Besluit op Woo-verzoek over beleidslijnen politie-inzet demonstraties",
        status: "Definitief",
        submittedDate: "2025-01-22",
        organization: "Ministerie van Justitie en Veiligheid",
        organizationType: "ministerie",
        category: "Openbare orde",
        subject:
          "Gepubliceerd op open.overheid.nl — operationele richtlijnen en evaluaties",
        handler: "Directie Politie",
        lastModified: getTimestamp(20),
      },
      {
        id: "WOO-JENV-2025-006",
        title:
          "Besluit op Woo-verzoek over financiering rechtsbijstand 2025",
        status: "Gepubliceerd",
        submittedDate: "2024-11-10",
        decidedDate: "2025-04-01",
        organization: "Ministerie van Justitie en Veiligheid",
        organizationType: "ministerie",
        category: "Rechtsbijstand",
        subject:
          "Gepubliceerd op open.overheid.nl — subsidiebesluiten en verantwoording",
        handler: "Directie Rechtsbijstand en Rechtsstaat",
        lastModified: getTimestamp(19),
      },

      // Ministerie van Financiën (6 documenten)
      {
        id: "WOO-MFIN-2025-001",
        title:
          "Besluit op Woo-verzoek over beleidsstukken reorganisatie Financiën",
        status: "Ontvangen",
        submittedDate: "2025-01-08",
        organization: "Ministerie van Financiën",
        organizationType: "ministerie",
        category: "Organisatie",
        subject:
          "Gepubliceerd op open.overheid.nl — reorganisatieplannen en besluitvorming",
        requester: "FNV Overheid",
        handler: "Directie Bedrijfsvoering",
        lastModified: getTimestamp(18),
      },
      {
        id: "WOO-MFIN-2025-002",
        title: "Besluit op Woo-verzoek over nota's Belastingplan 2025",
        status: "In behandeling",
        submittedDate: "2025-02-12",
        organization: "Ministerie van Financiën",
        organizationType: "ministerie",
        category: "Belastingen",
        subject:
          "Gepubliceerd op open.overheid.nl — fiscale nota's en onderliggende berekeningen",
        requester: "Centraal Planbureau",
        handler: "Directie Fiscale Zaken",
        lastModified: getTimestamp(17),
      },
      {
        id: "WOO-MFIN-2025-003",
        title:
          "Besluit op Woo-verzoek over exportkredietverzekeringen staatsgaranties",
        status: "1e Concept",
        submittedDate: "2025-02-28",
        organization: "Ministerie van Financiën",
        organizationType: "ministerie",
        category: "Financiële markten",
        subject:
          "Gepubliceerd op open.overheid.nl — verstrekkingen en risicobeoordelingen",
        handler: "Directie Financiële Markten",
        lastModified: getTimestamp(16),
      },
      {
        id: "WOO-MFIN-2025-004",
        title:
          "Besluit op Woo-verzoek over onderzoeken toeslagenaffaire",
        status: "2e Concept",
        submittedDate: "2025-03-14",
        organization: "Ministerie van Financiën",
        organizationType: "ministerie",
        category: "Uitvoering",
        subject:
          "Gepubliceerd op open.overheid.nl — hersteloperaties en compensatieregelingen",
        requester: "Ombudsman voor Belastingzaken",
        handler: "Directie Uitvoering",
        lastModified: getTimestamp(15),
      },
      {
        id: "WOO-MFIN-2025-005",
        title: "Besluit op Woo-verzoek over kasbeheer Rijksschuld 2024",
        status: "Definitief",
        submittedDate: "2025-01-30",
        organization: "Ministerie van Financiën",
        organizationType: "ministerie",
        category: "Schuldenbeheer",
        subject:
          "Gepubliceerd op open.overheid.nl — emissies en rentestand rapportages",
        handler: "Directie Schuldenbeheer",
        lastModified: getTimestamp(14),
      },
      {
        id: "WOO-MFIN-2025-006",
        title:
          "Besluit op Woo-verzoek over groene fiscale stimuleringsmaatregelen",
        status: "Gepubliceerd",
        submittedDate: "2024-12-05",
        decidedDate: "2025-03-20",
        organization: "Ministerie van Financiën",
        organizationType: "ministerie",
        category: "Duurzaamheid",
        subject:
          "Gepubliceerd op open.overheid.nl — subsidieregelingen en evaluaties",
        handler: "Directie Duurzame Financiën",
        lastModified: getTimestamp(13),
      },

      // Provincie Zuid-Holland (6 documenten)
      {
        id: "WOO-ZH-2025-001",
        title:
          "Besluit op Woo-verzoek over Regionaal Mobiliteitsplan Zuidvleugel",
        status: "Ontvangen",
        submittedDate: "2025-01-20",
        organization: "Provincie Zuid-Holland",
        organizationType: "provincie",
        category: "Mobiliteit",
        subject:
          "Gepubliceerd op open.overheid.nl — vervoersvisie en MIRT-afstemming",
        requester: "Metropoolregio Rotterdam Den Haag",
        handler: "Afdeling Mobiliteit en Bereikbaarheid",
        lastModified: getTimestamp(12),
      },
      {
        id: "WOO-ZH-2025-002",
        title:
          "Besluit op Woo-verzoek over provinciale milieuverordening",
        status: "In behandeling",
        submittedDate: "2025-02-08",
        organization: "Provincie Zuid-Holland",
        organizationType: "provincie",
        category: "Milieu",
        subject:
          "Gepubliceerd op open.overheid.nl — ontwerpverordening en zienswijzen",
        requester: "Milieudefensie Zuid-Holland",
        handler: "Afdeling Leefomgeving",
        lastModified: getTimestamp(11),
      },
      {
        id: "WOO-ZH-2025-003",
        title:
          "Besluit op Woo-verzoek over PAS-melders en stikstofmaatregelen",
        status: "1e Concept",
        submittedDate: "2025-02-25",
        organization: "Provincie Zuid-Holland",
        organizationType: "provincie",
        category: "Natuur en landbouw",
        subject:
          "Gepubliceerd op open.overheid.nl — meldingen en legalisatiebeleid",
        handler: "Afdeling Landelijk Gebied",
        lastModified: getTimestamp(10),
      },
      {
        id: "WOO-ZH-2025-004",
        title:
          "Besluit op Woo-verzoek over huisvesting arbeidsmigranten",
        status: "2e Concept",
        submittedDate: "2025-03-10",
        organization: "Provincie Zuid-Holland",
        organizationType: "provincie",
        category: "Wonen",
        subject:
          "Gepubliceerd op open.overheid.nl — locatiebeleid en handhaving",
        requester: "Federatie Arbeidsmigranten",
        handler: "Afdeling Wonen",
        lastModified: getTimestamp(9),
      },
      {
        id: "WOO-ZH-2025-005",
        title:
          "Besluit op Woo-verzoek over waterveiligheid Hollandse Delta",
        status: "Definitief",
        submittedDate: "2025-01-15",
        organization: "Provincie Zuid-Holland",
        organizationType: "provincie",
        category: "Water",
        subject:
          "Gepubliceerd op open.overheid.nl — dijkversterking en beheerplannen",
        handler: "Afdeling Water en Klimaat",
        lastModified: getTimestamp(8),
      },
      {
        id: "WOO-ZH-2025-006",
        title:
          "Besluit op Woo-verzoek over woningbouw op provinciale gronden",
        status: "Gepubliceerd",
        submittedDate: "2024-10-22",
        decidedDate: "2025-02-14",
        organization: "Provincie Zuid-Holland",
        organizationType: "provincie",
        category: "Ruimtelijke ordening",
        subject:
          "Gepubliceerd op open.overheid.nl — grondexploitatie en verkoopbesluiten",
        handler: "Afdeling Grondbeleid",
        lastModified: getTimestamp(7),
      },

      // Provincie Flevoland (6 documenten)
      {
        id: "WOO-FLE-2025-001",
        title:
          "Besluit op Woo-verzoek over stikstofrapportage landbouw",
        status: "Ontvangen",
        submittedDate: "2025-01-18",
        organization: "Provincie Flevoland",
        organizationType: "provincie",
        category: "Landbouw en natuur",
        subject:
          "Gepubliceerd op open.overheid.nl — rapportages stikstofuitstoot landbouw",
        requester: "Natuur & Milieu",
        handler: "Afdeling Omgevingsbeleid",
        lastModified: getTimestamp(6),
      },
      {
        id: "WOO-FLE-2025-002",
        title:
          "Besluit op Woo-verzoek over windenergie Noordoostpolder",
        status: "In behandeling",
        submittedDate: "2025-02-05",
        organization: "Provincie Flevoland",
        organizationType: "provincie",
        category: "Energie",
        subject:
          "Gepubliceerd op open.overheid.nl — vergunningen en participatie",
        requester: "Bewonerscomité Noordoostpolder",
        handler: "Afdeling Energie",
        lastModified: getTimestamp(5),
      },
      {
        id: "WOO-FLE-2025-003",
        title: "Besluit op Woo-verzoek over reconstructie N23",
        status: "1e Concept",
        submittedDate: "2025-02-20",
        organization: "Provincie Flevoland",
        organizationType: "provincie",
        category: "Infrastructuur",
        subject:
          "Gepubliceerd op open.overheid.nl — projectplannen en MER",
        handler: "Afdeling Infrastructuur",
        lastModified: getTimestamp(4),
      },
      {
        id: "WOO-FLE-2025-004",
        title:
          "Besluit op Woo-verzoek over beheer Oostvaardersplassen",
        status: "2e Concept",
        submittedDate: "2025-03-01",
        organization: "Provincie Flevoland",
        organizationType: "provincie",
        category: "Natuur",
        subject:
          "Gepubliceerd op open.overheid.nl — beheerplan en populatiebeheer",
        requester: "Staatsbosbeheer",
        handler: "Afdeling Natuur",
        lastModified: getTimestamp(3),
      },
      {
        id: "WOO-FLE-2025-005",
        title:
          "Besluit op Woo-verzoek over economisch actieplan Flevoland",
        status: "Definitief",
        submittedDate: "2025-01-25",
        organization: "Provincie Flevoland",
        organizationType: "provincie",
        category: "Economie",
        subject:
          "Gepubliceerd op open.overheid.nl — investeringsagenda en subsidies",
        handler: "Afdeling Economie",
        lastModified: getTimestamp(2),
      },
      {
        id: "WOO-FLE-2025-006",
        title: "Besluit op Woo-verzoek over toerismevisie Flevoland 2030",
        status: "Gepubliceerd",
        submittedDate: "2024-11-15",
        decidedDate: "2025-03-08",
        organization: "Provincie Flevoland",
        organizationType: "provincie",
        category: "Toerisme",
        subject:
          "Gepubliceerd op open.overheid.nl — strategische toerismeontwikkeling",
        handler: "Afdeling Economie",
        lastModified: getTimestamp(1),
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
      request.previousStatus = request.status;
      request.status = status;
      request.lastModified = new Date().toISOString();
      this.requests.set(id, request);
      console.log(
        `[Mock Database] Updated document ${id}: ${request.previousStatus} -> ${status}`,
      );
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
