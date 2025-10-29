/**
 * Database Actor - Erlang-style SQLite database management
 * Implements gen_server pattern for database operations
 */

import initSqlJs, { Database } from "sql.js";
import { WOORequest, WOOStatus, OrganizationType } from "./types";

export type DatabaseMessage =
  | { type: "init" }
  | { type: "insert"; data: { request: WOORequest } }
  | { type: "update"; data: { id: string; status: WOOStatus } }
  | { type: "query_all"; data: { callback: (requests: WOORequest[]) => void } }
  | {
      type: "query_by_org";
      data: {
        organization: string;
        callback: (requests: WOORequest[]) => void;
      };
    }
  | { type: "export"; data: { callback: (data: Uint8Array) => void } }
  | { type: "import"; data: { data: Uint8Array } };

class DatabaseService {
  private db: Database | null = null;
  private initialized = false;

  async init(): Promise<void> {
    if (this.initialized) return;

    console.log("[Database Actor] Initializing SQLite database...");

    const SQL = await initSqlJs({
      locateFile: (file) => `https://sql.js.org/dist/${file}`,
    });

    this.db = new SQL.Database();

    // Create schema
    this.db.run(`
      CREATE TABLE IF NOT EXISTS organizations (
        id TEXT PRIMARY KEY,
        name TEXT NOT NULL,
        type TEXT NOT NULL,
        status_workflow TEXT NOT NULL
      );
    `);

    this.db.run(`
      CREATE TABLE IF NOT EXISTS woo_requests (
        id TEXT PRIMARY KEY,
        title TEXT NOT NULL,
        status TEXT NOT NULL,
        submitted_date TEXT NOT NULL,
        decided_date TEXT,
        organization TEXT NOT NULL,
        organization_type TEXT NOT NULL,
        category TEXT NOT NULL,
        subject TEXT NOT NULL,
        requester TEXT,
        handler TEXT,
        last_modified TEXT NOT NULL,
        FOREIGN KEY (organization) REFERENCES organizations(name)
      );
    `);

    this.db.run(`
      CREATE TABLE IF NOT EXISTS status_history (
        id INTEGER PRIMARY KEY AUTOINCREMENT,
        request_id TEXT NOT NULL,
        old_status TEXT,
        new_status TEXT NOT NULL,
        changed_at TEXT NOT NULL,
        changed_by TEXT,
        FOREIGN KEY (request_id) REFERENCES woo_requests(id)
      );
    `);

    // Insert organizations
    this.insertOrganizations();

    // Insert initial data
    this.insertInitialData();

    this.initialized = true;
    console.log(
      "[Database Actor] Database initialized with Utrecht and Flevoland data",
    );
  }

  private insertOrganizations(): void {
    if (!this.db) return;

    const organizations = [
      {
        id: "gemeente-utrecht",
        name: "Gemeente Utrecht",
        type: "gemeente",
        workflow: JSON.stringify([
          "Ontvangen",
          "In behandeling",
          "1e Concept",
          "In behandeling",
          "2e Concept",
          "In behandeling",
          "Definitief",
          "In behandeling",
          "Gepubliceerd",
        ]),
      },
      {
        id: "provincie-flevoland",
        name: "Provincie Flevoland",
        type: "provincie",
        workflow: JSON.stringify([
          "Ontvangen",
          "In behandeling",
          "1e Concept",
          "In behandeling",
          "2e Concept",
          "In behandeling",
          "Definitief",
          "In behandeling",
          "Gepubliceerd",
        ]),
      },
    ];

    for (const org of organizations) {
      this.db.run(
        `INSERT OR REPLACE INTO organizations (id, name, type, status_workflow)
         VALUES (?, ?, ?, ?)`,
        [org.id, org.name, org.type, org.workflow],
      );
    }
  }

  private insertInitialData(): void {
    if (!this.db) return;

    const initialRequests = [
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

    for (const req of initialRequests) {
      this.db.run(
        `INSERT OR REPLACE INTO woo_requests
         (id, title, status, submitted_date, decided_date, organization,
          organization_type, category, subject, requester, handler, last_modified)
         VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)`,
        [
          req.id,
          req.title,
          req.status,
          req.submittedDate,
          req.decidedDate || null,
          req.organization,
          req.organizationType,
          req.category,
          req.subject,
          req.requester || null,
          req.handler || null,
          req.lastModified,
        ],
      );

      // Add to status history
      this.db.run(
        `INSERT INTO status_history (request_id, new_status, changed_at)
         VALUES (?, ?, ?)`,
        [req.id, req.status, req.lastModified],
      );
    }
  }

  insert(request: WOORequest): void {
    if (!this.db) return;

    this.db.run(
      `INSERT INTO woo_requests
       (id, title, status, submitted_date, decided_date, organization,
        organization_type, category, subject, requester, handler, last_modified)
       VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)`,
      [
        request.id,
        request.title,
        request.status,
        request.submittedDate,
        request.decidedDate || null,
        request.organization,
        request.organizationType,
        request.category,
        request.subject,
        request.requester || null,
        request.handler || null,
        request.lastModified,
      ],
    );

    // Add to status history
    this.db.run(
      `INSERT INTO status_history (request_id, new_status, changed_at)
       VALUES (?, ?, ?)`,
      [request.id, request.status, new Date().toISOString()],
    );

    console.log(`[Database Actor] Inserted document: ${request.id}`);
  }

  update(id: string, status: WOOStatus): void {
    if (!this.db) return;

    // Get old status
    const result = this.db.exec(
      `SELECT status FROM woo_requests WHERE id = ?`,
      [id],
    );

    const oldStatus =
      result.length > 0 ? (result[0].values[0][0] as string) : null;

    // Update status
    this.db.run(
      `UPDATE woo_requests
       SET status = ?, last_modified = ?
       WHERE id = ?`,
      [status, new Date().toISOString(), id],
    );

    // Add to status history
    this.db.run(
      `INSERT INTO status_history (request_id, old_status, new_status, changed_at)
       VALUES (?, ?, ?, ?)`,
      [id, oldStatus, status, new Date().toISOString()],
    );

    console.log(
      `[Database Actor] Updated document ${id}: ${oldStatus} -> ${status}`,
    );
  }

  queryAll(): WOORequest[] {
    if (!this.db) return [];

    const result = this.db.exec(
      `SELECT id, title, status, submitted_date, decided_date, organization,
              organization_type, category, subject, requester, handler, last_modified
       FROM woo_requests
       ORDER BY last_modified DESC`,
    );

    if (result.length === 0) return [];

    return result[0].values.map((row) => ({
      id: row[0] as string,
      title: row[1] as string,
      status: row[2] as WOOStatus,
      submittedDate: row[3] as string,
      decidedDate: row[4] as string | undefined,
      organization: row[5] as string,
      organizationType: row[6] as OrganizationType,
      category: row[7] as string,
      subject: row[8] as string,
      requester: row[9] as string | undefined,
      handler: row[10] as string | undefined,
      lastModified: row[11] as string,
    }));
  }

  queryByOrganization(organization: string): WOORequest[] {
    if (!this.db) return [];

    const result = this.db.exec(
      `SELECT id, title, status, submitted_date, decided_date, organization,
              organization_type, category, subject, requester, handler, last_modified
       FROM woo_requests
       WHERE organization = ?
       ORDER BY last_modified DESC`,
      [organization],
    );

    if (result.length === 0) return [];

    return result[0].values.map((row) => ({
      id: row[0] as string,
      title: row[1] as string,
      status: row[2] as WOOStatus,
      submittedDate: row[3] as string,
      decidedDate: row[4] as string | undefined,
      organization: row[5] as string,
      organizationType: row[6] as OrganizationType,
      category: row[7] as string,
      subject: row[8] as string,
      requester: row[9] as string | undefined,
      handler: row[10] as string | undefined,
      lastModified: row[11] as string,
    }));
  }

  export(): Uint8Array | null {
    if (!this.db) return null;
    return this.db.export();
  }

  import(_data: Uint8Array): void {
    // This would reinitialize the database with imported data
    console.log("[Database Actor] Import not yet implemented");
  }

  getStatistics(): {
    total: number;
    byOrg: Record<string, number>;
    byStatus: Record<string, number>;
  } {
    if (!this.db) return { total: 0, byOrg: {}, byStatus: {} };

    const totalResult = this.db.exec(`SELECT COUNT(*) FROM woo_requests`);
    const total = (totalResult[0]?.values[0][0] as number) || 0;

    const byOrgResult = this.db.exec(
      `SELECT organization, COUNT(*) as count
       FROM woo_requests
       GROUP BY organization`,
    );
    const byOrg: Record<string, number> = {};
    if (byOrgResult.length > 0) {
      byOrgResult[0].values.forEach((row) => {
        byOrg[row[0] as string] = row[1] as number;
      });
    }

    const byStatusResult = this.db.exec(
      `SELECT status, COUNT(*) as count
       FROM woo_requests
       GROUP BY status`,
    );
    const byStatus: Record<string, number> = {};
    if (byStatusResult.length > 0) {
      byStatusResult[0].values.forEach((row) => {
        byStatus[row[0] as string] = row[1] as number;
      });
    }

    return { total, byOrg, byStatus };
  }
}

// Singleton instance
export const databaseService = new DatabaseService();
