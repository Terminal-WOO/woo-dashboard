# WOO Dashboard - Utrecht & Flevoland

Een interactief dashboard voor het monitoren van Wet Open Overheid (WOO) verzoeken van **Gemeente Utrecht** en **Provincie Flevoland** met keuze tussen mock database, Erlang/OTP backend en PostgreSQL backend met moderne database features.

## Live Demo

Het dashboard is live beschikbaar op: **https://terminal-woo.github.io/woo-dashboard/**

> **Note**: GitHub Pages gebruikt CDN caching. Het kan 10-60 minuten duren voordat updates zichtbaar zijn. Voor directe updates: gebruik incognito mode of hard refresh (Ctrl+Shift+R).

## Overzicht

WOO Dashboard is een modern React-based dashboard dat statistieken en status updates van WOO-verzoeken visualiseert. Het systeem combineert **drie backend opties** met **volledige Document Management Systems** en **event streaming**:

### Backend Opties:
1. **Mock Backend**: Pure JavaScript in-memory database (TypeScript)
2. **Erlang Backend**: Echte Erlang/OTP applicatie met gen_server, supervisors en REST API
3. **PostgreSQL Backend**: Modern database systeem met LISTEN/NOTIFY, JSONB, full-text search en actor model

### Document Management:
4. **Paperless-ngx DMS**: Lichtgewicht open-source document management met OCR en tagging
5. **Alfresco DMS**: Enterprise ECM platform met workflow management
6. **DMS Simulator**: Interactieve document generator met realistische PDFs (6 types)

### Event Streaming:
7. **NATS JetStream**: Event streaming platform met 7-dagen persistente opslag
8. **Event Consumer API**: REST API voor event querying en statistieken
9. **Event Stream Viewer**: Real-time dashboard component voor live event monitoring

### Belangrijkste Functies

#### WOO Dashboard Core:
- **🔄 Triple Backend Architecture**: Switch tussen mock, Erlang en PostgreSQL backend via UI
- **⚡ Erlang/OTP Backend**: Volledige OTP applicatie met gen_server, gen_event, supervisor tree
- **💾 Mock Database**: Pure JavaScript in-memory database met 24 realistische documenten
- **🎭 Erlang Actor System**: Fault-tolerant event handling met supervisors en message passing
- **📊 6-Stage Workflow**: Lineaire status progressie met cyclische herstart
  - Ontvangen → In behandeling → 1e Concept → 2e Concept → Definitief → Gepubliceerd
- **⏱️ Real-time Simulatie**: Automatische doorloop van document statussen (2 seconden interval)
- **📡 Live Event Feed**: Real-time notificaties van alle status wijzigingen via Erlang actors
- **🥧 Detailed Status Visualization**: Pie chart toont alle 6 workflow statussen met unieke kleuren
- **🏛️ Organisatie Filtering**: Gescheiden data voor gemeente (12 documenten) en provincie (12 documenten)

#### Document Management Systems:
- **📄 Paperless-ngx**: Lichtgewicht DMS met PostgreSQL 18 + MinIO S3 storage
- **🏢 Alfresco**: Enterprise ECM met Digital Workspace en Share UI
- **🎭 DMS Simulator**: Upload 1-50 realistische PDFs (besluit, advies, brief, notitie, rapportage, contract)
- **📦 MinIO Storage**: S3-compatible lokale object storage (NIET AWS cloud!)
- **🗄️ PostgreSQL Metadata**: Automatische tracking van buckets, objects, en statistieken
- **🔄 Multi-DMS Upload**: Upload tegelijk naar Paperless én Alfresco

#### Event Streaming:
- **📡 NATS JetStream**: Persistent event storage met 7-dagen retentie
- **🔍 Event Stream Viewer**: Real-time dashboard component met filters en statistieken
- **📊 Event Analytics**: Totaal events, per systeem, gemiddelde upload tijd
- **🎯 Complete Audit Trail**: Alle document.uploaded, document.updated, document.deleted events
- **🌐 REST API**: Event Consumer API op port 3002 met SSE support

#### Developer Experience:
- **🚀 Automated Startup**: `start-all.sh` script voor complete systeem
- **🔍 Verification Tool**: `verify-system.sh` voor health checks
- **📚 Comprehensive Docs**: COMPLETE_SYSTEM_GUIDE.md met alle features
- **🐳 Docker Compose**: Alles draait in containers (PostgreSQL 18, MinIO, NATS, DMS)
- **📱 Responsive Design**: Werkt op desktop, tablet en mobile

## Technologie Stack

### Frontend
- **React 18** - UI framework met hooks (useState, useEffect)
- **TypeScript** - Type-safe development met strikte typing
- **Recharts** - Data visualisatie voor bar charts en pie charts
- **Vite** - Build tool en development server
- **CSS3** - Styling met custom properties en responsive grid

### Backend Opties

#### 1. Mock Backend (TypeScript)
- **In-Memory Database** - Pure JavaScript Map-based storage
- **Erlang-Inspired Actors** - Actor model simulatie in TypeScript
- **24 Sample Documents** - Pre-loaded realistic data
- **Client-Side Only** - No server required

#### 2. Erlang Backend (OTP)
- **Erlang/OTP 24+** - Production-grade runtime
- **Cowboy Web Server** - HTTP/REST API server
- **ETS Database** - In-memory Erlang Term Storage
- **Gen_Server** - Document management (woo_document_manager)
- **Gen_Event** - Event notification system (woo_event_manager)
- **Supervisor** - Fault-tolerant process supervision
- **Rebar3** - Build tool en dependency management

#### 3. PostgreSQL Backend (Modern DB)
- **PostgreSQL 14+** - Production-grade relational database
- **Node.js/Fastify** - Modern REST API server
- **LISTEN/NOTIFY** - Real-time pub/sub for event streaming
- **JSONB** - Flexible JSON storage with indexing
- **Full-text Search** - Dutch language text search with tsvector
- **Materialized Views** - Fast aggregated statistics
- **Triggers** - Automatic audit trails and notifications
- **Actor System** - Erlang-inspired actors backed by PostgreSQL

## Database Schema

Het systeem gebruikt een **in-memory mock database** (src/mockDatabase.ts) met Map-based storage:

### Data Structure

```typescript
interface WOORequest {
  id: string;                    // Document ID (bijv. WOO-UTR-2024-001)
  title: string;                 // Titel van het verzoek
  status: WOOStatus;             // Huidige status (6 mogelijke waarden)
  submittedDate: string;         // Datum van indiening (ISO format)
  organization: string;          // Gemeente Utrecht / Provincie Flevoland
  organizationType: OrganizationType;  // 'gemeente' | 'provincie'
  category: string;              // Categorie (bijv. Ruimtelijke ordening)
  subject: string;               // Onderwerp omschrijving
  requester: string;             // Indiener naam
  handler: string;               // Behandelaar/afdeling
  decidedDate?: string;          // Datum van besluit (optioneel)
  lastModified: string;          // Laatste wijziging timestamp
}
```

### Status Types

```typescript
type WOOStatus = 
  | "Ontvangen"           // 🟠 Oranje - Document binnengekomen
  | "In behandeling"      // 🔵 Blauw - Wordt beoordeeld
  | "1e Concept"          // 🟣 Paars - Eerste conceptversie
  | "2e Concept"          // 🔴 Roze - Tweede conceptversie
  | "Definitief"          // 🔷 Cyaan - Definitieve versie
  | "Gepubliceerd";       // 🟢 Groen - Openbaar gemaakt
```

### Database Operations

De mock database service (`mockDatabaseService`) biedt:

```typescript
- getAll(): Promise<WOORequest[]>                    // Alle documenten ophalen
- getById(id: string): Promise<WOORequest | null>    // Specifiek document ophalen
- getByOrganization(org: string): Promise<WOORequest[]>  // Filter op organisatie
- getByStatus(status: WOOStatus): Promise<WOORequest[]>  // Filter op status
- update(id: string, newStatus: WOOStatus): Promise<void>  // Status update
- getStatistics(): Promise<Statistics>               // Statistieken berekenen
```

### Initial Data: 24 Documents

Het systeem bevat 24 realistische documenten, verdeeld over beide organisaties en alle statussen:

#### Gemeente Utrecht (12 documenten)

- **Ontvangen** (2): Nieuwbouwproject Merwedekanaal, Duurzaamheidsplan gemeentelijke gebouwen
- **In behandeling** (2): Verkeersplan binnenstad, Subsidieregeling maatschappelijke initiatieven
- **1e Concept** (2): Subsidieverlening culturele instellingen, Nota parkeernormen 2024
- **2e Concept** (2): Omgevingsvisie Utrecht 2040, Herinrichtingsplan Vredenburg
- **Definitief** (2): Contracten afvalverwerking, Beleidsnota jeugdhulpverlening
- **Gepubliceerd** (2): Aanbesteding openbaar vervoer, Gemeentelijke begroting 2024

#### Provincie Flevoland (12 documenten)

- **Ontvangen** (2): Subsidieregeling duurzame landbouw, Provinciaal waterplan 2025
- **In behandeling** (2): Windmolenpark Noordoostpolder, Subsidie natuurbeheer landgoederen
- **1e Concept** (2): Reconstructie N23 wegverbreding, Visie recreatie en toerisme
- **2e Concept** (2): Stikstofrapportage landbouw, Ontwikkelplan bedrijventerrein Lelystad Airport
- **Definitief** (2): Natuurontwikkeling Oostvaardersplassen, Omgevingsvergunning zonnepark Zeewolde
- **Gepubliceerd** (2): N23 reconstructie projectplan, Energietransitie roadmap 2030

Elke document bevat realistische categorieën zoals:
- Ruimtelijke ordening
- Milieu en duurzaamheid
- Verkeer en vervoer
- Subsidies en financiën
- Cultuur en onderwijs
- Natuur en landschap

## Projectstructuur

```
woo-dashboard/
├── src/
│   ├── components/
│   │   ├── StatsCard.tsx        # Statistiek cards
│   │   ├── RequestsTable.tsx    # Documenten tabel met status badges
│   │   └── ActivityFeed.tsx     # Live event feed (Erlang actor subscriber)
│   ├── App.tsx                  # Hoofd applicatie component
│   ├── App.css                  # Applicatie styling met status colors
│   ├── types.ts                 # TypeScript type definities
│   ├── data.ts                  # Data utility functies
│   │   ├── calculateStats()            # Bereken dashboard statistieken
│   │   ├── getDetailedStatusDistribution()  # Pie chart data (alle 6 statussen)
│   │   ├── getMonthlyData()            # Maandelijkse trend data
│   │   └── getOrganizationStats()      # Per-organisatie statistieken
│   ├── erlangActorSystem.ts    # Complete Erlang/OTP implementatie
│   │   ├── Actor                       # Process met PID, mailbox, message handler
│   │   ├── Supervisor                  # OTP supervisor met restart strategies
│   │   ├── EventManager                # Gen_event voor pub/sub
│   │   ├── ProcessRegistry             # Named process registration
│   │   └── Application                 # Application controller
│   ├── erlangSimulatorV2.ts    # V2 met mock database integratie
│   │   ├── initialize()                # Setup database en actors
│   │   ├── start()                     # Start simulatie met interval
│   │   ├── stop()                      # Stop simulatie
│   │   └── getStatistics()             # Ophalen statistieken
│   ├── mockDatabase.ts         # In-memory database met 24 documenten
│   │   └── mockDatabaseService         # CRUD operations en queries
│   ├── databaseActor.ts        # Legacy SQLite actor (deprecated)
│   ├── erlangSimulator.ts      # V1 simulator (deprecated)
│   ├── eventSystem.ts          # Legacy event management (deprecated)
│   ├── statusSimulator.ts      # Legacy status simulator (deprecated)
│   ├── documentGenerator.ts    # Document generator utilities
│   └── main.tsx                # App entry point
├── .github/
│   └── workflows/
│       └── deploy.yml          # GitHub Actions automatic deployment
├── index.html
├── package.json
├── tsconfig.json
├── vite.config.ts              # Base path: /woo-dashboard/
└── README.md
```

## Architectuur

### Erlang-Inspired Actor System

Dit project implementeert een **Erlang/OTP-geïnspireerd Actor Model** voor robuuste event handling en concurrency. Erlang is wereldberoemd voor zijn betrouwbaarheid in telecom systemen en distributed systems.

#### Waarom Erlang patronen?

Erlang's filosofie ("let it crash", actor model, supervision trees) biedt:

- **Isolatie**: Elke actor heeft zijn eigen state, geen gedeeld geheugen
- **Message Passing**: Actors communiceren via immutable messages
- **Fault Tolerance**: Supervisors kunnen gefaalde actors herstarten
- **Concurrency**: Actors verwerken messages asynchroon maar sequentieel
- **No Race Conditions**: Sequential mailbox processing voorkomt race conditions

#### Core Components

**1. Actor System (src/erlangActorSystem.ts)**

Implementeert Erlang concepten in TypeScript:

```typescript
// Actor: Equivalent van Erlang process met PID, mailbox en message handler
class Actor {
  private pid: PID;                      // Unieke process identifier (bijv. <0.42.0>)
  private mailbox: Message[] = [];       // Message queue
  private isProcessing = false;          // Processing lock
  private state: any = {};               // Actor state
  
  send(message: Message): void           // Stuur bericht naar mailbox
  private processMailbox(): Promise<void>  // Verwerk berichten sequentieel
}

// Supervisor: OTP Supervisor pattern voor fault tolerance
class Supervisor {
  private children: Map<PID, Actor>;
  private strategy: RestartStrategy;     // permanent | temporary | transient
  
  startChild(childSpec): Promise<PID>
  restartChild(pid: PID): Promise<void>
  stopChild(pid: PID): Promise<void>
}

// EventManager: Gen_event behavior voor pub/sub
class EventManager {
  private handlers: Map<string, Actor>;
  private eventHistory: Event[] = [];
  
  addHandler(name: string, actor: Actor): void
  notify(event: Event): Promise<void>    // Broadcast naar alle handlers
}

// ProcessRegistry: Named process registration
class ProcessRegistry {
  private registry: Map<string, PID>;
  
  register(name: string, pid: PID): void
  whereis(name: string): PID | undefined
}
```

**2. Erlang Simulator V2 (src/erlangSimulatorV2.ts)**

Document lifecycle management met actors en mock database integratie:

```typescript
const ORGANIZATIONS: Organization[] = [
  {
    id: "gemeente-utrecht",
    name: "Gemeente Utrecht",
    type: "gemeente",
    statusWorkflow: [
      "Ontvangen",
      "In behandeling",
      "1e Concept",
      "2e Concept",
      "Definitief",
      "Gepubliceerd",
    ],
  },
  {
    id: "provincie-flevoland",
    name: "Provincie Flevoland",
    type: "provincie",
    statusWorkflow: [
      "Ontvangen",
      "In behandeling",
      "1e Concept",
      "2e Concept",
      "Definitief",
      "Gepubliceerd",
    ],
  },
];
```

**Actors in het systeem:**

- **Ticker Actor**: Timer proces dat periodieke tick messages stuurt (2 seconden interval)
- **Document Manager Actor**: GenServer-style state management voor document updates
- **Event Publisher Actor**: Gen_event voor event distribution naar subscribers

**Status Progressie:**

1. Start met 24 documenten verdeeld over alle statussen
2. Elke 2 seconden: selecteer willekeurig document dat nog niet "Gepubliceerd" is
3. Move document naar volgende status in workflow
4. Broadcast status change event naar alle subscribers
5. Als alle documenten "Gepubliceerd" zijn: reset 1 willekeurig document naar "Ontvangen"

**3. Activity Feed met Actors (src/components/ActivityFeed.tsx)**

```typescript
useEffect(() => {
  // Spawn subscriber actor bij component mount
  const subscriberActor = application.spawn(async (message) => {
    if (message.type === "status_change") {
      setEvents((prev) => [...message.data, ...prev].slice(0, 10));
    }
  });
  
  // Registreer bij EventManager
  eventManager.addHandler("activity_feed", subscriberActor);
  
  // Cleanup actor bij unmount
  return () => {
    application.kill(subscriberActor.getPid());
  };
}, []);
```

#### Message Types

Erlang-style message tuples (als TypeScript types):

```typescript
type Message =
  | { type: 'status_change', data: StatusChangeData }
  | { type: 'new_document', data: DocumentData }
  | { type: 'tick', data: { timestamp: number } }
  | { type: 'subscribe', data: { callback: Function } }
  | { type: 'shutdown' }
  | { type: 'restart' }
```

#### Voordelen van deze Architectuur

1. **Testbaarheid**: Actors zijn geïsoleerd en makkelijk te testen
2. **Debugbaarheid**: Message flow is traceable en inspectable
3. **Schaalbaarheid**: Easy toe te voegen nieuwe actors voor nieuwe features
4. **Betrouwbaarheid**: Supervisors vangen crashes op en herstarten actors
5. **Clean Code**: Separation of concerns via message passing
6. **No Race Conditions**: Sequential message processing in actors
7. **Fault Tolerance**: "Let it crash" philosophy met automatic recovery

## Erlang Backend Architectuur

Het project bevat een **volledige Erlang/OTP backend** implementatie in `erlang-backend/`:

### OTP Application Structure

```
erlang-backend/
├── apps/woo_backend/src/
│   ├── woo_backend_app.erl          # Application behavior
│   ├── woo_sup.erl                  # Top-level supervisor
│   ├── woo_document_manager.erl     # Gen_Server (documents)
│   ├── woo_event_manager.erl        # Gen_Event (events)
│   ├── woo_simulation_server.erl    # Gen_Server (simulation)
│   └── woo_http_handler.erl         # Cowboy HTTP handler
├── config/
│   ├── sys.config                   # System configuration
│   └── vm.args                      # VM arguments
└── rebar.config                     # Dependencies & build config
```

### OTP Behaviors Gebruikt

1. **Application** (woo_backend_app)
   - Lifecycle management
   - Start supervisor en HTTP server

2. **Supervisor** (woo_sup)
   - Strategy: `one_for_one`
   - Restart crashed processes automatisch
   - Intensity: 10 restarts / 60 seconden

3. **Gen_Server** (woo_document_manager)
   - Stateful document management
   - ETS table voor snelle storage
   - Synchrone calls voor CRUD operations

4. **Gen_Event** (woo_event_manager)
   - Event notification systeem
   - Pub/sub voor status changes
   - Event history buffer (50 events)

5. **Gen_Server** (woo_simulation_server)
   - Automatische status progressie
   - Timer-based ticks (2 seconden)
   - Start/stop API

### REST API Endpoints

De Erlang backend biedt een volledige REST API:

**Documents**
- `GET /api/documents` - Alle documenten
- `GET /api/documents/:id` - Specifiek document
- `POST /api/documents/:id/status` - Update status
  ```json
  {"status": "In behandeling"}
  ```

**Statistics**
- `GET /api/statistics` - Algemene stats
- `GET /api/statistics/utrecht` - Gemeente Utrecht stats
- `GET /api/statistics/flevoland` - Provincie Flevoland stats

**Simulation**
- `POST /api/simulation/start` - Start simulatie
- `POST /api/simulation/stop` - Stop simulatie

**Health**
- `GET /api/health` - Health check + simulation status

**Voorbeeld Response:**
```json
{
  "id": "WOO-UTR-2024-001",
  "title": "Informatie over nieuwbouwproject Merwedekanaal",
  "status": "In behandeling",
  "organization": "Gemeente Utrecht",
  "organizationType": "gemeente",
  "category": "Ruimtelijke ordening",
  "lastModified": "2024-10-30T14:23:15Z"
}
```

### Erlang Backend Features

- ✅ **Fault Tolerance** - Supervisor tree restart crashes
- ✅ **Concurrency** - Handles thousands of concurrent requests
- ✅ **Hot Code Reloading** - Update code zonder downtime
- ✅ **ETS Storage** - Microsecond read/write latency
- ✅ **CORS Support** - Frontend kan backend aanroepen
- ✅ **JSON API** - jsx library voor encoding/decoding
- ✅ **Cowboy Web Server** - Production-ready HTTP server

### Frontend Integration

De frontend gebruikt `erlangBackendService.ts` om met de backend te communiceren:

```typescript
// Fetch alle documenten
const docs = await erlangBackendService.getAll();

// Update document status
await erlangBackendService.update("WOO-UTR-2024-001", "Definitief");

// Start simulatie op backend
await erlangBackendService.startSimulation();

// Health check
const health = await erlangBackendService.healthCheck();
```

De `backendService.ts` abstraction layer maakt switching mogelijk:

```typescript
// Switch naar Erlang backend
backendService.switchBackend("erlang");

// Check of backend beschikbaar is
const available = await backendService.checkErlangBackendAvailable();
```

Zie `erlang-backend/README.md` voor volledige documentatie.

## PostgreSQL Backend Architectuur

De PostgreSQL backend combineert Erlang actor model patronen met moderne PostgreSQL features voor een robuuste, schaalbare oplossing.

### Actor System met PostgreSQL

Het actor systeem is volledig geïntegreerd met PostgreSQL:

```typescript
// Actor backed by PostgreSQL connection pool
class Actor extends EventEmitter {
  private pool: Pool;                   // PostgreSQL connection pool
  private listenClient?: PoolClient;    // Dedicated client for LISTEN
  
  async send(message: Message): Promise<any> {
    // Process message in transaction
    const client = await this.pool.connect();
    await client.query('BEGIN');
    const result = await this.behavior(this.state, message);
    await client.query('COMMIT');
    client.release();
    return result;
  }
  
  async listen(channel: string): Promise<void> {
    // Subscribe to PostgreSQL NOTIFY
    this.listenClient = await this.pool.connect();
    await this.listenClient.query(`LISTEN ${channel}`);
    this.listenClient.on('notification', (msg) => {
      this.emit('postgres_notify', {
        channel: msg.channel,
        payload: JSON.parse(msg.payload)
      });
    });
  }
}
```

### Modern PostgreSQL Features

**1. LISTEN/NOTIFY voor Real-time Events**

```sql
-- Trigger sends NOTIFY on status change
CREATE TRIGGER woo_request_status_change
  AFTER UPDATE ON woo_requests
  FOR EACH ROW
  WHEN (OLD.status IS DISTINCT FROM NEW.status)
  EXECUTE FUNCTION notify_status_change();

-- Function sends notification
CREATE FUNCTION notify_status_change() RETURNS trigger AS $$
BEGIN
  PERFORM pg_notify('status_change', 
    json_build_object(
      'document_id', NEW.id,
      'old_status', OLD.status,
      'new_status', NEW.status
    )::text
  );
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;
```

**2. Full-text Search (Nederlands)**

```sql
-- Auto-generated search vector
search_vector tsvector GENERATED ALWAYS AS (
  setweight(to_tsvector('dutch', coalesce(title, '')), 'A') ||
  setweight(to_tsvector('dutch', coalesce(subject, '')), 'B')
) STORED

-- GIN index voor snelle search
CREATE INDEX idx_woo_requests_search ON woo_requests USING GIN(search_vector);

-- Search query
SELECT *, ts_rank(search_vector, query) as rank
FROM woo_requests, 
     websearch_to_tsquery('dutch', 'bestemmingsplan') query
WHERE search_vector @@ query
ORDER BY rank DESC;
```

**3. JSONB voor Flexible Metadata**

```sql
-- JSONB column met GIN index
metadata JSONB NOT NULL DEFAULT '{}'::jsonb,

CREATE INDEX idx_woo_requests_metadata ON woo_requests USING GIN(metadata);

-- Query JSONB
SELECT * FROM woo_requests 
WHERE metadata @> '{"theme": "ruimtelijke_ordening"}'::jsonb;
```

**4. Materialized Views voor Statistics**

```sql
CREATE MATERIALIZED VIEW statistics_summary AS
SELECT 
  status,
  COUNT(*) as count,
  AVG(EXTRACT(EPOCH FROM (updated_at - created_at)) / 86400) as avg_days_in_status
FROM woo_requests
GROUP BY status;

-- Refresh when needed
REFRESH MATERIALIZED VIEW statistics_summary;
```

### PostgreSQL Backend Features

- ✅ **ACID Transactions** - Guaranteed data consistency
- ✅ **Real-time LISTEN/NOTIFY** - Push-based event streaming
- ✅ **Full-text Search** - Dutch language support with tsvector
- ✅ **JSONB Indexing** - Fast flexible schema queries
- ✅ **Materialized Views** - Pre-computed statistics
- ✅ **Automatic Audit Trail** - status_history table via triggers
- ✅ **Actor Model** - Erlang-inspired patterns with PostgreSQL backing
- ✅ **Connection Pooling** - Efficient resource management
- ✅ **Concurrent Access** - Row-level locking

### Frontend Integration

```typescript
// Fetch alle documenten
const docs = await postgresBackendService.getAll();

// Full-text search
const results = await postgresBackendService.search("bestemmingsplan");

// Update status (triggers NOTIFY)
await postgresBackendService.update("1", "Definitief");

// Start simulatie
await postgresBackendService.startSimulation();

// Health check
const available = await postgresBackendService.checkHealth();
```

Zie `postgres-backend/README.md` voor volledige documentatie.

### Pie Chart Visualization

De pie chart (src/App.tsx) gebruikt `getDetailedStatusDistribution()` om alle 6 workflow statussen weer te geven:

```typescript
// Oude implementatie (3 statussen):
const statusDistribution = getStatusDistribution(stats);
// Resultaat: Ontvangen, In Behandeling, Gepubliceerd

// Nieuwe implementatie (6 statussen):
const statusDistribution = getDetailedStatusDistribution(requests);
// Resultaat: Alle 6 individuele statussen met kleuren
```

Output in pie chart:
- 🟠 **Ontvangen** (#d97706) - 4 documenten (16.7%)
- 🔵 **In behandeling** (#107abe) - 4 documenten (16.7%)
- 🟣 **1e Concept** (#9333ea) - 4 documenten (16.7%)
- 🔴 **2e Concept** (#db2777) - 4 documenten (16.7%)
- 🔷 **Definitief** (#0891b2) - 4 documenten (16.7%)
- 🟢 **Gepubliceerd** (#16a34a) - 4 documenten (16.7%)

## Lokale Ontwikkeling

### Vereisten

**Frontend:**
- Node.js 20 of hoger
- npm

**Erlang Backend (optioneel):**
- Erlang/OTP 24 of hoger
- Rebar3

### Quick Start - Mock Backend (Standaard)

```bash
# Clone de repository
git clone https://github.com/YOUR_USERNAME/woo-dashboard.git
cd woo-dashboard

# Installeer dependencies
npm install

# Start development server
npm run dev
```

De applicatie is nu beschikbaar op `http://localhost:5173/woo-dashboard/`

Het dashboard start standaard met de **Mock Backend** (TypeScript in-memory database).

### Quick Start - Erlang Backend

Voor het gebruik van de echte Erlang/OTP backend:

**1. Installeer Erlang/OTP en Rebar3**

```bash
# macOS
brew install erlang rebar3

# Ubuntu/Debian
sudo apt-get update
sudo apt-get install erlang rebar3
```

**2. Start de Erlang backend**

Open een nieuwe terminal:

```bash
cd erlang-backend
rebar3 get-deps
rebar3 compile
rebar3 shell
```

De backend luistert nu op `http://localhost:8080`

**3. Start de frontend**

In een andere terminal:

```bash
cd woo-dashboard
npm run dev
```

**4. Switch naar Erlang backend**

- Open `http://localhost:5173/woo-dashboard/` in browser
- Klik op **"Erlang"** button in de rechterbovenhoek van de header
- De groene indicator toont dat de backend beschikbaar is
- Pagina wordt automatisch herladen met Erlang backend

### Quick Start - PostgreSQL Backend

Voor het gebruik van de PostgreSQL backend met moderne database features:

**1. Installeer PostgreSQL**

```bash
# macOS
brew install postgresql@14
brew services start postgresql@14

# Ubuntu/Debian
sudo apt-get update
sudo apt-get install postgresql-14
sudo systemctl start postgresql
```

**2. Create database**

```bash
createdb woo_dashboard
```

**3. Configureer environment**

```bash
cd postgres-backend
cp .env.example .env
# Edit .env met je PostgreSQL credentials
```

**4. Start de PostgreSQL backend**

```bash
cd postgres-backend
npm install
npm run dev
```

De backend luistert nu op `http://localhost:8081`

**5. Switch naar PostgreSQL backend**

- Open `http://localhost:5173/woo-dashboard/` in browser
- Klik op **"🐘 PostgreSQL"** button in de header
- De groene indicator toont dat de backend beschikbaar is
- Pagina wordt automatisch herladen met PostgreSQL backend

### Backend Switching

Het dashboard biedt een **Backend Switcher** component in de header:

- **💾 Mock** - TypeScript in-memory database (geen server vereist)
- **⚡ Erlang** - Echte Erlang/OTP backend (localhost:8080 vereist)
- **🐘 PostgreSQL** - PostgreSQL database met modern features (localhost:8081 vereist)

De status indicator toont:
- 🟢 **Groen** - Backend beschikbaar
- 🔴 **Rood** - Backend offline
- ⚪ **Grijs** - Status onbekend/checking

De backend keuze wordt opgeslagen in localStorage.

### Build voor productie

```bash
npm run build
```

De production build wordt geplaatst in de `dist/` directory.

Build output:
- `dist/index.html` - Entry point
- `dist/assets/index-*.css` - Compiled styles (~7 KB)
- `dist/assets/index-*.js` - Compiled JavaScript (~568 KB)

### Preview productie build

```bash
npm run preview
```

## Deployment naar GitHub Pages

De app wordt automatisch gedeployed naar GitHub Pages bij elke push naar de master branch via GitHub Actions.

### Handmatige deployment

```bash
npm run deploy
```

Dit commando:
1. Bouwt de productie versie (`npm run build`)
2. Deployt naar de gh-pages branch
3. Publiceert naar GitHub Pages

### GitHub Pages Configuratie

In de repository settings:

1. Ga naar **Settings** → **Pages**
2. Selecteer **Deploy from a branch**
3. Branch: **gh-pages**
4. Folder: **/ (root)**

### GitHub Actions Workflow

Het project gebruikt GitHub Actions voor automatische deployment (`.github/workflows/deploy.yml`):

```yaml
name: Deploy to GitHub Pages

on:
  push:
    branches: [ master ]

jobs:
  build-and-deploy:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      - uses: actions/setup-node@v3
        with:
          node-version: '20'
      - run: npm ci
      - run: npm run build
      - uses: peaceiris/actions-gh-pages@v3
        with:
          github_token: ${{ secrets.GITHUB_TOKEN }}
          publish_dir: ./dist
```

## Gebruik

### Simulatie Starten/Stoppen

Klik op de **"Start Simulatie"** knop in de rechterbovenhoek om:
- Automatische status updates te activeren
- Documents door de workflow te laten bewegen
- Live events te genereren in de activity feed

De simulatie update interval is **2 seconden** voor soepele progressie.

**Simulatie gedrag:**
1. Selecteert willekeurig een document dat nog niet "Gepubliceerd" is
2. Verplaatst het naar de volgende status in de workflow
3. Als alle documenten "Gepubliceerd" zijn: reset 1 willekeurig document naar "Ontvangen"
4. Broadcast event naar Activity Feed subscribers

### Dashboard Secties

**1. Statistieken Cards** (boven)

- 📊 **Totaal Verzoeken**: Totaal aantal documenten in database (24)
- 📥 **Ontvangen**: Aantal documenten in "Ontvangen" status
- ⏳ **In Behandeling**: Som van "In behandeling", "1e Concept", "2e Concept", "Definitief"
- ✅ **Afgerond**: Aantal documenten in "Gepubliceerd" status

**2. Grafieken** (midden)

- **Verzoeken per Maand** (Bar Chart)
  - Mock data voor maandelijkse trend
  - X-as: Maanden (Jan - Okt)
  - Y-as: Aantal verzoeken
  
- **Status Verdeling** (Pie Chart)
  - Toont alle 6 workflow statussen
  - Kleurgecodeerd per status type
  - Percentages berekend real-time
  - Label format: "Status: XX%"

**3. Documenten Tabel** (links beneden)

Kolommen:
- **ID**: Document identifier (bijv. WOO-UTR-2024-001)
- **Titel**: Document titel
- **Organisatie**: 🏛️ Gemeente Utrecht / 🗺️ Provincie Flevoland
- **Categorie**: Document categorie
- **Status**: Kleurgecodeerde badge met huidige status
- **Ingediend**: Indiening datum
- **Behandelaar**: Afdeling/persoon

**4. Activity Feed** (rechts beneden)

- Real-time event notificaties via Erlang actor subscription
- Toont laatste 10 events
- Event types:
  - Status wijzigingen met oude → nieuwe status
  - Kleuren matchen status badges
  - Organisatie icon (🏛️ / 🗺️)
- Automatische scroll naar nieuwe events

## Scripts

```json
{
  "dev": "vite",                    // Start development server
  "build": "tsc && vite build",     // TypeScript compile + Vite build
  "preview": "vite preview",        // Preview productie build lokaal
  "predeploy": "npm run build",     // Pre-deployment hook (automatic)
  "deploy": "gh-pages -d dist"      // Deploy dist/ naar gh-pages branch
}
```

## Type Definities

Het project gebruikt strikte TypeScript types (src/types.ts):

```typescript
// Status types met 6 workflow stages
export type WOOStatus = 
  | "Ontvangen" 
  | "In behandeling"
  | "1e Concept"
  | "2e Concept"
  | "Definitief"
  | "Gepubliceerd";

// Organisatie types
export type OrganizationType = "gemeente" | "provincie";

// Hoofd document interface
export interface WOORequest {
  id: string;
  title: string;
  status: WOOStatus;
  submittedDate: string;
  organization: string;
  organizationType: OrganizationType;
  category: string;
  subject: string;
  requester: string;
  handler: string;
  decidedDate?: string;
  lastModified: string;
}

// Statistieken interface
export interface WOOStats {
  totalRequests: number;
  received: number;
  inProgress: number;
  completed: number;
  averageHandlingDays: number;
}

// Event interface voor activity feed
export interface Event {
  id: string;
  timestamp: Date;
  type: string;
  documentId: string;
  message: string;
  oldStatus?: WOOStatus;
  newStatus?: WOOStatus;
  organization: string;
  organizationType: OrganizationType;
}

// Chart data interfaces
export interface MonthlyData {
  month: string;
  requests: number;
}

export interface StatusDistribution {
  name: string;
  value: number;
  color: string;
}

// Organisatie interface
export interface Organization {
  id: string;
  name: string;
  type: OrganizationType;
  statusWorkflow: WOOStatus[];
}
```

## CSS Styling

Status badge kleuren (src/App.css):

```css
/* Ontvangen - Oranje */
.status-received {
  background-color: #fef3c7;
  color: #d97706;
}

/* In behandeling - Blauw */
.status-in-progress {
  background-color: #dbeafe;
  color: #107abe;
}

/* 1e Concept - Paars */
.status-concept-1 {
  background-color: #f3e8ff;
  color: #9333ea;
}

/* 2e Concept - Roze */
.status-concept-2 {
  background-color: #fce7f3;
  color: #db2777;
}

/* Definitief - Cyaan */
.status-definitive {
  background-color: #cffafe;
  color: #0891b2;
}

/* Gepubliceerd - Groen */
.status-published {
  background-color: #dcfce7;
  color: #16a34a;
}
```

## Browser Support

- Chrome/Edge (laatste 2 versies)
- Firefox (laatste 2 versies)
- Safari (laatste 2 versies)

Vereist:
- ES2020+ support
- JavaScript enabled
- localStorage (voor potentiële toekomstige features)

## Contributing

Contributions zijn welkom! Volg deze stappen:

1. Fork de repository
2. Maak een feature branch (`git checkout -b feature/nieuwe-functie`)
3. Commit je wijzigingen (`git commit -m 'Voeg nieuwe functie toe'`)
4. Push naar de branch (`git push origin feature/nieuwe-functie`)
5. Open een Pull Request

### Development Guidelines

- Gebruik TypeScript voor alle nieuwe code
- Volg de bestaande code style
- Test lokaal met `npm run dev` voordat je pushed
- Build succesvol met `npm run build`
- Documenteer nieuwe features in README.md
- Update types.ts bij nieuwe interfaces

## License

Dit project is open source en beschikbaar onder de MIT License.

## Contact

Voor vragen of suggesties, open een issue op GitHub.

## Technische Details

### Bundle Size

Production build:
- HTML: ~0.86 KB (gzip: 0.42 KB)
- CSS: ~7.08 KB (gzip: 2.06 KB)
- JavaScript: ~568 KB (gzip: 161 KB)

Total: ~576 KB (~164 KB gzipped)

### Performance

- First Contentful Paint: < 1s
- Time to Interactive: < 2s
- Lighthouse Score: 90+

### Vite Configuration

```typescript
// vite.config.ts
export default defineConfig({
  plugins: [react()],
  base: '/woo-dashboard/',  // GitHub Pages subdirectory
})
```

## Changelog

## 📄 Document Management Systems (DMS)

Het WOO Dashboard is uitgebreid met **volledige Document Management Systems** voor archivering en beheer van WOO documenten:

### Beschikbare DMS Systemen

**1. Paperless-ngx** (Lichtgewicht - Aanbevolen)
- Open-source document management voor kleine tot middelgrote organisaties
- PostgreSQL 18 database met MinIO metadata tracking
- Automatische OCR en tagging
- MinIO S3-compatible storage (lokaal, geen AWS cloud!)
- Web UI op http://localhost:8000

**2. Alfresco Community Edition** (Enterprise)
- Enterprise-grade ECM/DMS platform voor grote organisaties
- Volledige workflow management en compliance features
- Digital Workspace en Share UI
- Vereist 4GB+ RAM, ~10 minuten startup tijd
- Web UI op http://localhost:8080

### DMS Simulator

Interactieve component in het dashboard voor het uploaden van test documenten:

**Features:**
- 📊 Variabel aantal documenten (1-50) via slider
- 📝 Realistische PDF generatie met PDFKit
- 🏛️ 6 documenttypes: besluit, advies, brief, notitie, rapportage, contract
- 🔄 Real-time progress tracking
- 🎯 Upload naar Paperless, Alfresco, of beide systemen

**Gebruik:**
1. Start het DMS systeem (zie hieronder)
2. Open dashboard op http://localhost:5173
3. Scroll naar "Document Management Simulator"
4. Stel aantal documenten in
5. Selecteer doelsysteem(en)
6. Klik "Start Simulatie"

### NATS JetStream Event Streaming

Alle document uploads worden geregistreerd in een **NATS JetStream** event systeem:

**Features:**
- 📡 Persistent event storage (7 dagen retentie)
- 🔍 Complete audit trail van alle document operaties
- 📊 Real-time event stream viewer in dashboard
- 🎯 Event types: document.uploaded, document.updated, document.deleted
- 📈 Statistieken: totaal events, per systeem, gemiddelde upload tijd

**Event Schema:**
```typescript
{
  eventId: "evt-1234-xyz",
  eventType: "document.uploaded",
  timestamp: "2024-11-09T10:30:00Z",
  system: "paperless",
  document: {
    id: "doc-123",
    title: "Besluit gemeenteraad 2024-01",
    type: "besluit",
    category: "Bestuur",
    size: 45678
  },
  metadata: {
    simulationId: "sim-456",
    uploadDuration: 1234,
    source: "dms-simulator"
  }
}
```

### MinIO Storage met PostgreSQL Tracking

Beide DMS systemen gebruiken **MinIO** voor object storage met volledige metadata tracking in PostgreSQL:

**Features:**
- 🪣 Automatische bucket statistieken
- 📦 Object metadata tracking (size, tags, upload time)
- 📊 SQL views voor analytics (v_bucket_overview, v_recent_uploads)
- 🔍 Query capabilities via PostgreSQL

**Voorbeeld Query:**
```sql
SELECT * FROM minio_metadata.v_bucket_overview;
SELECT * FROM minio_metadata.v_recent_uploads LIMIT 10;
```

### Snelstart DMS Systemen

**Volledig systeem starten:**
```bash
./start-all.sh
```

Dit script start in volgorde:
1. NATS JetStream (event streaming)
2. DMS keuze (Paperless, Alfresco, of beide)
3. DMS Simulator (document upload service)
4. WOO Dashboard (frontend)

**Alleen NATS + Paperless:**
```bash
# Start NATS
cd nats-events && docker-compose up -d && cd ..

# Start Paperless
cd paperless-ngx-dms && docker-compose up -d && cd ..

# Start DMS Simulator
cd dms-simulator && npm run dev &

# Start Dashboard
npm run dev
```

**Verificatie:**
```bash
./verify-system.sh
```

Controleert alle services en endpoints.

### Architectuur

```
Dashboard → DMS Simulator → [Paperless/Alfresco] → MinIO
              ↓
         NATS JetStream ← Event Consumer API
              ↓
      Event Stream Viewer (Dashboard Component)
```

### Documentatie

Uitgebreide documentatie per component:
- `COMPLETE_SYSTEM_GUIDE.md` - Volledige systeemgids met alle features
- `paperless-ngx-dms/README.md` - Paperless-ngx setup en configuratie
- `alfresco-dms/README.md` - Alfresco setup en vergelijking
- `nats-events/README.md` - NATS JetStream architectuur
- `nats-events/QUICKSTART.md` - NATS snelstart gids
- `dms-simulator/README.md` - Document generator details

---

## Changelog

### v2.2.0 (9 november 2024) - Document Management Edition

**🎉 Major New Features:**
- 📄 **Paperless-ngx DMS** - Lichtgewicht document management met MinIO + PostgreSQL 18
- 🏢 **Alfresco DMS** - Enterprise ECM platform voor grote organisaties
- 🎭 **DMS Simulator** - Interactieve document generator met realistische PDFs
- 📡 **NATS JetStream** - Event streaming platform voor complete audit trail
- 📊 **Event Stream Viewer** - Real-time dashboard component voor events
- 🗄️ **MinIO Metadata Tracking** - PostgreSQL schema voor object storage analytics

**DMS Features:**
- ✨ Twee volledige DMS opties (Paperless-ngx & Alfresco)
- 📝 Realistische PDF generatie (6 documenttypes)
- 🔄 Real-time upload progress tracking
- 🎯 Multi-systeem support (upload naar beide DMS tegelijk)
- 📡 NATS event publishing bij elke upload
- 🗄️ MinIO S3-compatible lokale storage (geen AWS!)
- 📊 PostgreSQL 18 met metadata tracking

**Event System Features:**
- 🎯 NATS JetStream met 7-dagen retentie
- 📡 Server-Sent Events voor live updates
- 📊 Statistieken dashboard (totaal, per systeem, gemiddelde tijd)
- 🔍 Event filtering en querying via REST API
- 💾 Persistent event storage met replay capabilities

**Developer Tools:**
- 🚀 `start-all.sh` - Complete systeem startup script
- 🔍 `verify-system.sh` - Service verification tool
- 📚 `COMPLETE_SYSTEM_GUIDE.md` - Uitgebreide documentatie
- 🐳 Docker Compose setups voor alle componenten

### v2.1.0 (30 oktober 2024) - Dual Backend Edition

**🎉 Major New Feature:**
- ⚡ **Echte Erlang/OTP Backend** - Volledige OTP applicatie met gen_server, supervisor, gen_event
- 🔄 **Dual Backend Architecture** - Switch tussen Mock en Erlang backend via UI
- 🌐 **REST API** - Complete RESTful API met Cowboy web server
- 🗄️ **ETS Database** - Production-grade Erlang Term Storage
- 💾 **Backend Switcher Component** - Real-time backend switching met status indicator

**Erlang Backend Features:**
- Gen_Server voor document management (woo_document_manager)
- Gen_Event voor event notifications (woo_event_manager)
- Gen_Server voor simulation (woo_simulation_server)
- Supervisor tree met one_for_one restart strategy
- CORS-enabled REST API (port 8080)
- Health check endpoint
- 8 initial documents (4 Utrecht, 4 Flevoland)

**Frontend Improvements:**
- 🔌 `erlangBackendService.ts` - Backend API client
- 🎛️ `backendService.ts` - Abstraction layer voor backend switching
- 🎨 `BackendSwitcher` component met status indicator
- 📦 localStorage persistence voor backend keuze
- ✨ v2.1 badge met "Multi-Backend" label

**Documentation:**
- 📚 Complete Erlang backend README (erlang-backend/README.md)
- 🔧 Setup instructies voor beide backends
- 📖 API documentatie met curl voorbeelden
- 🏗️ OTP architectuur uitleg
- 💡 Frontend integration examples

### v2.0.0 (29 oktober 2024)

**Breaking Changes:**
- Verwijderd SQL.js dependency (CORS/WASM issues)
- Vervangen door pure JavaScript mock database

**New Features:**
- ✨ Mock database met 24 realistische documenten
- ✨ Detailed pie chart met alle 6 workflow statussen
- ✨ Cyclische status progressie (auto-restart)
- ✨ 2-seconden simulatie interval
- ✨ Verbeterde kleuren voor alle statussen

**Improvements:**
- 🎨 Unieke kleuren voor elk status type in pie chart
- 📊 Betere data distributie (4 docs per status)
- 🚀 Snellere load time (geen WASM)
- 💾 Kleinere bundle size (562 KB vs 612 KB)
- 🔧 Simpelere maintenance (geen externe DB)

**Bug Fixes:**
- 🐛 Fixed GitHub Pages deployment CORS issues
- 🐛 Fixed status progression getting stuck
- 🐛 Fixed pie chart not showing all statuses

### v1.0.0 (Eerdere versie)

- Initiële release met SQL.js database
- Erlang Actor System implementatie (TypeScript simulatie)
- GitHub Pages deployment
- Basis simulatie systeem

---

**Gebouwd met ❤️ voor transparantie in de Nederlandse overheid**

**Tech Stack**: React 18 • TypeScript • Vite • Erlang/OTP Patterns • Recharts
