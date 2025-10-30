# Erlang Backend Quick Start Guide

Deze guide helpt je om snel aan de slag te gaan met de Erlang/OTP backend van het WOO Dashboard.

## 🎯 Wat is dit?

Het WOO Dashboard ondersteunt **twee backends**:

1. **Mock Backend** (standaard) - TypeScript in-memory database, geen server nodig
2. **Erlang Backend** - Echte Erlang/OTP applicatie met gen_server, supervisors en REST API

## 🚀 Quick Start

### Stap 1: Installeer Erlang & Rebar3

**macOS:**
```bash
brew install erlang rebar3
```

**Ubuntu/Debian:**
```bash
sudo apt-get update
sudo apt-get install erlang rebar3
```

**Verificatie:**
```bash
erl -version
rebar3 version
```

### Stap 2: Start Erlang Backend

Open een **nieuwe terminal** en voer uit:

```bash
cd erlang-backend
rebar3 get-deps      # Download dependencies (Cowboy, jsx)
rebar3 compile       # Compile Erlang code
rebar3 shell         # Start Erlang shell met applicatie
```

Je zou moeten zien:
```
=== Starting WOO Backend Application ===
Starting WOO Document Manager
Initializing WOO Supervisor
Starting WOO Event Handler
Starting WOO Simulation Server
WOO Backend started successfully
HTTP API listening on port 8080
```

**De backend draait nu op http://localhost:8080**

### Stap 3: Start Frontend

Open een **tweede terminal** en voer uit:

```bash
cd woo-dashboard
npm install          # Als je dit nog niet gedaan hebt
npm run dev
```

Open browser: **http://localhost:5173/woo-dashboard/**

### Stap 4: Switch naar Erlang Backend

In de browser:

1. Kijk naar de rechterbovenhoek van de header
2. Je ziet twee buttons: **💾 Mock** en **⚡ Erlang**
3. Klik op **⚡ Erlang**
4. De groene indicator (●) toont dat backend beschikbaar is
5. Pagina wordt automatisch herladen
6. Je gebruikt nu de echte Erlang backend!

## ✅ Verificatie

### Test 1: Health Check

```bash
curl http://localhost:8080/api/health
```

Verwachte output:
```json
{
  "status": "healthy",
  "simulation_running": false,
  "total_documents": 8,
  "timestamp": "2024-10-30T14:30:00Z"
}
```

### Test 2: Alle Documenten

```bash
curl http://localhost:8080/api/documents
```

Je zou 8 documenten moeten zien (4 Utrecht, 4 Flevoland).

### Test 3: Start Simulatie

```bash
curl -X POST http://localhost:8080/api/simulation/start
```

In de Erlang shell zie je nu elke 2 seconden status updates!

### Test 4: Update Document Status

```bash
curl -X POST http://localhost:8080/api/documents/WOO-UTR-2024-001/status \
  -H "Content-Type: application/json" \
  -d '{"status": "In behandeling"}'
```

## 🎮 Erlang Shell Commands

Terwijl de backend draait, kun je in de Erlang shell commando's uitvoeren:

```erlang
%% Check welke processen draaien
whereis(woo_document_manager).
whereis(woo_event_manager).
whereis(woo_simulation_server).

%% Haal alle documenten op
woo_document_manager:get_all_documents().

%% Update een document
woo_document_manager:update_status(<<"WOO-UTR-2024-001">>, <<"Definitief">>).

%% Start simulatie
woo_simulation_server:start_simulation().

%% Stop simulatie
woo_simulation_server:stop_simulation().

%% Check of simulatie draait
woo_simulation_server:is_running().

%% Statistieken
woo_document_manager:get_statistics().

%% Open Observer (grafische monitor)
observer:start().

%% Stop applicatie
application:stop(woo_backend).

%% Herstart applicatie
application:start(woo_backend).

%% Exit Erlang shell
q().
```

## 🏗️ Architectuur Overzicht

```
┌─────────────────────────────────────┐
│      React Frontend (Vite)          │
│   http://localhost:5173             │
└──────────────┬──────────────────────┘
               │ REST API calls
               ▼
┌─────────────────────────────────────┐
│   Erlang Backend (Cowboy)           │
│   http://localhost:8080/api         │
│                                     │
│  ┌──────────────────────────────┐  │
│  │  woo_backend_app             │  │
│  │  (Application Behavior)       │  │
│  └──────────┬───────────────────┘  │
│             │                       │
│  ┌──────────▼───────────────────┐  │
│  │  woo_sup (Supervisor)         │  │
│  │  Strategy: one_for_one        │  │
│  └──┬───────┬──────────┬─────────┘  │
│     │       │          │            │
│  ┌──▼───┐ ┌▼────┐  ┌──▼────┐       │
│  │Gen   │ │Gen  │  │Gen    │       │
│  │Server│ │Event│  │Server │       │
│  │      │ │     │  │       │       │
│  │Doc   │ │Event│  │Sim    │       │
│  │Mgr   │ │Mgr  │  │Server │       │
│  └──┬───┘ └─────┘  └───────┘       │
│     │                               │
│  ┌──▼──────────┐                    │
│  │  ETS Table  │                    │
│  │  (woo_docs) │                    │
│  └─────────────┘                    │
└─────────────────────────────────────┘
```

### Components

- **woo_backend_app** - OTP Application, start/stop lifecycle
- **woo_sup** - Supervisor, restart crashed processes
- **woo_document_manager** - Gen_Server, document CRUD operations
- **woo_event_manager** - Gen_Event, event pub/sub systeem
- **woo_simulation_server** - Gen_Server, automatische status progressie
- **woo_http_handler** - Cowboy HTTP handler, REST API endpoints
- **ETS Table** - In-memory database (microsecond latency)

## 📡 API Endpoints

### Documents

| Method | Endpoint | Beschrijving |
|--------|----------|--------------|
| GET | `/api/documents` | Alle documenten |
| GET | `/api/documents/:id` | Specifiek document |
| POST | `/api/documents/:id/status` | Update status |

### Statistics

| Method | Endpoint | Beschrijving |
|--------|----------|--------------|
| GET | `/api/statistics` | Algemene statistieken |
| GET | `/api/statistics/utrecht` | Utrecht stats |
| GET | `/api/statistics/flevoland` | Flevoland stats |

### Simulation

| Method | Endpoint | Beschrijving |
|--------|----------|--------------|
| POST | `/api/simulation/start` | Start simulatie |
| POST | `/api/simulation/stop` | Stop simulatie |

### Health

| Method | Endpoint | Beschrijving |
|--------|----------|--------------|
| GET | `/api/health` | Health check |

## 🐛 Troubleshooting

### Port 8080 al in gebruik

```bash
# Vind process op port 8080
lsof -i :8080

# Kill het process
kill -9 <PID>
```

### Erlang shell crashed

Geen probleem! Supervisor herstart automatisch alle processen:

```erlang
%% Check supervisor status
sys:get_status(woo_sup).

%% Check of children draaien
supervisor:which_children(woo_sup).
```

### Dependencies niet gevonden

```bash
cd erlang-backend
rebar3 clean
rebar3 get-deps
rebar3 compile
```

### CORS errors in browser

De backend heeft CORS enabled voor alle origins. Check of:
- Backend echt draait op port 8080
- Browser console voor exacte error
- Firewall instellingen

### Frontend kan backend niet bereiken

```bash
# Test of backend reageert
curl http://localhost:8080/api/health

# Check of beide servers draaien
ps aux | grep beam      # Erlang backend
ps aux | grep vite      # Frontend dev server
```

## 🎓 Leren van de Code

### Bekijk Gen_Server Implementation

```bash
cat erlang-backend/apps/woo_backend/src/woo_document_manager.erl
```

Zie hoe:
- `init/1` - Initialisatie met ETS table
- `handle_call/3` - Synchrone requests (get, update)
- `handle_cast/2` - Asynchrone messages
- `handle_info/2` - Timer messages, etc.

### Bekijk Supervisor Tree

```bash
cat erlang-backend/apps/woo_backend/src/woo_sup.erl
```

Zie hoe:
- Child specs gedefinieerd zijn
- Restart strategy werkt
- Intensity/period ingesteld zijn

### Monitor met Observer

In Erlang shell:

```erlang
observer:start().
```

Navigeer door:
- **System** tab - Memory, CPU usage
- **Load Charts** - Real-time grafieken
- **Processes** - Alle Erlang processen
- **Table Viewer** - ETS tables inspecteren
- **Trace Overview** - Message tracing

## 🚀 Production Deployment

### Build Release

```bash
cd erlang-backend
rebar3 as prod release
```

### Start Release

```bash
_build/prod/rel/woo_backend/bin/woo_backend foreground
```

Of als daemon:

```bash
_build/prod/rel/woo_backend/bin/woo_backend start
_build/prod/rel/woo_backend/bin/woo_backend ping      # Check status
_build/prod/rel/woo_backend/bin/woo_backend stop      # Stop
```

## 📚 Meer Informatie

- **Erlang Backend README**: `erlang-backend/README.md`
- **Main README**: `README.md`
- **Erlang/OTP Docs**: https://www.erlang.org/docs
- **Cowboy Docs**: https://ninenines.eu/docs/en/cowboy/2.10/guide/
- **Learn You Some Erlang**: https://learnyousomeerlang.com/

## 💡 Tips

1. **Development**: Gebruik `rebar3 shell` voor hot code reloading
2. **Debugging**: Gebruik `io:format/2` voor print debugging
3. **Monitoring**: Houd Observer open tijdens development
4. **Testing**: Gebruik `curl` om API endpoints te testen
5. **Performance**: ETS queries zijn microsecond-level snel

## ✨ Features van Erlang Backend

- ✅ Fault Tolerance - Crashes worden opgevangen en hersteld
- ✅ Concurrency - Duizenden simultane requests
- ✅ Hot Code Reloading - Update zonder downtime
- ✅ Distributed - Kan over meerdere nodes draaien
- ✅ Battle Tested - Erlang/OTP gebruikt door telecom industry
- ✅ Let It Crash - Philosophy zorgt voor robuuste systemen

Veel plezier met de Erlang backend! 🎉
