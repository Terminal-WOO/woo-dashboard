# WOO Backend - Erlang/OTP Implementation

Een echte Erlang/OTP backend implementatie voor het WOO Dashboard met gen_server, supervisor trees, gen_event en RESTful API.

## Architectuur

Deze backend implementeert klassieke Erlang/OTP design patterns:

### OTP Behaviors

1. **Application (woo_backend_app)**
   - OTP application behavior
   - Start/stop lifecycle management
   - Initialiseert supervisor tree en HTTP server

2. **Supervisor (woo_sup)**
   - OTP supervisor behavior
   - Strategy: `one_for_one` (restart only failed process)
   - Intensity: 10 restarts binnen 60 seconden
   - Managed children:
     - woo_document_manager (gen_server)
     - woo_event_manager (gen_event)
     - woo_simulation_server (gen_server)

3. **Gen_Server (woo_document_manager)**
   - Stateful document management
   - ETS table voor snelle document opslag
   - CRUD operations via synchrone calls
   - Notificeert event manager bij wijzigingen

4. **Gen_Event (woo_event_manager)**
   - Event notification systeem
   - Pub/sub pattern voor status changes
   - Event history buffer (laatste 50 events)
   - Multiple handlers support

5. **Gen_Server (woo_simulation_server)**
   - Automatische document status progressie
   - Timer-based ticks (2 seconden interval)
   - Cyclic workflow management
   - Start/stop API

### HTTP API (Cowboy)

RESTful API met JSON responses en CORS support:

#### Endpoints

**Documents**
- `GET /api/documents` - Alle documenten ophalen
- `GET /api/documents/:id` - Specifiek document ophalen
- `POST /api/documents/:id/status` - Status updaten

**Statistics**
- `GET /api/statistics` - Algemene statistieken
- `GET /api/statistics/:org` - Organisatie statistieken (utrecht/flevoland)

**Simulation**
- `POST /api/simulation/start` - Start simulatie
- `POST /api/simulation/stop` - Stop simulatie

**Health**
- `GET /api/health` - Health check + status

### Data Storage

**ETS (Erlang Term Storage)**
- In-memory key-value store
- Type: `set` (unieke keys)
- Read concurrency enabled
- Record-based schema (#document{})

### Sample Data

8 initiële documenten:
- 4x Gemeente Utrecht
- 4x Provincie Flevoland

Statussen: Ontvangen, In behandeling, 1e Concept, 2e Concept, Definitief, Gepubliceerd

## Vereisten

- Erlang/OTP 24 of hoger
- rebar3

### Installatie Erlang (macOS)

```bash
brew install erlang rebar3
```

### Installatie Erlang (Ubuntu/Debian)

```bash
sudo apt-get update
sudo apt-get install erlang rebar3
```

## Ontwikkeling

### Dependencies downloaden

```bash
cd erlang-backend
rebar3 get-deps
```

### Compileren

```bash
rebar3 compile
```

### Starten in development mode

```bash
rebar3 shell
```

Dit start de Erlang shell met de applicatie geladen.

### Handmatig starten vanuit shell

```erlang
%% Start de applicatie
application:start(woo_backend).

%% Check of processes draaien
whereis(woo_document_manager).
whereis(woo_event_manager).
whereis(woo_simulation_server).

%% Haal alle documenten op
woo_document_manager:get_all_documents().

%% Update een document status
woo_document_manager:update_status(<<"WOO-UTR-2024-001">>, <<"In behandeling">>).

%% Start simulatie
woo_simulation_server:start_simulation().

%% Stop simulatie
woo_simulation_server:stop_simulation().

%% Stop de applicatie
application:stop(woo_backend).
```

## Production Release

### Release bouwen

```bash
rebar3 as prod release
```

Dit creëert een standalone release in `_build/prod/rel/woo_backend/`

### Release starten

```bash
_build/prod/rel/woo_backend/bin/woo_backend foreground
```

Of als daemon:

```bash
_build/prod/rel/woo_backend/bin/woo_backend start
```

### Release stoppen

```bash
_build/prod/rel/woo_backend/bin/woo_backend stop
```

### Release status

```bash
_build/prod/rel/woo_backend/bin/woo_backend ping
```

## API Gebruik

### Curl Examples

```bash
# Health check
curl http://localhost:8080/api/health

# Alle documenten ophalen
curl http://localhost:8080/api/documents

# Specifiek document ophalen
curl http://localhost:8080/api/documents/WOO-UTR-2024-001

# Status updaten
curl -X POST http://localhost:8080/api/documents/WOO-UTR-2024-001/status \
  -H "Content-Type: application/json" \
  -d '{"status": "In behandeling"}'

# Statistieken ophalen
curl http://localhost:8080/api/statistics

# Organisatie statistieken
curl http://localhost:8080/api/statistics/utrecht
curl http://localhost:8080/api/statistics/flevoland

# Simulatie starten
curl -X POST http://localhost:8080/api/simulation/start

# Simulatie stoppen
curl -X POST http://localhost:8080/api/simulation/stop
```

### JavaScript/TypeScript Voorbeeld

```typescript
const API_BASE = 'http://localhost:8080/api';

// Fetch alle documenten
async function getAllDocuments() {
  const response = await fetch(`${API_BASE}/documents`);
  const data = await response.json();
  return data.documents;
}

// Update document status
async function updateStatus(docId: string, newStatus: string) {
  const response = await fetch(`${API_BASE}/documents/${docId}/status`, {
    method: 'POST',
    headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify({ status: newStatus })
  });
  return response.json();
}

// Start simulatie
async function startSimulation() {
  const response = await fetch(`${API_BASE}/simulation/start`, {
    method: 'POST'
  });
  return response.json();
}

// Stop simulatie
async function stopSimulation() {
  const response = await fetch(`${API_BASE}/simulation/stop`, {
    method: 'POST'
  });
  return response.json();
}

// Get statistieken
async function getStatistics() {
  const response = await fetch(`${API_BASE}/statistics`);
  return response.json();
}
```

## Project Structuur

```
erlang-backend/
├── rebar.config              # Rebar3 configuratie + dependencies
├── config/
│   ├── sys.config            # System configuratie
│   └── vm.args               # VM argumenten
├── apps/
│   └── woo_backend/
│       └── src/
│           ├── woo_backend.app.src          # App specification
│           ├── woo_backend_app.erl          # Application behavior
│           ├── woo_sup.erl                  # Supervisor
│           ├── woo_document_manager.erl     # Gen_Server (documents)
│           ├── woo_event_manager.erl        # Gen_Event (events)
│           ├── woo_simulation_server.erl    # Gen_Server (simulation)
│           └── woo_http_handler.erl         # Cowboy HTTP handler
└── README.md
```

## Logging

Logs worden geschreven naar:
- `log/console.log` - Algemene logs
- `log/sasl-error.log` - SASL error logs
- `log/sasl/` - SASL crash reports

## Configuratie

### HTTP Port aanpassen

Edit `config/sys.config`:

```erlang
{woo_backend, [
    {http_port, 9090},  % Verander naar gewenste poort
    {max_documents, 1000}
]}
```

### VM Memory aanpassen

Edit `config/vm.args`:

```
## Increase number of concurrent ports/sockets
-env ERL_MAX_PORTS 8192
```

## Monitoring & Debugging

### Observer GUI starten

Vanuit `rebar3 shell`:

```erlang
observer:start().
```

Dit opent een grafische interface met:
- Process list en supervisor tree
- Memory usage
- CPU load
- ETS table browser
- Message queue lengths

### Process Info

```erlang
%% Supervisor info
sys:get_status(woo_sup).

%% Document Manager info
sys:get_status(woo_document_manager).

%% Check mailbox size
{message_queue_len, Len} = process_info(whereis(woo_document_manager), message_queue_len).

%% Check state
sys:get_state(woo_document_manager).
```

### ETS Table inspectie

```erlang
%% Alle tables
ets:all().

%% Table info
ets:info(woo_documents).

%% Browse table in Observer
observer:start().
%% Ga naar "Table Viewer" tab
```

## Testing

### Unit Tests (TODO)

```bash
rebar3 eunit
```

### Common Tests (TODO)

```bash
rebar3 ct
```

### Manual Testing Checklist

- [ ] Start backend: `rebar3 shell`
- [ ] Check health: `curl http://localhost:8080/api/health`
- [ ] Fetch documents: `curl http://localhost:8080/api/documents`
- [ ] Update status: POST to `/api/documents/:id/status`
- [ ] Start simulation: POST to `/api/simulation/start`
- [ ] Verify simulation updates documents
- [ ] Stop simulation: POST to `/api/simulation/stop`
- [ ] Check statistics: `curl http://localhost:8080/api/statistics`

## OTP Design Patterns

### Let It Crash Philosophy

De supervisor herstart automatisch gefaalde processen:

```erlang
%% Als woo_document_manager crashed:
%% 1. Supervisor detecteert exit signal
%% 2. Supervisor restart het process
%% 3. ETS table wordt opnieuw aangemaakt
%% 4. Sample data wordt opnieuw geladen
%% 5. System blijft beschikbaar
```

### Message Passing

Alle communicatie via messages (geen shared state):

```erlang
%% Synchronous call (get_all_documents)
gen_server:call(woo_document_manager, get_all_documents)
  -> woo_document_manager ! {call, From, get_all_documents}
  -> handle_call(get_all_documents, From, State)
  -> {reply, Documents, State}

%% Asynchronous cast (initialize_data)
gen_server:cast(woo_document_manager, initialize_data)
  -> woo_document_manager ! {cast, initialize_data}
  -> handle_cast(initialize_data, State)
  -> {noreply, State}
```

### State Management

Gen_server encapsuleert state:

```erlang
-record(state, {
    table :: ets:tid()
}).

%% State transitions via handle_call/handle_cast return values:
{reply, Reply, NewState}     % Synchronous with reply
{noreply, NewState}          % Asynchronous
{stop, Reason, State}        % Terminate process
```

## Troubleshooting

### Port 8080 al in gebruik

```bash
# Vind process op poort 8080
lsof -i :8080

# Kill het process
kill -9 <PID>

# Of verander poort in config/sys.config
```

### Dependencies niet gevonden

```bash
rebar3 clean
rebar3 get-deps
rebar3 compile
```

### ETS table already exists error

Dit betekent dat een oude instantie nog draait:

```erlang
%% In Erlang shell
ets:delete(woo_documents).

%% Of herstart de shell
q().
```

### Supervisor restart loop

Check de logs in `log/sasl-error.log` voor crash reports.

Meestal door:
- Configuratie fouten
- Missing dependencies
- Port binding failures

## Performance

### Benchmarks (indicatief)

- Document lookup: < 1ms (ETS)
- Status update: < 5ms (ETS + event notification)
- Full document list: < 10ms (100 documenten)
- HTTP request overhead: ~2-5ms

### Scaling

- Verticaal: ETS schaalt tot gigabytes
- Horizontaal: Distributed Erlang (meerdere nodes)
- Concurrency: Thousands of concurrent connections

## Toekomstige Verbeteringen

- [ ] Mnesia database (persistent storage)
- [ ] WebSocket support voor real-time events
- [ ] JWT authenticatie
- [ ] Rate limiting
- [ ] Metrics export (Prometheus)
- [ ] Distributed Erlang cluster
- [ ] Hot code reloading
- [ ] Full test suite (EUnit + Common Test)

## License

MIT

## Contact

Voor vragen over de Erlang implementatie, zie de hoofdproject README.
