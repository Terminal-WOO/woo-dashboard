# NATS JetStream Event System voor Document Management

Een event-driven architecture voor het tracken van alle document operaties in Paperless-ngx en Alfresco DMS systemen.

## Architectuur Overzicht

```
┌──────────────────────┐
│  DMS Simulator       │
│  (Document Upload)   │
└──────────┬───────────┘
           │
           │ Publish Events
           ▼
┌──────────────────────────────────┐
│    NATS JetStream Server         │
│                                  │
│  Stream: DOCUMENT_EVENTS         │
│  Subjects: document.*            │
│  - document.uploaded             │
│  - document.updated              │
│  - document.deleted              │
│                                  │
│  Retention: 7 days               │
│  Storage: File (persistent)      │
└──────────┬───────────────────────┘
           │
           │ Subscribe
           ▼
┌──────────────────────┐      ┌──────────────────┐
│  Event Consumer      │─────►│  WOO Dashboard   │
│  (Fastify API)       │ HTTP │  (Event Viewer)  │
│                      │      │                  │
│  - Stores events     │      │  - Real-time UI  │
│  - Provides API      │      │  - Statistics    │
│  - Statistics        │      │  - Filtering     │
└──────────────────────┘      └──────────────────┘
```

## Waarom NATS JetStream?

### Voordelen

1. **Event Sourcing**: Complete audit trail van alle document operaties
2. **Decoupling**: DMS systemen zijn onafhankelijk van consumers
3. **Replay**: Events kunnen opnieuw afgespeeld worden
4. **Persistence**: Events blijven 7 dagen beschikbaar
5. **Performance**: Hoge doorvoer, lage latency
6. **Schaalbaar**: Makkelijk horizontaal te schalen
7. **At-Least-Once Delivery**: Garantie dat events niet verloren gaan

### Gebruik Cases

- **Audit Logging**: Wie heeft wat wanneer geüpload
- **Analytics**: Document upload trends en patronen
- **Monitoring**: Real-time inzicht in DMS activiteit
- **Integration**: Trigger externe systemen bij document events
- **Compliance**: Volledige traceability voor WOO verzoeken
- **Debugging**: Analyseer wat er gebeurd is bij problemen

## Event Schema

Elk document event heeft de volgende structuur:

```typescript
interface DocumentEvent {
  // Event identificatie
  eventId: string;              // Uniek event ID
  eventType: 'document.uploaded' | 'document.updated' | 'document.deleted';
  timestamp: string;             // ISO 8601 timestamp
  
  // DMS systeem
  system: 'paperless' | 'alfresco';
  
  // Document metadata
  document: {
    id: string;                  // DMS document ID
    title: string;               // Document titel
    type: string;                // besluit, advies, brief, etc.
    category: string;            // Bouwvergunningen, Belastingen, etc.
    author: string;              // Auteur naam
    date: string;                // Document datum (ISO 8601)
    tags: string[];              // Tags array
    size: number;                // Bestandsgrootte in bytes
    contentType: string;         // MIME type
  };
  
  // Extra metadata
  metadata: {
    simulationId?: string;       // ID van simulatie run
    uploadDuration?: number;     // Upload tijd in ms
    source: string;              // Bron: 'dms-simulator', 'manual', etc.
  };
}
```

### Voorbeeld Event

```json
{
  "eventId": "evt-1704123456789-abc123",
  "eventType": "document.uploaded",
  "timestamp": "2024-01-15T10:30:45.123Z",
  "system": "paperless",
  "document": {
    "id": "1234",
    "title": "Besluit Bouwvergunningen 2024-01-15",
    "type": "besluit",
    "category": "Bouwvergunningen",
    "author": "Jan de Vries",
    "date": "2024-01-15T00:00:00.000Z",
    "tags": ["openbaar", "definitief", "vergunning"],
    "size": 45678,
    "contentType": "application/pdf"
  },
  "metadata": {
    "simulationId": "sim-1704123456789",
    "uploadDuration": 1234,
    "source": "dms-simulator"
  }
}
```

## NATS Stream Configuratie

### Stream: DOCUMENT_EVENTS

```yaml
Name: DOCUMENT_EVENTS
Subjects: document.*
Retention: Limits (time-based)
Max Messages: 100,000
Max Bytes: 1 GB
Max Age: 7 days
Storage: File (persistent)
Replicas: 1
```

### Subjects

- `document.uploaded` - Nieuw document geüpload
- `document.updated` - Bestaand document gewijzigd
- `document.deleted` - Document verwijderd

### Consumer

```yaml
Durable Name: document-event-consumer
Ack Policy: Explicit
Deliver Policy: All (vanaf begin van stream)
Filter Subject: document.*
```

## Installatie

### Vereisten

- Docker & Docker Compose
- Node.js 18+ (voor development)

### Quick Start

```bash
# 1. Start NATS JetStream en Event Consumer
cd nats-events
docker-compose up -d

# 2. Check logs
docker-compose logs -f

# 3. Test NATS connection
curl http://localhost:4222
```

### Service Endpoints

| Service | URL | Beschrijving |
|---------|-----|--------------|
| NATS Client Port | localhost:4222 | NATS protocol |
| NATS HTTP Monitoring | localhost:8222 | Monitoring UI |
| Event Consumer API | localhost:3002 | REST API voor events |

## Event Consumer API

### GET /health

Health check endpoint.

**Response:**
```json
{
  "status": "ok",
  "timestamp": "2024-01-15T10:30:00.000Z"
}
```

### GET /events

Haal recente events op.

**Query Parameters:**
- `limit` (optional): Aantal events (default: 100)

**Response:**
```json
{
  "events": [...],
  "count": 42
}
```

### GET /events/system/:system

Filter events per DMS systeem.

**Parameters:**
- `system`: `paperless` of `alfresco`

**Response:**
```json
{
  "events": [...],
  "count": 20,
  "system": "paperless"
}
```

### GET /events/type/:type

Filter events per event type.

**Parameters:**
- `type`: `uploaded`, `updated`, of `deleted`

**Response:**
```json
{
  "events": [...],
  "count": 15,
  "type": "document.uploaded"
}
```

### GET /stats

Haal statistieken op.

**Response:**
```json
{
  "total": 1234,
  "last24h": 156,
  "bySystem": {
    "paperless": 800,
    "alfresco": 434
  },
  "byType": {
    "uploaded": 1200,
    "updated": 30,
    "deleted": 4
  }
}
```

### GET /events/stream

Server-Sent Events (SSE) stream voor real-time updates.

**Usage:**
```javascript
const eventSource = new EventSource('http://localhost:3002/events/stream');
eventSource.onmessage = (event) => {
  const data = JSON.parse(event.data);
  console.log('Events:', data.events);
  console.log('Stats:', data.stats);
};
```

## Dashboard Integratie

De EventStreamViewer component toont:

1. **Live Status Indicator** - Groen (connected) / Rood (offline)
2. **Filter Dropdown** - Filter op systeem (all, paperless, alfresco)
3. **Statistics Cards**:
   - Totaal events
   - Laatste 24 uur
   - Per systeem breakdown
4. **Event List** - Real-time scrollable lijst met:
   - Event type emoji (📤 upload, 📝 update, 🗑️ delete)
   - Document titel en systeem
   - Metadata (categorie, auteur, grootte, upload tijd)
   - Tags
   - Timestamp

### Screenshot Beschrijving

```
┌────────────────────────────────────────────────────┐
│ 🟢 Document Event Stream        [Filter: All ▼]   │
├────────────────────────────────────────────────────┤
│ [Totaal: 1234] [24h: 156] [📘 PL: 800] [🟣 AF:434]│
├────────────────────────────────────────────────────┤
│ 📤 Besluit Bouwvergunningen 2024-01-15  [paperless]│
│    📁 Bouwvergunningen • 👤 Jan de Vries • 45KB    │
│    openbaar definitief vergunning                  │
│    15-01-2024 10:30                         #1234  │
├────────────────────────────────────────────────────┤
│ 📤 Advies Subsidies 2024-01-15          [alfresco] │
│    📁 Subsidies • 👤 Maria Jansen • 78KB           │
│    urgent concept                                   │
│    15-01-2024 10:29                         #5678  │
└────────────────────────────────────────────────────┘
```

## NATS CLI Tools

### Installeer NATS CLI

```bash
brew install nats-io/nats-tools/nats

# Of download binary van:
# https://github.com/nats-io/natscli/releases
```

### Handige Commands

```bash
# Connect to NATS
nats context save local --server nats://localhost:4222

# List streams
nats stream ls

# Stream info
nats stream info DOCUMENT_EVENTS

# View stream contents
nats stream view DOCUMENT_EVENTS

# Consumer info
nats consumer info DOCUMENT_EVENTS document-event-consumer

# Subscribe to events (live)
nats sub "document.*"

# Publish test event
nats pub document.uploaded '{"eventId":"test-123","eventType":"document.uploaded",...}'

# Statistics
nats stream report DOCUMENT_EVENTS
```

## Monitoring

### NATS Monitoring UI

Open http://localhost:8222 voor NATS monitoring dashboard.

Toont:
- Server status
- Connections
- Subscriptions
- Message rates
- Memory usage

### Prometheus Metrics

NATS exposes Prometheus metrics op `/metrics`:

```bash
curl http://localhost:8222/metrics
```

### Consumer Logs

```bash
# Follow logs
docker-compose logs -f event-consumer

# Check specific service
docker-compose logs nats
```

## Performance

### Benchmarks

Met default configuratie:

- **Throughput**: ~100,000 msgs/sec
- **Latency**: < 1ms (lokaal)
- **Storage**: ~1KB per event
- **Retention**: 100,000 events of 1GB (eerst bereikt)

### Tuning

Voor hogere volumes, pas aan in `docker-compose.yml`:

```yaml
command: >
  -js
  -m 8222
  --max_payload 10MB       # Verhoog voor grote events
  --max_pending 500MB      # Verhoog buffer
  --store_dir /data
```

## Troubleshooting

### NATS niet bereikbaar

```bash
# Check of NATS draait
docker ps | grep nats

# Check logs
docker-compose logs nats

# Test connection
telnet localhost 4222
```

### Events komen niet aan

```bash
# Check of stream bestaat
nats stream ls

# Bekijk consumer status
nats consumer info DOCUMENT_EVENTS document-event-consumer

# Test publish direct
nats pub document.uploaded '{"test":"event"}'
```

### Event Consumer errors

```bash
# Check logs
docker-compose logs event-consumer

# Restart consumer
docker-compose restart event-consumer

# Rebuild
docker-compose up -d --build event-consumer
```

### Stream vol

Stream heeft limits (100k msgs of 1GB). Events ouder dan 7 dagen worden automatisch verwijderd.

Handmatig cleanup:
```bash
# Purge oude events
nats stream purge DOCUMENT_EVENTS --seq 1000

# Of verwijder en hermaak
nats stream rm DOCUMENT_EVENTS
# Restart consumer (maakt stream opnieuw aan)
docker-compose restart event-consumer
```

## Development

### Lokale Development (zonder Docker)

```bash
# Start NATS standalone
docker run -p 4222:4222 -p 8222:8222 nats:2.10-alpine -js -m 8222

# Start consumer
cd consumer
npm install
npm run dev

# Test met DMS simulator
cd ../../dms-simulator
# Set NATS_ENABLED=true in .env
npm run dev
```

### Custom Event Publisher

```typescript
import { NATSEventClient, DocumentEvent } from './nats-client';

const client = new NATSEventClient('nats://localhost:4222');
await client.connect();

const event: DocumentEvent = {
  eventId: 'my-event-123',
  eventType: 'document.uploaded',
  timestamp: new Date().toISOString(),
  system: 'paperless',
  document: { /* ... */ },
  metadata: { source: 'my-app' },
};

await client.publishDocumentEvent(event);
```

## Security

### Production Overwegingen

1. **Authentication**: Enable NATS authentication
2. **TLS**: Use encrypted connections
3. **ACLs**: Limit permissions per client
4. **Network**: Isoleer NATS in private network

### Enable Authentication

```yaml
# docker-compose.yml
command: >
  -js
  -m 8222
  --user natsuser
  --pass natspassword
```

### TLS Configuration

Genereer certificaten en mount in container:

```yaml
volumes:
  - ./certs:/certs:ro
command: >
  -js
  --tlscert /certs/server-cert.pem
  --tlskey /certs/server-key.pem
```

## Advanced Features

### Event Replay

Herlaad alle events vanaf een specifiek punt:

```typescript
const consumer = await js.consumers.get('DOCUMENT_EVENTS', 'replay-consumer');
const messages = await consumer.fetch({ max_messages: 1000 });

for await (const msg of messages) {
  const event = JSON.parse(msg.data.toString());
  // Process event
  msg.ack();
}
```

### Multiple Consumers

Meerdere services kunnen dezelfde events consumeren:

```bash
# Consumer 1: Analytics
nats consumer add DOCUMENT_EVENTS analytics-consumer

# Consumer 2: Audit Logging
nats consumer add DOCUMENT_EVENTS audit-consumer

# Consumer 3: Notifications
nats consumer add DOCUMENT_EVENTS notification-consumer
```

### Stream Backup

```bash
# Export stream
nats stream backup DOCUMENT_EVENTS ./backup

# Restore
nats stream restore DOCUMENT_EVENTS ./backup
```

## Integration Voorbeelden

### Webhook op Upload

```typescript
await natsClient.subscribe(async (event) => {
  if (event.eventType === 'document.uploaded') {
    await fetch('https://example.com/webhook', {
      method: 'POST',
      body: JSON.stringify(event),
    });
  }
});
```

### Elasticsearch Indexing

```typescript
await natsClient.subscribe(async (event) => {
  await esClient.index({
    index: 'documents',
    id: event.eventId,
    document: event.document,
  });
});
```

### Slack Notifications

```typescript
await natsClient.subscribe(async (event) => {
  if (event.document.tags.includes('urgent')) {
    await slackClient.postMessage({
      channel: '#documents',
      text: `🚨 Urgent document uploaded: ${event.document.title}`,
    });
  }
});
```

## Resources

- [NATS Documentation](https://docs.nats.io/)
- [JetStream Guide](https://docs.nats.io/nats-concepts/jetstream)
- [NATS CLI Tools](https://github.com/nats-io/natscli)
- [Node.js Client](https://github.com/nats-io/nats.js)

## Licentie

MIT
