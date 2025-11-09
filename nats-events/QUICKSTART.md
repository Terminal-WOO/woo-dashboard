# NATS Event System - Quick Start

## In 5 Minuten aan de Slag

### 1. Start NATS JetStream en Event Consumer

```bash
cd nats-events
docker-compose up -d
```

Dit start:
- NATS JetStream server (port 4222)
- NATS monitoring (port 8222)
- Event Consumer API (port 3002)

### 2. Check of alles draait

```bash
# Check containers
docker-compose ps

# Should show:
# nats-jetstream      running
# nats-event-consumer running

# Check logs
docker-compose logs -f
```

### 3. Test de services

```bash
# Test NATS monitoring
curl http://localhost:8222/varz

# Test Event Consumer API
curl http://localhost:3002/health
# Response: {"status":"ok","timestamp":"..."}

# Get current stats
curl http://localhost:3002/stats
```

### 4. Enable NATS in DMS Simulator

```bash
cd ../dms-simulator

# Create or edit .env
cat > .env << 'EOF'
# Server
PORT=3001
HOST=0.0.0.0

# NATS JetStream Event System
NATS_ENABLED=true
NATS_URL=nats://localhost:4222

# Paperless-ngx Configuration
PAPERLESS_ENABLED=true
PAPERLESS_URL=http://localhost:8000
PAPERLESS_API_KEY=your-api-key-here

# Alfresco Configuration
ALFRESCO_ENABLED=false
ALFRESCO_URL=http://localhost:8080
ALFRESCO_USERNAME=admin
ALFRESCO_PASSWORD=admin
EOF

# Start simulator
npm run dev
```

### 5. Upload Test Document

Via DMS Simulator API:

```bash
curl -X POST http://localhost:3001/simulate \
  -H "Content-Type: application/json" \
  -d '{
    "count": 1,
    "systems": ["paperless"]
  }'

# Response: {"simulationId":"sim-...","status":"started"}
```

### 6. View Events

```bash
# Get latest events
curl http://localhost:3002/events?limit=5

# Get events from specific system
curl http://localhost:3002/events/system/paperless

# Get only uploaded events
curl http://localhost:3002/events/type/uploaded

# Get statistics
curl http://localhost:3002/stats
```

### 7. View in Dashboard

```bash
cd ../
npm run dev

# Open browser: http://localhost:5173
# Scroll to "Document Event Stream" section
```

You should see:
- 🟢 Live connection indicator
- Statistics cards
- Real-time event list

## Verify Everything Works

### Complete Test Flow

```bash
# 1. Start all services
cd nats-events && docker-compose up -d
cd ../dms-simulator && npm run dev &
cd ../ && npm run dev &

# 2. Upload document via simulator API
curl -X POST http://localhost:3001/simulate \
  -H "Content-Type: application/json" \
  -d '{"count":5,"systems":["paperless"]}'

# 3. Check events appeared
curl http://localhost:3002/events?limit=10

# 4. View in dashboard
# Open http://localhost:5173
# Check "Document Event Stream" section
```

## Common Issues

### NATS Container Won't Start

```bash
# Check if port 4222 is already in use
lsof -i :4222

# View logs
docker-compose logs nats

# Restart
docker-compose restart nats
```

### Event Consumer Can't Connect

```bash
# Make sure NATS is running first
docker-compose ps nats

# Check consumer logs
docker-compose logs event-consumer

# Restart consumer
docker-compose restart event-consumer
```

### No Events Appearing

Check:
1. Is NATS_ENABLED=true in dms-simulator/.env?
2. Is DMS simulator running?
3. Did you upload documents?
4. Check simulator logs for NATS connection

```bash
# Simulator logs should show:
# "Connected to NATS JetStream"

# If not, check NATS_URL is correct
```

### Dashboard Shows "Offline"

Check:
1. Is event-consumer running? (port 3002)
2. CORS enabled? (should be by default)
3. Browser console for errors

```bash
# Test consumer from browser
curl http://localhost:3002/health
```

## NATS CLI (Optional)

Install NATS CLI for advanced debugging:

```bash
# macOS
brew install nats-io/nats-tools/nats

# Linux
curl -sf https://binaries.nats.dev/nats-io/natscli/nats@latest | sh

# Windows
scoop install nats
```

### Useful CLI Commands

```bash
# Connect to local NATS
nats context save local --server nats://localhost:4222

# View streams
nats stream ls

# Stream details
nats stream info DOCUMENT_EVENTS

# View events live
nats sub "document.*"

# View specific event
nats stream get DOCUMENT_EVENTS 1

# Consumer info
nats consumer info DOCUMENT_EVENTS document-event-consumer
```

## Stop Services

```bash
# Stop NATS system
cd nats-events
docker-compose down

# Stop simulators
pkill -f "npm run dev"
```

## Next Steps

- Read full [README.md](README.md) for complete documentation
- Explore Event Consumer API endpoints
- Customize event handling
- Add webhooks or integrations
- Monitor with NATS monitoring UI (http://localhost:8222)

## Architecture Summary

```
┌─────────────────┐
│ DMS Simulator   │ ─── publishes events ───┐
└─────────────────┘                          │
                                             ▼
                                    ┌─────────────────┐
                                    │ NATS JetStream  │
                                    │ (Port 4222)     │
                                    └────────┬────────┘
                                             │
                          ┌──────────────────┴──────────────────┐
                          ▼                                     ▼
                   ┌──────────────┐                    ┌───────────────┐
                   │ Event        │                    │ NATS CLI      │
                   │ Consumer API │────HTTP────►       │ Tools         │
                   │ (Port 3002)  │                    │ (Optional)    │
                   └──────┬───────┘                    └───────────────┘
                          │
                          │ HTTP/SSE
                          ▼
                   ┌──────────────┐
                   │ WOO Dashboard│
                   │ Event Viewer │
                   └──────────────┘
```

Happy event streaming! 🎉
