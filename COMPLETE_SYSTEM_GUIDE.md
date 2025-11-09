# Complete WOO Dashboard + DMS System Guide

## 🎯 Systeemoverzicht

Dit project integreert een WOO (Wet Open Overheid) Dashboard met twee Document Management Systemen en een event streaming platform:

### Componenten

1. **WOO Dashboard** - React frontend met Erlang backend voor WOO-verzoeken
2. **Paperless-ngx DMS** - Lichtgewicht document management (PostgreSQL 18 + MinIO)
3. **Alfresco DMS** - Enterprise-grade ECM platform (PostgreSQL 18 + MinIO)
4. **DMS Simulator** - Document generator met API clients voor beide systemen
5. **NATS JetStream** - Event streaming platform voor document tracking
6. **PostgreSQL Backends** - Drie database varianten (Mock, Erlang, PostgreSQL)

### Architectuur

```
┌─────────────────────────────────────────────────────────────────┐
│                     WOO Dashboard (React)                       │
│  - DMSSimulator Component    - EventStreamViewer Component     │
└───────────────┬─────────────────────────────────┬───────────────┘
                │                                 │
    ┌───────────▼──────────┐         ┌───────────▼──────────┐
    │  DMS Simulator API   │         │ NATS Event Consumer  │
    │  (port 3001)         │         │ (port 3002)          │
    └───────┬──────────┬───┘         └──────────┬───────────┘
            │          │                        │
   ┌────────▼───┐  ┌──▼─────────┐      ┌───────▼────────┐
   │ Paperless  │  │ Alfresco   │      │ NATS JetStream │
   │ (port 8000)│  │ (port 8080)│      │ (port 4222)    │
   └─────┬──────┘  └──┬─────────┘      └────────────────┘
         │            │
    ┌────▼────────────▼────┐
    │  MinIO (S3-local)    │
    │  (port 9000/9001)    │
    └──────────────────────┘
         │            │
    ┌────▼────┐  ┌───▼──────┐
    │ PG 18   │  │ PG 18    │
    │ (5433)  │  │ (5434)   │
    └─────────┘  └──────────┘
```

## 🚀 Snelstart - Volledig Systeem

### Stap 1: Installeer Dependencies

```bash
# Main dashboard
npm install

# DMS Simulator
cd dms-simulator
npm install
cd ..
```

### Stap 2: Start NATS JetStream

```bash
cd nats-events
docker-compose up -d
cd ..
```

**Verificatie:**
```bash
# Check NATS status
curl http://localhost:8222/varz

# Check Event Consumer API
curl http://localhost:3002/health
```

### Stap 3: Start Paperless-ngx DMS (Aanbevolen voor eerste test)

```bash
cd paperless-ngx-dms
docker-compose up -d
cd ..
```

**Wachttijd:** ~2-3 minuten voor eerste start (database initialisatie)

**Verificatie:**
```bash
# Check services
docker-compose -f paperless-ngx-dms/docker-compose.yml ps

# Check MinIO
curl http://localhost:9000/minio/health/live

# Check Paperless API
curl http://localhost:8000/api/
```

**Web Interfaces:**
- Paperless UI: http://localhost:8000 (admin / changeme123)
- MinIO Console: http://localhost:9001 (minioadmin / minioadmin)

### Stap 4: Start DMS Simulator

```bash
cd dms-simulator

# Kopieer environment configuratie
cp .env.example .env

# Edit .env om alleen Paperless te enablen voor eerste test
# PAPERLESS_ENABLED=true
# ALFRESCO_ENABLED=false
# NATS_ENABLED=true

# Start simulator
npm run dev
```

**Verificatie:**
```bash
# Test connections
curl http://localhost:3001/test-connections
```

### Stap 5: Start WOO Dashboard

```bash
# In hoofdmap
npm run dev
```

Open: http://localhost:5173

## 📊 Dashboard Gebruik

### DMS Simulator Sectie

1. **Verbindingsstatus** - Toont welke systemen actief zijn
2. **Document Aantal** - Slider van 1-50 documenten
3. **Systeem Selectie** - Kies Paperless, Alfresco, of beide
4. **Start Simulatie** - Upload realistische PDF documenten

**Documenttypes die worden gegenereerd:**
- Besluit (gemeenteraad beslissingen)
- Advies (expertadviezen)
- Brief (officiële correspondentie)
- Notitie (interne memo's)
- Rapportage (onderzoeksrapporten)
- Contract (overeenkomsten)

### Event Stream Viewer Sectie

1. **Live Status** - Groene indicator bij actieve NATS verbinding
2. **Statistieken Cards:**
   - Totaal Events
   - Paperless Uploads
   - Alfresco Uploads
   - Gemiddelde Upload Tijd
3. **Filter** - Bekijk alle events of filter per systeem
4. **Event List** - Real-time scrollbare lijst met:
   - Event ID
   - Documenttitel
   - Type & Categorie
   - Upload tijdstip
   - Systeem (Paperless/Alfresco)

**Auto-refresh:** Elke 3 seconden

## 🔄 Complete Workflow Test

### Test 1: Paperless Upload Flow

```bash
# 1. Start alle services (zoals hierboven)
# 2. Open dashboard http://localhost:5173
# 3. Scroll naar "Document Management Simulator"
# 4. Stel in:
#    - Aantal: 5 documenten
#    - Systeem: Paperless
# 5. Klik "Start Simulatie"
# 6. Observeer:
#    - Progress bar vult zich
#    - Documenten verschijnen in lijst
#    - Event Stream Viewer toont events
# 7. Verifieer in Paperless UI:
#    - Ga naar http://localhost:8000
#    - Login: admin / changeme123
#    - Zie 5 nieuwe documenten met tags
```

### Test 2: NATS Event Queries

```bash
# Alle events ophalen
curl http://localhost:3002/events?limit=10

# Alleen Paperless events
curl http://localhost:3002/events/system/paperless

# Event statistieken
curl http://localhost:3002/stats

# Live stream (Server-Sent Events)
curl -N http://localhost:3002/events/stream
```

### Test 3: MinIO Metadata in PostgreSQL

```bash
# Connecteer met Paperless database
docker exec -it paperless-ngx-dms-db-1 psql -U paperless

# Query MinIO metadata
SELECT * FROM minio_metadata.v_bucket_overview;
SELECT * FROM minio_metadata.v_recent_uploads LIMIT 10;
SELECT * FROM minio_metadata.v_bucket_statistics;

# Exit
\q
```

## 🏢 Alfresco DMS (Optioneel - Enterprise)

### Wanneer Alfresco gebruiken?

- **Paperless-ngx**: Klein tot middelgroot (< 10.000 documenten)
- **Alfresco**: Enterprise (> 10.000 documenten, workflow, compliance)

### Start Alfresco

```bash
cd alfresco-dms
docker-compose up -d
cd ..
```

**Systeemvereisten:**
- Minimaal 4GB RAM vrij
- ~10 minuten startup tijd (eerste keer)

**Web Interfaces:**
- Alfresco Share: http://localhost:8080/share (admin / admin)
- Alfresco Digital Workspace: http://localhost:8080/workspace
- MinIO Console: http://localhost:9002 (minioadmin / minioadmin)

### Alfresco Simulatie Enablen

```bash
# Edit dms-simulator/.env
ALFRESCO_ENABLED=true
ALFRESCO_URL=http://localhost:8080
ALFRESCO_USERNAME=admin
ALFRESCO_PASSWORD=admin

# Restart simulator
cd dms-simulator
npm run dev
```

## 🔧 Configuratie Details

### DMS Simulator Environment (.env)

```bash
# Server
PORT=3001

# Paperless-ngx
PAPERLESS_ENABLED=true
PAPERLESS_URL=http://localhost:8000
PAPERLESS_API_KEY=your-api-key-here

# Alfresco
ALFRESCO_ENABLED=false
ALFRESCO_URL=http://localhost:8080
ALFRESCO_USERNAME=admin
ALFRESCO_PASSWORD=admin

# NATS JetStream
NATS_ENABLED=true
NATS_URL=nats://localhost:4222
```

### NATS Consumer Environment

```bash
NATS_URL=nats://nats:4222
STREAM_NAME=DOCUMENT_EVENTS
CONSUMER_NAME=document-consumer
PORT=3000
```

## 📡 API Endpoints

### DMS Simulator (port 3001)

```bash
GET  /health                    # Health check
GET  /test-connections          # Test DMS connections
POST /simulate                  # Start simulation
  Body: { count: number, systems: string[] }
GET  /simulation/:id            # Get simulation status
```

### NATS Event Consumer (port 3002)

```bash
GET /health                     # Health check
GET /events?limit=100           # Get recent events
GET /events/system/:system      # Filter by system
GET /events/type/:type          # Filter by event type
GET /stats                      # Event statistics
GET /events/stream              # SSE live stream
```

## 🗄️ Database Toegang

### Paperless PostgreSQL (port 5433)

```bash
docker exec -it paperless-ngx-dms-db-1 psql -U paperless

# Schemas
\dn

# MinIO metadata tables
\dt minio_metadata.*

# Views
\dv minio_metadata.*
```

### Alfresco PostgreSQL (port 5434)

```bash
docker exec -it alfresco-dms-postgres-1 psql -U alfresco

# Schemas
\dn

# MinIO metadata
SELECT * FROM minio_metadata.v_bucket_overview;
```

## 🛠️ Troubleshooting

### DMS Simulator kan niet verbinden

```bash
# Check of services draaien
docker-compose -f paperless-ngx-dms/docker-compose.yml ps
docker-compose -f nats-events/docker-compose.yml ps

# Check logs
docker-compose -f paperless-ngx-dms/docker-compose.yml logs webserver
```

### NATS events komen niet door

```bash
# Check NATS status
curl http://localhost:8222/jsz

# Check stream
docker exec nats-events-nats-1 nats stream info DOCUMENT_EVENTS

# Check consumer logs
docker-compose -f nats-events/docker-compose.yml logs event-consumer
```

### Paperless upload faalt

```bash
# Check Paperless logs
docker-compose -f paperless-ngx-dms/docker-compose.yml logs webserver

# Check MinIO
curl http://localhost:9000/minio/health/live

# Verify API key
curl -H "Authorization: Token your-api-key" http://localhost:8000/api/
```

### Dashboard toont geen events

```bash
# Check browser console voor errors
# Verify Event Consumer is running
curl http://localhost:3002/health

# Verify CORS is enabled
curl -H "Origin: http://localhost:5173" http://localhost:3002/events
```

## 📈 Performance Tips

### Voor Grote Simulaties (> 25 documenten)

1. **Verhoog timeouts:**
```typescript
// dms-simulator/src/index.ts
const timeout = count > 25 ? 300000 : 120000; // 5 min voor grote batches
```

2. **Parallel processing:**
```typescript
// Upload in batches van 5
const batchSize = 5;
for (let i = 0; i < documents.length; i += batchSize) {
  const batch = documents.slice(i, i + batchSize);
  await Promise.all(batch.map(doc => uploadDocument(doc)));
}
```

3. **MinIO tuning:**
```yaml
# paperless-ngx-dms/docker-compose.yml
environment:
  MINIO_BROWSER_REDIRECT_URL: http://localhost:9001
  MINIO_API_REQUESTS_MAX: 1000  # Voor high throughput
```

## 🔐 Productie Overwegingen

### Security Checklist

- [ ] Verander default passwords (Paperless, Alfresco, MinIO, PostgreSQL)
- [ ] Gebruik environment variables voor secrets (nooit hardcoded)
- [ ] Enable HTTPS voor alle web interfaces
- [ ] Configure MinIO access policies
- [ ] Enable Paperless 2FA
- [ ] Setup NATS authentication
- [ ] Configure PostgreSQL SSL connections
- [ ] Setup backup strategie
- [ ] Enable audit logging
- [ ] Configure firewall rules

### Backup Strategie

```bash
# PostgreSQL dumps
docker exec paperless-ngx-dms-db-1 pg_dump -U paperless > backup_$(date +%Y%m%d).sql

# MinIO sync
mc mirror paperless-minio/paperless-documents /backup/minio/

# NATS stream backup
nats stream backup DOCUMENT_EVENTS /backup/nats/
```

## 📚 Gerelateerde Documentatie

- `paperless-ngx-dms/README.md` - Paperless DMS setup
- `alfresco-dms/README.md` - Alfresco DMS setup & comparison
- `nats-events/README.md` - NATS JetStream architecture
- `nats-events/QUICKSTART.md` - NATS snelstart gids
- `dms-simulator/README.md` - Document generator details

## 🎓 Volgende Stappen

1. **Test individuele componenten** met bovenstaande tests
2. **Run complete workflow** met 5-10 documenten
3. **Bekijk MinIO metadata** in PostgreSQL
4. **Analyseer NATS events** via API en dashboard
5. **Experimenteer met Alfresco** voor enterprise features
6. **Customize document templates** in dms-simulator/src/document-generator.ts

## 💡 Use Cases

### Use Case 1: WOO-verzoek Archivering
Gebruik Paperless-ngx om WOO-verzoek documenten te archiveren met automatische tags en OCR.

### Use Case 2: Enterprise Document Workflow
Gebruik Alfresco voor volledige workflow management met goedkeuringsprocessen.

### Use Case 3: Audit Trail
Gebruik NATS JetStream voor complete audit trail van alle document operaties.

### Use Case 4: Analytics
Query MinIO metadata in PostgreSQL voor storage analytics en usage patterns.

## 🤝 Support

Voor vragen of issues:
1. Check troubleshooting sectie hierboven
2. Review component-specifieke README files
3. Check Docker logs voor details

---

**Systeem Status Dashboard:** http://localhost:5173

**Happy Document Managing! 📄✨**
