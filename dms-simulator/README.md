# DMS Simulator

Een service die automatisch testdocumenten genereert en uploadt naar Paperless-ngx en/of Alfresco DMS systemen.

## Features

- 📄 **PDF Generatie**: Realistische overheidsdocumenten (besluiten, adviezen, brieven, etc.)
- 🎲 **Random Metadata**: Automatisch gegenereerde categorieën, auteurs, datums en tags
- 🔄 **Multi-DMS Support**: Upload naar Paperless-ngx én/of Alfresco tegelijkertijd
- 📊 **Real-time Progress**: Live voortgang tracking tijdens upload
- 🎯 **Configureerbaar**: Instelbaar aantal documenten (1-100)
- 🚀 **REST API**: Fastify-based API voor integratie met WOO dashboard

## Document Types

De simulator genereert 6 verschillende types documenten:

1. **Besluit** - Formele besluiten van het college
2. **Advies** - Ambtelijke adviezen
3. **Brief** - Officiële correspondentie
4. **Notitie** - Interne notities
5. **Rapportage** - Periodieke rapportages
6. **Contract** - Overeenkomsten

Elk document heeft:
- Unieke titel met datum
- Categorie (Bouwvergunningen, Belastingen, etc.)
- Auteur (random gekozen)
- Tags (urgent, openbaar, vertrouwelijk, etc.)
- Realistische content in PDF format

## Installatie

```bash
cd dms-simulator

# Installeer dependencies
npm install

# Kopieer environment variables
cp .env.example .env

# Edit .env en voeg Paperless API key toe
nano .env
```

### Paperless API Key Verkrijgen

1. Start Paperless-ngx: `cd ../paperless-ngx-dms && docker-compose up -d`
2. Open http://localhost:8000
3. Login (admin/admin)
4. Ga naar Settings (tandwiel icoon)
5. Scroll naar "API Tokens"
6. Click "Generate Token"
7. Kopieer token en plak in `.env`:
   ```
   PAPERLESS_API_KEY=jouw-token-hier
   ```

### Alfresco Configuratie

Alfresco gebruikt basic authentication (standaard admin/admin):

```env
ALFRESCO_ENABLED=true
ALFRESCO_URL=http://localhost:8080
ALFRESCO_USERNAME=admin
ALFRESCO_PASSWORD=admin
```

## Gebruik

### Development Server

```bash
npm run dev
```

Server start op http://localhost:3001

### Production

```bash
npm run build
npm start
```

## API Endpoints

### GET /health
Health check endpoint

**Response:**
```json
{
  "status": "ok",
  "timestamp": "2024-01-15T10:30:00.000Z"
}
```

### GET /test-connections
Test verbindingen met DMS systemen

**Response:**
```json
{
  "paperless": true,
  "alfresco": false
}
```

### POST /simulate
Start document simulatie

**Request Body:**
```json
{
  "count": 10,
  "systems": ["paperless", "alfresco"]
}
```

**Response:**
```json
{
  "simulationId": "sim-1234567890",
  "status": "started"
}
```

### GET /simulate/:id
Bekijk simulatie status

**Response:**
```json
{
  "total": 20,
  "completed": 15,
  "failed": 0,
  "status": "running",
  "progress": 75,
  "startTime": "2024-01-15T10:30:00.000Z",
  "documents": [
    {
      "id": "123",
      "title": "Besluit Bouwvergunningen 2024-01-15",
      "system": "paperless",
      "status": "success"
    }
  ]
}
```

### GET /simulations
Lijst alle simulaties

**Response:**
```json
[
  {
    "id": "sim-1234567890",
    "total": 10,
    "completed": 10,
    "failed": 0,
    "status": "completed",
    "progress": 100
  }
]
```

## Integratie met WOO Dashboard

De DMS Simulator is geïntegreerd in het WOO Dashboard via de `DMSSimulator` component.

### Setup

1. **Start DMS systemen:**
   ```bash
   # Paperless-ngx
   cd paperless-ngx-dms
   docker-compose up -d

   # Of Alfresco
   cd alfresco-dms
   docker-compose up -d
   ```

2. **Start Simulator:**
   ```bash
   cd dms-simulator
   npm run dev
   ```

3. **Start WOO Dashboard:**
   ```bash
   cd ..
   npm run dev
   ```

4. **Open Dashboard:**
   - http://localhost:5173
   - Scroll naar "DMS Simulator" sectie
   - Selecteer aantal documenten en systemen
   - Click "Start Simulatie"

### UI Features

De dashboard UI toont:
- ✅ Connection status voor beide DMS systemen
- 🎚️ Slider voor aantal documenten (1-50)
- ☑️ Checkboxes voor systeem selectie
- 📊 Real-time progress bar
- 📄 Live lijst van geüploade documenten
- ✓ Success/failure status per document

## Document Structuur

Gegenereerde PDFs bevatten:
- **Header**: Titel en document type
- **Metadata**: Type, categorie, auteur, datum, tags
- **Content**: Type-specifieke template met realistische tekst
- **Footer**: Paginanummering

### Voorbeeld PDF Content

```
BESLUIT

Onderwerp: Besluit Bouwvergunningen 2024-01-15
Datum: 15 januari 2024
Behandelaar: Jan de Vries
Categorie: Bouwvergunningen

HET COLLEGE VAN BURGEMEESTER EN WETHOUDERS,

Gelet op:
- De Algemene wet bestuursrecht
- De gemeentewet
...
```

## Categorieën

De simulator gebruikt 10 categorieën:
- Bouwvergunningen
- Belastingen
- Subsidies
- Handhaving
- Bezwaarschriften
- Financiën
- Personeel
- Inkoop
- Communicatie
- Milieu

## Tags

Mogelijk tags pool:
- urgent
- openbaar
- vertrouwelijk
- concept
- definitief
- archief
- woo-verzoek
- klacht
- subsidie
- vergunning
- handhaving
- bezwaar

## Technische Details

### Dependencies

- **fastify**: Web framework
- **pdfkit**: PDF generatie
- **axios**: HTTP client voor DMS APIs
- **form-data**: Multipart uploads
- **pino**: Logging

### Architectuur

```
┌─────────────────────┐
│   WOO Dashboard     │
│  (React Frontend)   │
└──────────┬──────────┘
           │ HTTP
           ▼
┌─────────────────────┐
│   DMS Simulator     │
│  (Fastify Server)   │
│                     │
│  - Document Gen     │
│  - PDF Creation     │
│  - Upload Logic     │
└──────┬──────────┬───┘
       │          │
       ▼          ▼
┌───────────┐  ┌───────────┐
│ Paperless │  │ Alfresco  │
│    API    │  │    API    │
└───────────┘  └───────────┘
```

### PDF Generation

PDFKit wordt gebruikt om realistische PDFs te maken:
- A4 formaat
- 50px margins
- Professionele fonts
- Gestructureerde layout
- Paginanummering

### Upload Flow

1. Genereer metadata (random)
2. Maak PDF met PDFKit
3. Voor elk geselecteerd systeem:
   - Maak FormData met PDF buffer
   - Voeg metadata toe
   - POST naar DMS API
   - Track succes/failure
4. Update simulatie status
5. Return resultaten

## Troubleshooting

### Connection Failed

**Probleem:** DMS systeem niet bereikbaar

**Oplossing:**
1. Check of DMS container draait: `docker ps`
2. Test direct: `curl http://localhost:8000/api/` (Paperless)
3. Check firewall/ports
4. Bekijk DMS logs: `docker-compose logs`

### Upload Failed

**Probleem:** Document upload mislukt

**Oplossing:**
1. Check API credentials in `.env`
2. Bekijk simulator logs voor details
3. Test API direct met curl
4. Controleer DMS storage quota

### Paperless Authentication Error

**Probleem:** 401 Unauthorized

**Oplossing:**
1. Genereer nieuwe API token in Paperless UI
2. Update `.env` met nieuwe token
3. Restart simulator: `npm run dev`

### Alfresco Login Failed

**Probleem:** Alfresco ticket creation failed

**Oplossing:**
1. Check Alfresco is gestart: `docker-compose ps alfresco`
2. Wacht tot fully started (5-10 min bij eerste keer)
3. Test login: `curl -u admin:admin http://localhost:8080/alfresco/api/discovery`

## Limitaties

- Maximum 100 documenten per simulatie (performance)
- Alleen PDF format (geen Word, etc.)
- Vaste document templates (niet customizable via UI)
- Geen support voor custom metadata fields
- Rate limiting: 500ms delay tussen uploads

## Toekomstige Features

- [ ] Custom document templates
- [ ] Support voor meer file types (DOCX, XLSX)
- [ ] Bulk export functionaliteit
- [ ] Document relationship tracking
- [ ] Webhook notifications
- [ ] Prometheus metrics
- [ ] Docker container variant

## Development

```bash
# Watch mode
npm run dev

# Build
npm run build

# Type checking
npx tsc --noEmit

# Format
npx prettier --write src/
```

## Licentie

MIT

## Support

Voor vragen en issues:
- Check de logs: DMS simulator console output
- Test verbindingen: GET `/test-connections`
- Verify DMS systemen draaien: `docker ps`
