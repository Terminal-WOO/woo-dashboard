# Paperless-ngx Document Management System

Een lichtgewicht, open-source document management systeem gebaseerd op Paperless-ngx.

## Over Paperless-ngx

Paperless-ngx is een community-supported open-source document management systeem dat fysieke documenten transformeert naar een doorzoekbaar online archief. Het gebruikt OCR (Optical Character Recognition) om tekst uit gescande documenten te halen en maakt deze volledig doorzoekbaar.

### Belangrijkste Features

- 📄 **OCR Support**: Automatische tekst extractie uit gescande documenten
- 🔍 **Krachtige Zoekfunctie**: Volledig doorzoekbare documenten
- 🏷️ **Tags & Categorieën**: Organiseer documenten met tags, correspondenten en document types
- 📅 **Datum Detectie**: Automatische detectie van document datums
- 📧 **Email Import**: Importeer documenten via email
- 🔐 **Gebruikersbeheer**: Multi-user support met permissies
- 📱 **Responsive UI**: Moderne web interface die werkt op alle devices
- 🔄 **API**: RESTful API voor integraties
- 📊 **Dashboard**: Overzicht van recent toegevoegde documenten
- 🗂️ **Batch Operations**: Bewerk meerdere documenten tegelijk
- 💾 **MinIO Storage**: S3-compatibele object storage voor schaalbare document opslag

## Installatie

### Vereisten

- Docker
- Docker Compose

### Stappen

1. **Start de services**:
   ```bash
   docker-compose up -d
   ```

2. **Wacht tot alle services gestart zijn** (kan enkele minuten duren bij eerste keer):
   ```bash
   docker-compose logs -f webserver
   ```

3. **Open de applicaties**:
   - Paperless-ngx: http://localhost:8000
     - Username: `admin`
     - Password: `admin`
   - MinIO Console: http://localhost:9001
     - Username: `minioadmin`
     - Password: `minioadmin`

4. **⚠️ BELANGRIJK**: Wijzig direct na eerste login:
   - Het admin wachtwoord (Paperless-ngx)
   - De MinIO credentials in docker-compose.yml
   - De `PAPERLESS_SECRET_KEY` in docker-compose.yml

## Gebruik

### Documenten Toevoegen

Er zijn verschillende manieren om documenten toe te voegen:

1. **Via de Web Interface**:
   - Klik op de upload knop in de interface
   - Sleep documenten naar de upload zone

2. **Via de Consume Folder**:
   - Plaats documenten in de `./consume` map
   - Paperless-ngx detecteert en verwerkt ze automatisch
   - Na verwerking worden ze verwijderd uit de consume map

3. **Via Email** (vereist extra configuratie):
   - Configureer email instellingen in de admin interface
   - Stuur documenten als bijlage naar het geconfigureerde email adres

### Documenten Organiseren

- **Tags**: Voeg tags toe om documenten te categoriseren
- **Correspondenten**: Wijs documenten toe aan afzenders/ontvangers
- **Document Types**: Categoriseer op document type (factuur, contract, etc.)
- **Custom Fields**: Voeg eigen velden toe voor extra metadata

### Documenten Zoeken

- **Full-text search**: Zoek in de volledige inhoud van documenten
- **Filters**: Filter op tags, correspondenten, datum, document type
- **Saved Views**: Bewaar zoekopdrachten voor hergebruik
- **Advanced Search**: Gebruik operators voor complexe zoekopdrachten

## MinIO Object Storage

Deze setup gebruikt **MinIO** voor document opslag - een lokale, S3-compatibele object storage oplossing.

### Waarom MinIO?

- ✅ **Lokale controle**: Al je documenten blijven op je eigen infrastructuur
- ✅ **Schaalbaar**: Eenvoudig uit te breiden
- ✅ **S3-compatible**: Industry standard protocol
- ✅ **Geen cloud kosten**: Alles draait lokaal
- ✅ **Privacy**: Data verlaat nooit je netwerk

**Zie [MINIO.md](MINIO.md) voor uitgebreide technische uitleg**

### MinIO Console Toegang

- URL: http://localhost:9001
- Username: `minioadmin`
- Password: `minioadmin`

Hier kun je:
- Buckets en bestanden bekijken
- Storage usage monitoren
- Access policies beheren

## PostgreSQL 18 Backend

Deze setup gebruikt **PostgreSQL 18** met een dedicated metadata schema voor MinIO object tracking:

### Features

- 📊 **Object Metadata**: Alle MinIO objecten geïndexeerd in PostgreSQL
- 📈 **Usage Analytics**: Dagelijkse statistieken per bucket
- 🔍 **Advanced Queries**: SQL queries op document metadata
- 📝 **Access Logging**: Track alle storage operaties
- 🔄 **Auto-Sync**: Continuous synchronisatie elke 5 minuten

**Database toegang**:
- Host: `localhost`
- Port: `5433`
- Database: `paperless`
- User/Password: `paperless/paperless`

**Zie [POSTGRES-MINIO-INTEGRATION.md](POSTGRES-MINIO-INTEGRATION.md) voor uitgebreide SQL voorbeelden en queries**

## Directory Structuur

```
paperless-ngx-dms/
├── docker-compose.yml           # Docker configuratie
├── .env.example                 # Environment variabelen voorbeeld
├── init-db/                     # PostgreSQL init scripts
│   └── 01-create-minio-schema.sql
├── minio-metadata-sync/         # MinIO → PostgreSQL sync service
│   ├── src/
│   ├── package.json
│   └── Dockerfile
├── consume/                     # Drop folder voor nieuwe documenten
├── export/                      # Export folder voor documenten
├── README.md                    # Deze file
├── MINIO.md                     # MinIO technische documentatie
└── POSTGRES-MINIO-INTEGRATION.md # PostgreSQL + MinIO integratie
```

## Configuratie

### Talen

Het systeem is geconfigureerd voor Nederlands en Engels OCR:
- `PAPERLESS_OCR_LANGUAGE=nld+eng`

Andere beschikbare talen kunnen toegevoegd worden. Zie de [Paperless-ngx documentatie](https://docs.paperless-ngx.com/configuration/).

### Timezone

Standaard ingesteld op `Europe/Amsterdam`. Wijzig in docker-compose.yml indien nodig.

### Advanced Features

De setup bevat optioneel:
- **Gotenberg**: Voor betere PDF conversie
- **Apache Tika**: Voor verbeterde document parsing

Deze kunnen uitgezet worden door de betreffende services uit docker-compose.yml te verwijderen.

## Beheer

### Logs Bekijken

```bash
docker-compose logs -f webserver
```

### Backup Maken

```bash
# Stop de containers
docker-compose down

# Backup volumes (database en MinIO storage)
mkdir -p backup
docker run --rm -v paperless-ngx-dms_data:/data -v $(pwd)/backup:/backup ubuntu tar czf /backup/data-backup.tar.gz -C /data .
docker run --rm -v paperless-ngx-dms_minio_data:/minio -v $(pwd)/backup:/backup ubuntu tar czf /backup/minio-backup.tar.gz -C /minio .
docker run --rm -v paperless-ngx-dms_pgdata:/pgdata -v $(pwd)/backup:/backup ubuntu tar czf /backup/db-backup.tar.gz -C /pgdata .

# Start weer op
docker-compose up -d
```

**Let op**: De documenten zitten nu in MinIO (`minio-backup.tar.gz`), niet meer in een media volume.

### Database Backup via Paperless Export

Een eenvoudigere methode:
1. Ga naar admin interface
2. Klik op "Documents" → "Export"
3. Download de export (bevat alle documenten + metadata)

### Updates

```bash
docker-compose pull
docker-compose up -d
```

## Troubleshooting

### Port 8000 al in gebruik

Wijzig de port mapping in docker-compose.yml:
```yaml
ports:
  - "8080:8000"  # gebruik port 8080 in plaats van 8000
```

### OCR werkt niet goed

1. Controleer of de juiste talen geïnstalleerd zijn
2. Check de logs: `docker-compose logs -f webserver`
3. Mogelijk moet je documenten opnieuw verwerken via de admin interface

### Services starten niet

```bash
# Stop alles
docker-compose down

# Verwijder oude volumes (⚠️ verliest data!)
docker-compose down -v

# Start opnieuw
docker-compose up -d
```

## Resources

- [Officiele Documentatie](https://docs.paperless-ngx.com/)
- [GitHub Repository](https://github.com/paperless-ngx/paperless-ngx)
- [Community Forum](https://github.com/paperless-ngx/paperless-ngx/discussions)

## Licentie

Paperless-ngx is gelicenseerd onder de GNU General Public License v3.0.

Deze setup is gemaakt voor educatieve en persoonlijke doeleinden.
