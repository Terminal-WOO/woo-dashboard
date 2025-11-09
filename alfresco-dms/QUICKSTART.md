# Alfresco DMS - Snelstart Gids

## In 10 minuten aan de slag

### ⚠️ Let Op

Alfresco heeft significant meer resources nodig dan Paperless-ngx:
- Minimaal **8GB RAM** beschikbaar voor Docker
- Eerste opstart duurt **5-10 minuten**
- Wees geduldig tijdens het opstarten!

### 1. Controleer Docker Resources

```bash
# Check beschikbare memory
docker system info | grep "Total Memory"

# Moet minimaal 8GB zijn
# Pas Docker Desktop settings aan indien nodig
```

### 2. Start het Systeem

```bash
cd alfresco-dms
docker-compose up -d
```

### 3. Volg de Opstart

```bash
# Volg Alfresco logs
docker-compose logs -f alfresco

# Wacht tot je ziet:
# "Server startup in [XXXXX] milliseconds"
```

Dit kan 5-10 minuten duren bij eerste keer!

### 4. Toegang tot Interfaces

**Alfresco Share (Classic UI)**
- URL: http://localhost:8081/share
- Username: `admin`
- Password: `admin`
- Bekende interface, veel features

**Digital Workspace (Modern UI)**
- URL: http://localhost:8082
- Username: `admin`
- Password: `admin`
- Modern, responsive interface

**MinIO Console**
- URL: http://localhost:9003
- Username: `minioadmin`
- Password: `minioadmin`
- Bekijk opgeslagen content

### 5. Upload Je Eerste Document

**Via Share:**
1. Login op http://localhost:8081/share
2. Click "Repository" (linker menu)
3. Ga naar "User Homes" → "admin"
4. Click "Upload" knop
5. Selecteer bestand(en)
6. Document verschijnt in lijst

**Via Digital Workspace:**
1. Login op http://localhost:8082
2. Click "Personal Files"
3. Drag & drop bestand
4. Klaar!

### 6. Test Zoekfunctionaliteit

1. Upload een PDF of Word document
2. Wacht 30 seconden (indexering)
3. Gebruik search bar bovenaan
4. Typ woord uit het document
5. Document verschijnt in resultaten!

### 7. Probeer Versioning

1. Open een document
2. Click "Upload New Version"
3. Selecteer aangepaste versie
4. Click "Version History" om alle versies te zien

## Database Toegang

```bash
# Verbind met PostgreSQL
psql -h localhost -p 5434 -U alfresco -d alfresco

# In psql:
SET search_path TO minio_metadata;
SELECT * FROM v_bucket_overview;
SELECT * FROM v_alfresco_storage LIMIT 10;
```

## Handige Tips

### Content Organization

**Sites Gebruiken:**
1. Ga naar "Sites" in menu
2. Click "Create Site"
3. Geef naam en beschrijving
4. Nodig teamleden uit
5. Upload documents naar site library

### Metadata Toevoegen

1. Select document
2. Click "Edit Properties"
3. Voeg Title, Description toe
4. Add custom properties
5. Gebruik voor betere zoekresultaten

### Delen

1. Select document
2. Click "Share"
3. Kies gebruikers of genereer link
4. Stel permissions in
5. Send notification

### Workflows Starten

1. Select document
2. Click "Start Workflow"
3. Kies "Review and Approve"
4. Assign reviewers
5. Set due date
6. Submit

## Veelvoorkomende Problemen

### "Connection Refused" Errors

Service is nog aan het opstarten. Wacht nog 2-3 minuten.

```bash
# Check status
docker-compose ps

# Check logs
docker-compose logs alfresco
```

### Langzame Performance

```bash
# Verhoog memory voor Alfresco
# Edit docker-compose.yml:
# -Xms2g -Xmx2g  # naar -Xms4g -Xmx4g

docker-compose down
docker-compose up -d
```

### Search Werkt Niet

```bash
# Check Solr status
curl http://localhost:8083/solr/admin/cores?action=STATUS

# Restart Solr
docker-compose restart solr6
```

### Port 8080 Bezet

```bash
# Wijzig in docker-compose.yml:
ports:
  - "8085:8080"  # gebruik 8085 in plaats van 8080
```

## Stoppen en Opstarten

**Stoppen:**
```bash
docker-compose down
```

**Opstarten (na stoppen):**
```bash
docker-compose up -d
```

**Volledig verwijderen (inclusief data):**
```bash
docker-compose down -v
# ⚠️ Dit verwijdert alle documenten!
```

## Services Overzicht

| Service | Functie | Memory | Port |
|---------|---------|--------|------|
| alfresco | Content Repository | 1.5GB | 8080 |
| share | Web UI (Classic) | 500MB | 8081 |
| digital-workspace | Web UI (Modern) | - | 8082 |
| solr6 | Search Engine | 1GB | 8083 |
| postgres | Database | - | 5434 |
| minio | Content Storage | - | 9002, 9003 |
| activemq | Message Broker | - | 8161, 61616 |
| transform | Document Conversion | 512MB | 8090 |

**Totaal:** ~4-5GB RAM benodigd

## Volgende Stappen

1. **Wijzig Wachtwoorden** (productie)
2. **Configureer Email** (notifications)
3. **Setup LDAP** (user management)
4. **Install Extensions** (extra features)
5. **Configure Workflows** (business processes)

## Meer Informatie

Zie **README.md** voor:
- Uitgebreide configuratie opties
- PostgreSQL query voorbeelden
- API documentatie
- Troubleshooting guide
- Security best practices

## API Quick Reference

```bash
# Repository info
curl -u admin:admin http://localhost:8080/alfresco/api/discovery

# List content
curl -u admin:admin \
  "http://localhost:8080/alfresco/api/-default-/public/alfresco/versions/1/nodes/-my-/children"

# Upload
curl -u admin:admin \
  -X POST \
  -F filedata=@document.pdf \
  -F relativePath="/test" \
  "http://localhost:8080/alfresco/api/-default-/public/alfresco/versions/1/nodes/-my-/children"
```

## Resources

- Share: http://localhost:8081/share
- Digital Workspace: http://localhost:8082
- API Explorer: http://localhost:8080/api-explorer
- Admin Console: http://localhost:8081/share/page/console/admin-console
- MinIO: http://localhost:9003

Veel succes met Alfresco! 🚀
