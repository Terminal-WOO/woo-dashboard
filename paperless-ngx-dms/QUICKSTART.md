# Snelstart Gids - Paperless-ngx DMS

## In 5 minuten aan de slag

### 1. Start het systeem

```bash
cd paperless-ngx-dms
docker-compose up -d
```

### 2. Wacht tot services klaar zijn

De eerste keer kan dit 2-5 minuten duren:

```bash
docker-compose logs -f webserver
```

Wacht tot je ziet: `Application startup complete`

### 3. Open de applicaties

- **Paperless-ngx**: http://localhost:8000
  - Username: `admin`
  - Password: `admin`
  
- **MinIO Console** (optioneel): http://localhost:9001
  - Username: `minioadmin`
  - Password: `minioadmin`
  - Hier kun je de opgeslagen documenten bekijken in de buckets

### 4. ⚠️ VERPLICHT: Wijzig wachtwoord

1. Klik rechtsboven op je gebruikersnaam
2. Ga naar "Settings" → "Change Password"
3. Wijzig het wachtwoord

### 5. Upload je eerste document

**Optie A - Via Web Interface:**
1. Klik op "Documents" in het menu
2. Klik op de upload knop (of sleep een bestand)
3. Wacht tot het verwerkt is

**Optie B - Via Consume Folder:**
```bash
# Kopieer een document naar de consume folder
cp ~/Documents/mijn-document.pdf ./consume/
```

Het document wordt binnen enkele seconden automatisch verwerkt.

### 6. Organiseer je documenten

1. **Tags toevoegen**: Klik op een document → "Edit" → Voeg tags toe
2. **Correspondent instellen**: Wijs toe aan wie het document van/naar is
3. **Document type**: Categoriseer (factuur, contract, brief, etc.)

## Handige Tips

### OCR Taal

Standaard ondersteunt het systeem Nederlands en Engels. De OCR herkent automatisch tekst in beide talen.

### Zoeken

- Gebruik de zoekbalk bovenaan voor full-text search
- Klik op tags/correspondenten om te filteren
- Gebruik "Saved Views" voor veelgebruikte zoekopdrachten

### Batch Upload

Sleep meerdere bestanden tegelijk naar de upload zone of plaats ze allemaal in de `consume/` folder.

### Mobiel Gebruik

De interface is volledig responsive en werkt op telefoons/tablets.

## Problemen?

### Port 8000 bezet?

Wijzig in `docker-compose.yml`:
```yaml
ports:
  - "8080:8000"
```

### Services starten niet?

```bash
docker-compose down
docker-compose up -d
```

### Meer hulp nodig?

Zie `README.md` voor uitgebreide documentatie.

## PostgreSQL Database (Bonus)

Deze setup bevat ook een PostgreSQL 18 database die MinIO metadata bijhoudt!

**Toegang:**
```bash
psql -h localhost -p 5433 -U paperless -d paperless
```

**Voorbeeld queries:**
```sql
-- Bekijk alle buckets en hun grootte
SELECT * FROM minio_metadata.v_bucket_overview;

-- Recente activiteit
SELECT * FROM minio_metadata.v_recent_activity LIMIT 10;

-- Storage per content type
SELECT content_type, COUNT(*), pg_size_pretty(SUM(size_bytes)::bigint)
FROM minio_metadata.objects
GROUP BY content_type;
```

Zie `POSTGRES-MINIO-INTEGRATION.md` voor meer SQL voorbeelden!

## Stoppen

```bash
docker-compose down
```

## Opstarten (na stoppen)

```bash
docker-compose up -d
```

Je documenten blijven bewaard in Docker volumes.
