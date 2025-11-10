# Alfresco DMS Status

## Huidige Status

Alfresco Community Edition 23.2.1 is geconfigureerd maar start niet volledig op vanwege keystore configuratie problemen.

## Probleem

Alfresco vereist encryption keystores voor het opslaan van gevoelige data. De error:
```
org.alfresco.error.AlfrescoRuntimeException: 10100002 Keystores are invalid
```

## Wat is Gedaan

1. ✅ Alfresco client code aangepast voor REST API v1 (`/api/` in plaats van `/alfresco/api/`)
2. ✅ Docker Compose configuratie gecorrigeerd:
   - PostgreSQL 18 volume mount gefixed (`/var/lib/postgresql`)
   - JAVA_OPTS gesimplificeerd (ActiveMQ URL verwijderd)
   - Digital workspace uitgeschakeld (image pull issues)
3. ✅ Alfresco enabled in DMS Simulator (`.env`)
4. ✅ Keystore directory aangemaakt met test keys
5. ✅ Encryption configuratie geprobeerd

## Volgende Stappen voor Productie

Voor een volledige Alfresco integratie zijn de volgende stappen nodig:

### Optie 1: Enterprise Edition (Aanbevolen voor Productie)
- Alfresco Enterprise Edition heeft betere documentatie en support
- Includes pre-configured keystores en easier setup
- Vereist licentie

### Optie 2: Community Edition Met Volledige Keystore Setup
1. Genereer complete keystore met alle vereiste keys:
   ```bash
   keytool -genseckey -alias metadata -keyalg DESede -keysize 168 \
     -keystore keystore -storetype JCEKS \
     -keypass <password> -storepass <storepass>
   ```

2. Configureer alle encryption properties in `alfresco-global.properties`:
   ```properties
   encryption.keystore.type=JCEKS
   encryption.cipherAlgorithm=DESede/CBC/PKCS5Padding
   encryption.keyAlgorithm=DESede
   encryption.keystore.location=/usr/local/tomcat/shared/classes/alfresco/extension/keystore/keystore
   metadata-keystore.password=<password>
   metadata-keystore.aliases=metadata
   metadata-keystore.metadata.password=<password>
   metadata-keystore.metadata.algorithm=DESede
   ```

3. Zorg voor proper SSL keystores voor Solr communicatie

### Optie 3: Alternatief - Gebruik Alfresco Share in plaats van REST API
- Alfresco Share web UI werkt vaak wel zonder volledige keystore setup
- Toegankelijk via `http://localhost:8081/share` (als geconfigureerd)

## Voor Development/Testing

Voor een snelle development setup kan je overwegen:
- **Alfresco Content Services SDK**: Lightweight development image
- **Paperless-ngx**: Alternatief DMS systeem dat wel volledig werkt (reeds geïntegreerd)
- **Mock Alfresco**: Stub implementation voor testing

## Huidige Configuratie Files

- `docker-compose.yml`: Alfresco stack met PostgreSQL, Solr, ActiveMQ
- `config/keystore/`: Encryption keystore directory (incomplete)
- `dms-simulator/.env`: ALFRESCO_ENABLED=true
- `dms-simulator/src/alfresco-client.ts`: REST API v1 client code

## Resources

- [Alfresco Documentation](https://docs.alfresco.com/)
- [Alfresco Docker Compose](https://github.com/Alfresco/acs-deployment)
- [Encryption Configuration](https://docs.alfresco.com/content-services/latest/admin/security/#encryption)
