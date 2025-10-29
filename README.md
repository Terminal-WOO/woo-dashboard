# WOO Dashboard

Een interactief dashboard voor het monitoren van Wet Open Overheid (WOO) verzoeken met real-time simulatie functionaliteit.

## Live Demo

Het dashboard is live beschikbaar op: **https://terminal-woo.github.io/woo-dashboard/**

## Overzicht

WOO Dashboard is een modern React-based dashboard dat statistieken en status updates van WOO-verzoeken visualiseert. Het biedt een real-time simulatie van document lifecycle management met automatische status updates en event tracking.

### Belangrijkste Functies

- **Real-time Status Updates**: Automatische simulatie van document status wijzigingen
- **Live Event Feed**: Real-time notificaties van alle document events
- **Interactieve Statistieken**: Visuele grafieken en metrics
- **Smart Event System**: Geoptimaliseerd event systeem met automatische cleanup
- **Responsive Design**: Werkt op desktop, tablet en mobile

## Technologie Stack

- **React 18** - UI framework
- **TypeScript** - Type-safe development
- **Recharts** - Data visualisatie
- **Vite** - Build tool en development server
- **CSS3** - Styling met custom properties

## Projectstructuur

```
woo-dashboard/
├── src/
│   ├── components/
│   │   ├── StatsCard.tsx        # Statistiek cards
│   │   ├── RequestsTable.tsx    # Documenten tabel
│   │   └── ActivityFeed.tsx     # Live event feed
│   ├── App.tsx                  # Hoofd applicatie component
│   ├── App.css                  # Applicatie styling
│   ├── types.ts                 # TypeScript type definities
│   ├── data.ts                  # Mock data en data functies
│   ├── eventSystem.ts           # Smart event management systeem
│   ├── statusSimulator.ts       # Document status simulator
│   ├── documentGenerator.ts     # Nieuwe document generator
│   └── main.tsx                 # App entry point
├── .github/
│   └── workflows/
│       └── deploy.yml           # GitHub Actions deployment
├── index.html
├── package.json
├── tsconfig.json
├── vite.config.ts
└── README.md
```

## Architectuur

### Event System

Het dashboard gebruikt een slim event systeem (src/eventSystem.ts:1) dat:
- Events beheert met automatische cleanup na 30 seconden
- Maximaal 50 events in memory houdt
- Event listeners ondersteunt voor real-time updates
- Memory leaks voorkomt door oude events te verwijderen

### Status Simulator

De status simulator (src/statusSimulator.ts:1) zorgt voor:
- Automatische status progressie: Ontvangen → In behandeling → Afgerond
- Dynamisch toevoegen van nieuwe documenten
- Event emissie bij elke status wijziging
- Configureerbare update intervallen (standaard 5 seconden)

### Document Generator

De document generator (src/documentGenerator.ts:1) creëert realistische WOO documenten met:
- Variabele titels en onderwerpen
- Realistische organisatie namen
- Automatische datum generatie
- Willekeurige initiële statussen

## Lokale Ontwikkeling

### Vereisten

- Node.js 20 of hoger
- npm

### Installatie

```bash
# Clone de repository
git clone https://github.com/Terminal-WOO/woo-dashboard.git
cd woo-dashboard

# Installeer dependencies
npm install

# Start development server
npm run dev
```

De applicatie is nu beschikbaar op `http://localhost:5173`

### Build voor productie

```bash
npm run build
```

De production build wordt geplaatst in de `dist/` directory.

### Preview productie build

```bash
npm run preview
```

## Deployment naar GitHub Pages

De app wordt automatisch gedeployed naar GitHub Pages bij elke push naar de master branch via GitHub Actions.

### Handmatige deployment

```bash
npm run deploy
```

Dit commando:
1. Bouwt de productie versie
2. Deployt naar de gh-pages branch
3. Publiceert naar GitHub Pages

### GitHub Pages Configuratie

In de repository settings:
1. Ga naar **Settings** → **Pages**
2. Selecteer **Deploy from a branch**
3. Branch: **gh-pages**
4. Folder: **/ (root)**

## Gebruik

### Simulatie Starten/Stoppen

Klik op de "Start Simulatie" knop in de rechterbovenhoek om:
- Automatische status updates te activeren
- Nieuwe documenten te laten verschijnen
- Live events te genereren

De simulatie update interval is 5 seconden.

### Dashboard Secties

1. **Statistieken Cards** (boven)
   - Totaal aantal verzoeken
   - Openstaande verzoeken
   - Afgeronde verzoeken
   - Gemiddelde verwerkingstijd

2. **Grafieken** (midden)
   - Maandelijkse trend (Bar Chart)
   - Status verdeling (Pie Chart)

3. **Documenten Tabel** (links beneden)
   - Alle WOO verzoeken met details
   - Status badges met kleuren
   - Organisatie informatie

4. **Activity Feed** (rechts beneden)
   - Real-time event notificaties
   - Status change tracking
   - Nieuwe document alerts

## Scripts

```json
{
  "dev": "vite",                    // Start development server
  "build": "tsc && vite build",     // Build voor productie
  "preview": "vite preview",        // Preview productie build
  "predeploy": "npm run build",     // Pre-deployment build
  "deploy": "gh-pages -d dist"      // Deploy naar GitHub Pages
}
```

## Type Definities

Het project gebruikt strikte TypeScript types (src/types.ts:1):

```typescript
type WOOStatus = "Ontvangen" | "In behandeling" | "Afgerond";

interface WOORequest {
  id: string;
  title: string;
  organization: string;
  requestDate: string;
  decidedDate?: string;
  status: WOOStatus;
  subject: string;
}

interface Event {
  id: string;
  timestamp: Date;
  type: string;
  documentId: string;
  message: string;
}
```

## Browser Support

- Chrome/Edge (laatste 2 versies)
- Firefox (laatste 2 versies)
- Safari (laatste 2 versies)

## Contributing

Contributions zijn welkom! Volg deze stappen:

1. Fork de repository
2. Maak een feature branch (`git checkout -b feature/nieuwe-functie`)
3. Commit je wijzigingen (`git commit -m 'Voeg nieuwe functie toe'`)
4. Push naar de branch (`git push origin feature/nieuwe-functie`)
5. Open een Pull Request

## License

Dit project is open source en beschikbaar onder de MIT License.

## Contact

Voor vragen of suggesties, open een issue op GitHub.

## Changelog

### v1.0.0 (29 oktober 2024)
- Initiele release met complete WOO Dashboard
- Smart event systeem met automatische cleanup
- Real-time status simulatie
- GitHub Pages deployment
- Responsive design
- Live activity feed

---

Gebouwd met ❤️ voor transparantie in de Nederlandse overheid
