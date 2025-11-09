import PDFDocument from 'pdfkit';
import { Readable } from 'stream';

export interface DocumentMetadata {
  title: string;
  type: 'besluit' | 'advies' | 'brief' | 'notitie' | 'rapportage' | 'contract';
  category: string;
  author: string;
  date: Date;
  tags: string[];
  summary: string;
}

const DOCUMENT_TYPES = ['besluit', 'advies', 'brief', 'notitie', 'rapportage', 'contract'] as const;

const CATEGORIES = [
  'Bouwvergunningen',
  'Belastingen',
  'Subsidies',
  'Handhaving',
  'Bezwaarschriften',
  'Financiën',
  'Personeel',
  'Inkoop',
  'Communicatie',
  'Milieu',
];

const AUTHORS = [
  'Jan de Vries',
  'Maria Jansen',
  'Peter van Dam',
  'Lisa Bakker',
  'Tom Visser',
  'Emma de Jong',
  'Lucas Meijer',
  'Sophie van Dijk',
];

const TAGS_POOL = [
  'urgent',
  'openbaar',
  'vertrouwelijk',
  'concept',
  'definitief',
  'archief',
  'woo-verzoek',
  'klacht',
  'subsidie',
  'vergunning',
  'handhaving',
  'bezwaar',
];

const CONTENT_TEMPLATES = {
  besluit: (meta: DocumentMetadata) => `
BESLUIT

Onderwerp: ${meta.title}
Datum: ${meta.date.toLocaleDateString('nl-NL')}
Behandelaar: ${meta.author}
Categorie: ${meta.category}

HET COLLEGE VAN BURGEMEESTER EN WETHOUDERS,

Gelet op:
- De Algemene wet bestuursrecht
- De gemeentewet
- Het ter zake geldende beleid

Overwegende dat:
${meta.summary}

BESLUIT:

1. Het verzoek wordt gehonoreerd zoals omschreven in dit besluit.
2. Dit besluit treedt in werking met ingang van de dag na publicatie.
3. Belanghebbenden kunnen binnen zes weken na verzending van dit besluit een bezwaarschrift indienen.

Aldus besloten op ${meta.date.toLocaleDateString('nl-NL')}

Namens het college,
${meta.author}
`,

  advies: (meta: DocumentMetadata) => `
AMBTELIJK ADVIES

Onderwerp: ${meta.title}
Datum: ${meta.date.toLocaleDateString('nl-NL')}
Adviseur: ${meta.author}
Categorie: ${meta.category}

SAMENVATTING
${meta.summary}

AANLEIDING
Dit advies is opgesteld naar aanleiding van een verzoek tot advisering over bovengenoemd onderwerp.

FEITEN EN OMSTANDIGHEDEN
- Het dossier is volledig en actueel
- Alle relevante belanghebbenden zijn gehoord
- De juridische aspecten zijn onderzocht

BEOORDELING
Op basis van de beschikbare informatie en geldende wet- en regelgeving adviseren wij positief over dit onderwerp.

ADVIES
Wij adviseren om:
1. Het voorliggende voorstel goed te keuren
2. De uitvoering conform planning te laten plaatsvinden
3. De voortgang te monitoren

${meta.author}
${meta.date.toLocaleDateString('nl-NL')}
`,

  brief: (meta: DocumentMetadata) => `
BRIEF

Aan: Belanghebbende
Van: ${meta.author}
Datum: ${meta.date.toLocaleDateString('nl-NL')}
Betreft: ${meta.title}
Categorie: ${meta.category}

Geachte heer/mevrouw,

${meta.summary}

Wij vertrouwen erop u hiermee voldoende te hebben geïnformeerd. Voor vragen kunt u contact opnemen met ondergetekende.

Met vriendelijke groet,

${meta.author}
Afdeling ${meta.category}

Bijlagen: geen
`,

  notitie: (meta: DocumentMetadata) => `
NOTITIE

Onderwerp: ${meta.title}
Datum: ${meta.date.toLocaleDateString('nl-NL')}
Auteur: ${meta.author}
Categorie: ${meta.category}

INLEIDING
${meta.summary}

CONTEXT
Deze notitie geeft een overzicht van de huidige stand van zaken en benodigde vervolgacties.

KERNPUNTEN
- Uitvoering loopt volgens planning
- Budget is toereikend
- Geen bijzonderheden te melden

VERVOLGACTIES
1. Monitoren van de voortgang
2. Rapportage aan het college
3. Evaluatie na afronding

${meta.author}
${meta.date.toLocaleDateString('nl-NL')}
`,

  rapportage: (meta: DocumentMetadata) => `
RAPPORTAGE

Titel: ${meta.title}
Periode: Q${Math.floor(meta.date.getMonth() / 3) + 1} ${meta.date.getFullYear()}
Opgesteld door: ${meta.author}
Categorie: ${meta.category}

MANAGEMENTSAMENVATTING
${meta.summary}

1. INLEIDING
Deze rapportage geeft inzicht in de ontwikkelingen gedurende de verslagperiode.

2. VOORTGANG
De werkzaamheden verlopen conform planning en binnen budget.

3. CIJFERMATIGE ONDERBOUWING
- Target: 100%
- Realisatie: 95%
- Prognose: op schema

4. RISICO'S EN MAATREGELEN
Er zijn geen acute risico's geïdentificeerd.

5. CONCLUSIES EN AANBEVELINGEN
Voortzetting conform huidige planning wordt geadviseerd.

Opgesteld door: ${meta.author}
Datum: ${meta.date.toLocaleDateString('nl-NL')}
`,

  contract: (meta: DocumentMetadata) => `
OVEREENKOMST

Datum: ${meta.date.toLocaleDateString('nl-NL')}
Onderwerp: ${meta.title}
Categorie: ${meta.category}
Contractbeheerder: ${meta.author}

DE ONDERGETEKENDEN:

1. Gemeente, vertegenwoordigd door ${meta.author}
2. Contractpartij, vertegenwoordigd door [naam]

OVERWEGEN:

${meta.summary}

KOMEN HET VOLGENDE OVEREEN:

Artikel 1 - Definities
In deze overeenkomst wordt verstaan onder:
a) Opdrachtgever: de gemeente
b) Opdrachtnemer: de contractpartij

Artikel 2 - Onderwerp
De opdrachtnemer voert uit zoals omschreven in deze overeenkomst.

Artikel 3 - Looptijd
Deze overeenkomst heeft een looptijd van 12 maanden.

Artikel 4 - Vergoeding
De overeengekomen vergoeding bedraagt € [bedrag] exclusief BTW.

Artikel 5 - Toepasselijk recht
Op deze overeenkomst is Nederlands recht van toepassing.

Aldus overeengekomen en getekend te [plaats] op ${meta.date.toLocaleDateString('nl-NL')}

Gemeente                    Contractpartij
${meta.author}              [Naam]
`,
};

export function generateRandomMetadata(): DocumentMetadata {
  const type = DOCUMENT_TYPES[Math.floor(Math.random() * DOCUMENT_TYPES.length)];
  const category = CATEGORIES[Math.floor(Math.random() * CATEGORIES.length)];
  const author = AUTHORS[Math.floor(Math.random() * AUTHORS.length)];

  // Random date in last 2 years
  const daysAgo = Math.floor(Math.random() * 730);
  const date = new Date();
  date.setDate(date.getDate() - daysAgo);

  // Random tags (2-4)
  const numTags = 2 + Math.floor(Math.random() * 3);
  const tags = [];
  const shuffled = [...TAGS_POOL].sort(() => 0.5 - Math.random());
  for (let i = 0; i < numTags; i++) {
    tags.push(shuffled[i]);
  }

  const typeLabels = {
    besluit: 'Besluit',
    advies: 'Advies',
    brief: 'Brief',
    notitie: 'Notitie',
    rapportage: 'Rapportage',
    contract: 'Contract',
  };

  return {
    title: `${typeLabels[type]} ${category} ${date.getFullYear()}-${String(date.getMonth() + 1).padStart(2, '0')}-${String(date.getDate()).padStart(2, '0')}`,
    type,
    category,
    author,
    date,
    tags,
    summary: `Dit is een ${typeLabels[type].toLowerCase()} betreffende ${category.toLowerCase()}. Het document behandelt een standaard proces binnen deze categorie.`,
  };
}

export async function generatePDF(metadata: DocumentMetadata): Promise<Buffer> {
  return new Promise((resolve, reject) => {
    const doc = new PDFDocument({
      size: 'A4',
      margins: { top: 50, bottom: 50, left: 50, right: 50 },
    });

    const chunks: Buffer[] = [];
    doc.on('data', (chunk) => chunks.push(chunk));
    doc.on('end', () => resolve(Buffer.concat(chunks)));
    doc.on('error', reject);

    // Header
    doc.fontSize(20).text(metadata.title, { align: 'center' });
    doc.moveDown();

    // Metadata
    doc.fontSize(10);
    doc.text(`Type: ${metadata.type}`, { continued: true });
    doc.text(`    Categorie: ${metadata.category}`);
    doc.text(`Auteur: ${metadata.author}`, { continued: true });
    doc.text(`    Datum: ${metadata.date.toLocaleDateString('nl-NL')}`);
    doc.text(`Tags: ${metadata.tags.join(', ')}`);
    doc.moveDown();

    // Separator
    doc.moveTo(50, doc.y).lineTo(545, doc.y).stroke();
    doc.moveDown();

    // Content
    doc.fontSize(11);
    const content = CONTENT_TEMPLATES[metadata.type](metadata);
    doc.text(content, { align: 'justify' });

    // Footer
    doc.fontSize(8);
    const pageCount = doc.bufferedPageRange().count;
    for (let i = 0; i < pageCount; i++) {
      doc.switchToPage(i);
      doc.text(
        `Pagina ${i + 1} van ${pageCount}`,
        50,
        doc.page.height - 50,
        { align: 'center' }
      );
    }

    doc.end();
  });
}
