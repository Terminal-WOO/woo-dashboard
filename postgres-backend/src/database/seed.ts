import { Pool } from 'pg';

const organizations = [
  {
    name: 'Gemeente Utrecht',
    code: 'GMU',
    type: 'municipality'
  },
  {
    name: 'Provincie Flevoland',
    code: 'PFL',
    type: 'province'
  }
];

const documents = [
  // Gemeente Utrecht documents
  {
    title: 'Bestemmingsplan Binnenstad',
    subject: 'Herziening van het bestemmingsplan voor de binnenstad',
    organization: 'Gemeente Utrecht',
    status: 'ontvangen',
    priority: 'high',
    metadata: { theme: 'ruimtelijke_ordening', department: 'Ruimte' }
  },
  {
    title: 'Vergunning Evenement Jaarbeurs',
    subject: 'Aanvraag evenementenvergunning Jaarbeurs terrein',
    organization: 'Gemeente Utrecht',
    status: 'in_behandeling',
    priority: 'medium',
    metadata: { theme: 'evenementen', department: 'Vergunningen' }
  },
  {
    title: 'Bouwvergunning Stationsgebied',
    subject: 'Nieuwe ontwikkeling station Utrecht Centraal',
    organization: 'Gemeente Utrecht',
    status: 'aanvullende_info',
    priority: 'high',
    metadata: { theme: 'ruimtelijke_ordening', department: 'Bouw' }
  },
  {
    title: 'Subsidieaanvraag Sport Utrecht',
    subject: 'Aanvraag subsidie voor sportverenigingen',
    organization: 'Gemeente Utrecht',
    status: 'beslissing',
    priority: 'low',
    metadata: { theme: 'sport_en_recreatie', department: 'Sport' }
  },
  {
    title: 'Verkeersplan Merwede',
    subject: 'Verkeersplan voor nieuwe wijk Merwedekanaalzone',
    organization: 'Gemeente Utrecht',
    status: 'besluit_genomen',
    priority: 'medium',
    metadata: { theme: 'verkeer', department: 'Mobiliteit' }
  },
    {
    title: 'Handhaving Parkeren Centrum',
    subject: 'Controle op illegaal parkeren binnenstad',
    organization: 'Gemeente Utrecht',
    status: 'afgehandeld',
    priority: 'low',
    metadata: { theme: 'handhaving', department: 'Toezicht' }
  },
  {
    title: 'Kapvergunning Griftpark',
    subject: 'Aanvraag voor kappen bomen Griftpark',
    organization: 'Gemeente Utrecht',
    status: 'ontvangen',
    priority: 'medium',
    metadata: { theme: 'milieu', department: 'Groen' }
  },
  {
    title: 'Omgevingsvergunning Winkelcentrum',
    subject: 'Uitbreiding winkelcentrum Kanaleneiland',
    organization: 'Gemeente Utrecht',
    status: 'in_behandeling',
    priority: 'high',
    metadata: { theme: 'ruimtelijke_ordening', department: 'Vergunningen' }
  },
  {
    title: 'Horecavergunning Oudegracht',
    subject: 'Nieuwe horecagelegenheid aan de Oudegracht',
    organization: 'Gemeente Utrecht',
    status: 'aanvullende_info',
    priority: 'medium',
    metadata: { theme: 'horeca', department: 'Vergunningen' }
  },
  {
    title: 'Subsidie Duurzaamheid',
    subject: 'Subsidieregeling verduurzaming woningen',
    organization: 'Gemeente Utrecht',
    status: 'beslissing',
    priority: 'medium',
    metadata: { theme: 'duurzaamheid', department: 'Klimaat' }
  },
  {
    title: 'Monumentenvergunning Domkerk',
    subject: 'Restauratie Domkerk Utrecht',
    organization: 'Gemeente Utrecht',
    status: 'besluit_genomen',
    priority: 'high',
    metadata: { theme: 'monumenten', department: 'Erfgoed' }
  },
  {
    title: 'Geluidsmeting Snelweg',
    subject: 'Onderzoek geluidsoverlast A27',
    organization: 'Gemeente Utrecht',
    status: 'afgehandeld',
    priority: 'low',
    metadata: { theme: 'milieu', department: 'Leefomgeving' }
  },

  // Provincie Flevoland documents
  {
    title: 'Natuurontwikkeling Oostvaardersplassen',
    subject: 'Plan voor natuurontwikkeling gebied Oostvaardersplassen',
    organization: 'Provincie Flevoland',
    status: 'ontvangen',
    priority: 'high',
    metadata: { theme: 'natuur', department: 'Natuur en Milieu' }
  },
  {
    title: 'Vergunning Windpark Noordoostpolder',
    subject: 'Omgevingsvergunning windmolenpark',
    organization: 'Provincie Flevoland',
    status: 'in_behandeling',
    priority: 'high',
    metadata: { theme: 'duurzaamheid', department: 'Energie' }
  },
  {
    title: 'Subsidie Agrarisch Natuurbeheer',
    subject: 'Subsidieregeling voor agrarisch natuurbeheer',
    organization: 'Provincie Flevoland',
    status: 'aanvullende_info',
    priority: 'medium',
    metadata: { theme: 'landbouw', department: 'Landbouw' }
  },
  {
    title: 'Watervergunning Markermeer',
    subject: 'Vergunning voor waterberging Markermeer',
    organization: 'Provincie Flevoland',
    status: 'beslissing',
    priority: 'high',
    metadata: { theme: 'water', department: 'Water' }
  },
  {
    title: 'Infrastructuurplan N23',
    subject: 'Verbreding provinciale weg N23',
    organization: 'Provincie Flevoland',
    status: 'besluit_genomen',
    priority: 'high',
    metadata: { theme: 'verkeer', department: 'Infrastructuur' }
  },
  {
    title: 'Handhaving Mestbeleid',
    subject: 'Controle naleving mestbeleid landbouw',
    organization: 'Provincie Flevoland',
    status: 'afgehandeld',
    priority: 'medium',
    metadata: { theme: 'handhaving', department: 'Handhaving' }
  },
  {
    title: 'Vergunning Zonnepark Zeewolde',
    subject: 'Omgevingsvergunning grootschalig zonnepark',
    organization: 'Provincie Flevoland',
    status: 'ontvangen',
    priority: 'high',
    metadata: { theme: 'duurzaamheid', department: 'Energie' }
  },
  {
    title: 'Natuurcompensatie A6',
    subject: 'Natuurcompensatie voor verbreding snelweg',
    organization: 'Provincie Flevoland',
    status: 'in_behandeling',
    priority: 'medium',
    metadata: { theme: 'natuur', department: 'Natuur en Milieu' }
  },
  {
    title: 'Subsidie Recreatiegebied',
    subject: 'Ontwikkeling recreatiegebied Lelystad',
    organization: 'Provincie Flevoland',
    status: 'aanvullende_info',
    priority: 'low',
    metadata: { theme: 'recreatie', department: 'Recreatie' }
  },
  {
    title: 'Grondwatervergunning Bedrijventerrein',
    subject: 'Vergunning voor grondwateronttrekking',
    organization: 'Provincie Flevoland',
    status: 'beslissing',
    priority: 'medium',
    metadata: { theme: 'water', department: 'Water' }
  },
  {
    title: 'Milieueffectrapportage Datacenter',
    subject: 'MER voor nieuw datacenter Almere',
    organization: 'Provincie Flevoland',
    status: 'besluit_genomen',
    priority: 'high',
    metadata: { theme: 'milieu', department: 'Milieu' }
  },
  {
    title: 'Handhaving Natuurbeschermingswet',
    subject: 'Controle naleving natuurwetgeving',
    organization: 'Provincie Flevoland',
    status: 'afgehandeld',
    priority: 'low',
    metadata: { theme: 'handhaving', department: 'Handhaving' }
  }
];

export async function seed(pool: Pool): Promise<void> {
  const client = await pool.connect();

  try {
    console.log('Seeding database...');

    await client.query('BEGIN');

    // Check if data already exists
    const { rows } = await client.query('SELECT COUNT(*) FROM organizations');
    if (parseInt(rows[0].count) > 0) {
      console.log('Database already seeded, skipping...');
      await client.query('ROLLBACK');
      return;
    }

    // Insert organizations
    for (const org of organizations) {
      await client.query(
        `INSERT INTO organizations (name, code, type) VALUES ($1, $2, $3)`,
        [org.name, org.code, org.type]
      );
    }
    console.log(`✓ Inserted ${organizations.length} organizations`);

    // Insert documents
    for (const doc of documents) {
      await client.query(
        `INSERT INTO woo_requests
         (title, subject, organization, status, priority, metadata)
         VALUES ($1, $2, $3, $4, $5, $6)`,
        [doc.title, doc.subject, doc.organization, doc.status, doc.priority, doc.metadata]
      );
    }
    console.log(`✓ Inserted ${documents.length} WOO requests`);

    // Refresh materialized view
    await client.query('REFRESH MATERIALIZED VIEW statistics_summary');
    console.log('✓ Refreshed statistics view');

    await client.query('COMMIT');
    console.log('✓ Database seeding completed successfully');
  } catch (error) {
    await client.query('ROLLBACK');
    console.error('✗ Seeding failed:', error);
    throw error;
  } finally {
    client.release();
  }
}
