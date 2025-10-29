import { WOORequest, OrganizationType } from "./types";

const subjects = [
  "gemeentelijke uitgaven",
  "nieuwe wetgeving",
  "bouwvergunningen",
  "milieu-impact rapportage",
  "gezondheidszorg beleid",
  "onderwijsbeleid",
  "verkeerscijfers",
  "sociale voorzieningen",
  "openbare werken",
  "externe leveranciers",
  "digitalisering",
  "duurzaamheidsplannen",
  "cultuursubsidies",
  "horeca vergunningen",
  "gemeenteraad besluiten",
  "infrastructuurprojecten",
  "woningbouwplannen",
  "energietransitie",
  "ruimtelijke ordening",
  "volksgezondheid",
  "klimaatadaptatie",
  "openbare veiligheid",
  "sportvoorzieningen",
  "natuur en landschap",
  "afvalverwerking",
  "waterhuishouding",
  "monumentenbescherming",
  "jeugdzorg",
  "participatie burgers",
  "economische ontwikkeling",
];

const prefixes = [
  "Verzoek om informatie over",
  "Documenten betreffende",
  "Informatie over",
  "Rapporten over",
  "Advies over",
  "Besluitvorming over",
  "Beleidsnotities over",
  "Contracten met betrekking tot",
  "Plannen voor",
  "Subsidieaanvragen voor",
];

// Updated for Utrecht and Flevoland
const organizations: Array<{ name: string; type: OrganizationType }> = [
  { name: "Gemeente Utrecht", type: "gemeente" },
  { name: "Provincie Flevoland", type: "provincie" },
];

const categories = [
  "Financiën",
  "Wetgeving",
  "Ruimtelijke Ordening",
  "Milieu",
  "Gezondheidszorg",
  "Onderwijs",
  "Verkeer",
  "Sociale Zaken",
  "Cultuur",
  "Economie",
  "Duurzaamheid",
  "Infrastructuur",
  "Landbouw",
  "Natuur",
  "Energie",
];

let documentCounter = 1000;

export function generateNewDocument(): WOORequest {
  documentCounter++;

  // Generate unique title
  const randomPrefix = prefixes[Math.floor(Math.random() * prefixes.length)];
  const randomSubject = subjects[Math.floor(Math.random() * subjects.length)];
  const uniqueTitle = `${randomPrefix} ${randomSubject}`;

  const randomOrg =
    organizations[Math.floor(Math.random() * organizations.length)];
  const randomCategory =
    categories[Math.floor(Math.random() * categories.length)];

  // Date between now and 30 days ago
  const daysAgo = Math.floor(Math.random() * 30);
  const submittedDate = new Date();
  submittedDate.setDate(submittedDate.getDate() - daysAgo);

  return {
    id: `WOO-GEN-${documentCounter}`,
    title: uniqueTitle,
    status: "Ontvangen",
    submittedDate: submittedDate.toISOString().split("T")[0],
    organization: randomOrg.name,
    organizationType: randomOrg.type,
    category: randomCategory,
    subject: `${randomPrefix} ${randomSubject} - gegenereerd document`,
    lastModified: new Date().toISOString(),
  };
}
