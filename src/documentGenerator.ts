import { WOORequest } from "./types";

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

const organizations = [
  "Gemeente Amsterdam",
  "Gemeente Rotterdam",
  "Gemeente Den Haag",
  "Gemeente Utrecht",
  "Ministerie van Justitie",
  "Ministerie van Infrastructuur",
  "Ministerie van VWS",
  "Ministerie van Onderwijs",
  "Ministerie van Financiën",
  "Provincie Noord-Holland",
  "Waterschap Rijnland",
  "Gemeente Eindhoven",
  "Gemeente Groningen",
  "Gemeente Tilburg",
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
];

let documentCounter = 1000;

export function generateNewDocument(): WOORequest {
  documentCounter++;

  // Genereer unieke titel door prefix en subject te combineren
  const randomPrefix = prefixes[Math.floor(Math.random() * prefixes.length)];
  const randomSubject = subjects[Math.floor(Math.random() * subjects.length)];
  const uniqueTitle = `${randomPrefix} ${randomSubject}`;

  const randomOrg =
    organizations[Math.floor(Math.random() * organizations.length)];
  const randomCategory =
    categories[Math.floor(Math.random() * categories.length)];

  // Datum tussen nu en 30 dagen geleden
  const daysAgo = Math.floor(Math.random() * 30);
  const submittedDate = new Date();
  submittedDate.setDate(submittedDate.getDate() - daysAgo);

  return {
    id: `doc-${documentCounter}`,
    title: uniqueTitle,
    status: "Ontvangen",
    submittedDate: submittedDate.toISOString().split("T")[0],
    organization: randomOrg,
    category: randomCategory,
  };
}
