const logoBase = `${import.meta.env.BASE_URL}logos/`;

/** Officiële logo's (Wikimedia Commons / provinciale huisstijlgids) */
export const ORGANIZATION_LOGOS: Record<string, string> = {
  "Ministerie van Justitie en Veiligheid": `${logoBase}minjenv.png`,
  "Ministerie van Financiën": `${logoBase}minfin.png`,
  "Provincie Zuid-Holland": `${logoBase}zuid-holland.svg`,
  "Provincie Flevoland": `${logoBase}flevoland.svg`,
};

export function getOrganizationLogoUrl(organization: string): string | undefined {
  return ORGANIZATION_LOGOS[organization];
}
