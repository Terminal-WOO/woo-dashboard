import { WOORequest, WOOStats, MonthlyData, StatusDistribution } from "./types";

// Mock data voor WOO verzoeken
export const mockWOORequests: WOORequest[] = [
  {
    id: "1",
    title: "Verzoek om informatie over gemeentelijke uitgaven",
    status: "Ontvangen",
    submittedDate: "2024-01-15",
    organization: "Gemeente Amsterdam",
    category: "Financiën",
  },
  {
    id: "2",
    title: "Documenten over nieuwe wetgeving",
    status: "In behandeling",
    submittedDate: "2024-03-10",
    organization: "Ministerie van Justitie",
    category: "Wetgeving",
  },
  {
    id: "3",
    title: "Informatie over bouwvergunningen",
    status: "In behandeling",
    submittedDate: "2024-02-05",
    organization: "Gemeente Rotterdam",
    category: "Ruimtelijke Ordening",
  },
  {
    id: "4",
    title: "Verzoek om milieu-impact rapportage",
    status: "Ontvangen",
    submittedDate: "2024-01-20",
    organization: "Ministerie van Infrastructuur",
    category: "Milieu",
  },
  {
    id: "5",
    title: "Adviesrapporten over gezondheidszorg",
    status: "In behandeling",
    submittedDate: "2024-03-01",
    organization: "Ministerie van VWS",
    category: "Gezondheidszorg",
  },
  {
    id: "6",
    title: "Informatie over onderwijsbeleid",
    status: "In behandeling",
    submittedDate: "2024-03-20",
    organization: "Ministerie van Onderwijs",
    category: "Onderwijs",
  },
  {
    id: "7",
    title: "Verkeerscijfers en verkeersplannen",
    status: "Ontvangen",
    submittedDate: "2024-02-10",
    organization: "Gemeente Utrecht",
    category: "Verkeer",
  },
  {
    id: "8",
    title: "Informatie over sociale voorzieningen",
    status: "In behandeling",
    submittedDate: "2024-01-05",
    organization: "Gemeente Den Haag",
    category: "Sociale Zaken",
  },
];

// Bereken statistieken
export const calculateStats = (requests: WOORequest[]): WOOStats => {
  const received = requests.filter((r) => r.status === "Ontvangen").length;
  const inProgress = requests.filter(
    (r) => r.status === "In behandeling",
  ).length;
  const completed = requests.filter((r) => r.status === "Afgerond").length;

  // Bereken gemiddelde behandeltijd
  const completedRequests = requests.filter((r) => r.decidedDate);
  const totalDays = completedRequests.reduce((sum, req) => {
    const submitted = new Date(req.submittedDate);
    const decided = new Date(req.decidedDate!);
    const days = Math.floor(
      (decided.getTime() - submitted.getTime()) / (1000 * 60 * 60 * 24),
    );
    return sum + days;
  }, 0);

  const averageHandlingDays =
    completedRequests.length > 0
      ? Math.round(totalDays / completedRequests.length)
      : 0;

  return {
    totalRequests: requests.length,
    received,
    inProgress,
    completed,
    averageHandlingDays,
  };
};

// Data per maand
export const getMonthlyData = (): MonthlyData[] => {
  return [
    { month: "Jan", requests: 3 },
    { month: "Feb", requests: 2 },
    { month: "Mrt", requests: 3 },
    { month: "Apr", requests: 5 },
    { month: "Mei", requests: 4 },
    { month: "Jun", requests: 6 },
  ];
};

// Status verdeling voor pie chart
export const getStatusDistribution = (
  stats: WOOStats,
): StatusDistribution[] => {
  return [
    { name: "Ontvangen", value: stats.received, color: "#d97706" },
    { name: "In behandeling", value: stats.inProgress, color: "#107abe" },
    { name: "Afgerond", value: stats.completed, color: "#2e7d32" },
  ];
};
