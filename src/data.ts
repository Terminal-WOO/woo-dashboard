import {
  WOORequest,
  WOOStats,
  MonthlyData,
  StatusDistribution,
  WOOStatus,
} from "./types";

// Mock data - will be replaced by database
export const mockWOORequests: WOORequest[] = [];

// Calculate statistics with new status types
export const calculateStats = (requests: WOORequest[]): WOOStats => {
  const received = requests.filter((r) => r.status === "Ontvangen").length;

  const inProgress = requests.filter(
    (r) =>
      r.status === "In behandeling" ||
      r.status === "1e Concept" ||
      r.status === "2e Concept" ||
      r.status === "Definitief",
  ).length;

  const completed = requests.filter((r) => r.status === "Gepubliceerd").length;

  // Calculate average handling time
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

// Generate monthly data
export const getMonthlyData = (): MonthlyData[] => {
  return [
    { month: "Jan", requests: 12 },
    { month: "Feb", requests: 15 },
    { month: "Mrt", requests: 18 },
    { month: "Apr", requests: 14 },
    { month: "Mei", requests: 16 },
    { month: "Jun", requests: 19 },
    { month: "Jul", requests: 17 },
    { month: "Aug", requests: 13 },
    { month: "Sep", requests: 20 },
    { month: "Okt", requests: 22 },
  ];
};

// Get status distribution with new statuses
export const getStatusDistribution = (
  stats: WOOStats,
): StatusDistribution[] => {
  return [
    { name: "Ontvangen", value: stats.received, color: "#d97706" },
    { name: "In Behandeling", value: stats.inProgress, color: "#107abe" },
    { name: "Gepubliceerd", value: stats.completed, color: "#2e7d32" },
  ];
};

// Get detailed status distribution including all workflow steps
export const getDetailedStatusDistribution = (
  requests: WOORequest[],
): StatusDistribution[] => {
  const statusCounts: Record<WOOStatus, number> = {
    Ontvangen: 0,
    "In behandeling": 0,
    "1e Concept": 0,
    "2e Concept": 0,
    Definitief: 0,
    Gepubliceerd: 0,
    Afgerond: 0,
  };

  requests.forEach((req) => {
    statusCounts[req.status]++;
  });

  const statusColors: Record<WOOStatus, string> = {
    Ontvangen: "#d97706",
    "In behandeling": "#107abe",
    "1e Concept": "#9333ea",
    "2e Concept": "#db2777",
    Definitief: "#0891b2",
    Gepubliceerd: "#2e7d32",
    Afgerond: "#16a34a",
  };

  return Object.entries(statusCounts)
    .filter(([_, count]) => count > 0)
    .map(([status, count]) => ({
      name: status,
      value: count,
      color: statusColors[status as WOOStatus],
    }));
};

// Get organization statistics
export const getOrganizationStats = (
  requests: WOORequest[],
): { name: string; count: number; color: string }[] => {
  const orgCounts: Record<string, number> = {};

  requests.forEach((req) => {
    orgCounts[req.organization] = (orgCounts[req.organization] || 0) + 1;
  });

  const orgColors: Record<string, string> = {
    "Gemeente Utrecht": "#E30613",
    "Provincie Flevoland": "#00A0E1",
  };

  return Object.entries(orgCounts).map(([org, count]) => ({
    name: org,
    count,
    color: orgColors[org] || "#6b7280",
  }));
};
