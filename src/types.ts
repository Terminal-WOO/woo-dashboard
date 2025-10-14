export type WOOStatus = "Ontvangen" | "In behandeling" | "Afgerond";

export interface WOORequest {
  id: string;
  title: string;
  status: WOOStatus;
  submittedDate: string;
  decidedDate?: string;
  organization: string;
  category: string;
}

export interface StatusEvent {
  id: string;
  requestId: string;
  requestTitle: string;
  previousStatus: WOOStatus | null;
  newStatus: WOOStatus;
  timestamp: Date;
  organization: string;
}

export interface WOOStats {
  totalRequests: number;
  received: number;
  inProgress: number;
  completed: number;
  averageHandlingDays: number;
}

export interface MonthlyData {
  month: string;
  requests: number;
}

export interface StatusDistribution {
  name: string;
  value: number;
  color: string;
}
