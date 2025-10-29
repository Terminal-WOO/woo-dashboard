// Extended status system for government organizations
export type WOOStatus =
  | "Ontvangen"
  | "In behandeling"
  | "1e Concept"
  | "2e Concept"
  | "Definitief"
  | "Gepubliceerd"
  | "Afgerond";

export type OrganizationType = "gemeente" | "provincie" | "ministerie";

export interface WOORequest {
  id: string;
  title: string;
  status: WOOStatus;
  submittedDate: string;
  decidedDate?: string;
  organization: string;
  organizationType: OrganizationType;
  category: string;
  subject: string;
  requester?: string;
  handler?: string;
  lastModified: string;
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

export interface Organization {
  id: string;
  name: string;
  type: OrganizationType;
  statusWorkflow: WOOStatus[];
}
