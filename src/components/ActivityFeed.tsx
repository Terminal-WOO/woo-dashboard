import { WOOStatus, WOORequest } from "../types";
import { OrganizationLogo } from "./OrganizationLogo";

interface ActivityFeedProps {
  requests: WOORequest[];
}

export const ActivityFeed = ({ requests }: ActivityFeedProps) => {
  // Show the 10 most recently modified documents
  const recentChanges = [...requests]
    .sort(
      (a, b) =>
        new Date(b.lastModified).getTime() - new Date(a.lastModified).getTime(),
    )
    .slice(0, 10);

  console.log("[ActivityFeed] Total requests:", requests.length);
  console.log("[ActivityFeed] Recent changes:", recentChanges.length);
  console.log(
    "[ActivityFeed] With previousStatus:",
    recentChanges.filter((r) => r.previousStatus).length,
  );
  const getStatusColor = (status: WOOStatus): string => {
    switch (status) {
      case "Ontvangen":
        return "#d97706";
      case "In behandeling":
        return "#107abe";
      case "1e Concept":
        return "#9333ea";
      case "2e Concept":
        return "#db2777";
      case "Definitief":
        return "#0891b2";
      case "Gepubliceerd":
        return "#16a34a";
      case "Afgerond":
        return "#2e7d32";
      default:
        return "#6b7280";
    }
  };

  const getStatusIcon = (status: WOOStatus): string => {
    switch (status) {
      case "Ontvangen":
        return "📥";
      case "In behandeling":
        return "⏳";
      case "1e Concept":
        return "📝";
      case "2e Concept":
        return "📋";
      case "Definitief":
        return "✔️";
      case "Gepubliceerd":
        return "📢";
      case "Afgerond":
        return "✅";
      default:
        return "📄";
    }
  };

  const formatTime = (dateString: string): string => {
    const date = new Date(dateString);
    const now = new Date();
    const diffMs = now.getTime() - date.getTime();
    const diffSecs = Math.floor(diffMs / 1000);
    const diffMins = Math.floor(diffSecs / 60);

    if (diffSecs < 10) return "Zojuist";
    if (diffSecs < 60) return `${diffSecs}s geleden`;
    if (diffMins < 60) return `${diffMins}m geleden`;

    return date.toLocaleDateString("nl-NL", {
      day: "numeric",
      month: "short",
      hour: "2-digit",
      minute: "2-digit",
    });
  };

  if (recentChanges.length === 0) {
    return (
      <div className="activity-feed">
        <h2>Recente Activiteit</h2>
        <div className="no-events">
          <p>Geen recente activiteit</p>
          <p style={{ fontSize: "0.8em", color: "#6b7280", marginTop: "8px" }}>
            💡 Start de simulatie om events te zien
          </p>
        </div>
      </div>
    );
  }

  return (
    <div className="activity-feed">
      <h2>Recente Activiteit</h2>
      <div className="events-list">
        {recentChanges.map((request) => (
          <div
            key={`${request.id}-${request.lastModified}`}
            className="event-item"
          >
            <div
              className="event-icon"
              style={{
                backgroundColor: `${getStatusColor(request.status)}20`,
              }}
            >
              <span>{getStatusIcon(request.status)}</span>
            </div>
            <div className="event-content">
              <div className="event-header">
                <span className="event-title">{request.title}</span>
                <span className="event-time">
                  {formatTime(request.lastModified)}
                </span>
              </div>
              <div className="event-details">
                <span className="event-org">
                  <OrganizationLogo
                    organization={request.organization}
                    size={20}
                    className="org-logo-inline"
                  />
                  {request.organization}
                </span>
                {request.previousStatus && (
                  <>
                    <span className="event-separator">•</span>
                    <span className="status-change">
                      <span
                        style={{
                          color: getStatusColor(request.previousStatus),
                        }}
                      >
                        {request.previousStatus}
                      </span>
                      <span className="arrow"> → </span>
                      <span style={{ color: getStatusColor(request.status) }}>
                        {request.status}
                      </span>
                    </span>
                  </>
                )}
                {!request.previousStatus && (
                  <>
                    <span className="event-separator">•</span>
                    <span style={{ color: getStatusColor(request.status) }}>
                      Status: {request.status}
                    </span>
                  </>
                )}
              </div>
            </div>
          </div>
        ))}
      </div>
    </div>
  );
};
