import { useState, useEffect } from "react";
import { StatusEvent, WOOStatus } from "../types";
import { eventSystem } from "../eventSystem";

export const ActivityFeed = () => {
  const [events, setEvents] = useState<StatusEvent[]>([]);

  useEffect(() => {
    // Laad initiële events
    setEvents(eventSystem.getRecentEvents(10));

    // Subscribe to nieuwe events
    const unsubscribe = eventSystem.subscribe((event) => {
      setEvents((prevEvents) => [event, ...prevEvents].slice(0, 10));
    });

    return unsubscribe;
  }, []);

  const getStatusColor = (status: WOOStatus): string => {
    switch (status) {
      case "Ontvangen":
        return "#d97706";
      case "In behandeling":
        return "#107abe";
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
      case "Afgerond":
        return "✅";
      default:
        return "📄";
    }
  };

  const formatTime = (date: Date): string => {
    const now = new Date();
    const diffMs = now.getTime() - date.getTime();
    const diffSecs = Math.floor(diffMs / 1000);
    const diffMins = Math.floor(diffSecs / 60);
    const diffHours = Math.floor(diffMins / 60);

    if (diffSecs < 60) return "Zojuist";
    if (diffMins < 60) return `${diffMins} min geleden`;
    if (diffHours < 24) return `${diffHours} uur geleden`;

    return date.toLocaleDateString("nl-NL", {
      day: "numeric",
      month: "short",
      hour: "2-digit",
      minute: "2-digit",
    });
  };

  if (events.length === 0) {
    return (
      <div className="activity-feed">
        <h2>Recente Activiteit</h2>
        <div className="no-events">
          <p>Geen recente activiteit</p>
        </div>
      </div>
    );
  }

  return (
    <div className="activity-feed">
      <h2>Recente Activiteit</h2>
      <div className="events-list">
        {events.map((event) => (
          <div key={event.id} className="event-item">
            <div
              className="event-icon"
              style={{
                backgroundColor: `${getStatusColor(event.newStatus)}20`,
              }}
            >
              <span>{getStatusIcon(event.newStatus)}</span>
            </div>
            <div className="event-content">
              <div className="event-header">
                <span className="event-title">{event.requestTitle}</span>
                <span className="event-time">
                  {formatTime(event.timestamp)}
                </span>
              </div>
              <div className="event-details">
                <span className="event-org">{event.organization}</span>
                {event.previousStatus && (
                  <>
                    <span className="event-separator">•</span>
                    <span className="status-change">
                      <span
                        style={{ color: getStatusColor(event.previousStatus) }}
                      >
                        {event.previousStatus}
                      </span>
                      <span className="arrow">→</span>
                      <span style={{ color: getStatusColor(event.newStatus) }}>
                        {event.newStatus}
                      </span>
                    </span>
                  </>
                )}
                {!event.previousStatus && (
                  <>
                    <span className="event-separator">•</span>
                    <span style={{ color: getStatusColor(event.newStatus) }}>
                      {event.newStatus}
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
