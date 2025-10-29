import { useState, useEffect } from "react";
import { WOOStatus } from "../types";
import { erlangSystem, Message } from "../erlangActorSystem";

interface ActivityEvent {
  id: string;
  timestamp: Date;
  requestTitle: string;
  organization: string;
  previousStatus: WOOStatus | null;
  newStatus: WOOStatus;
}

export const ActivityFeed = () => {
  const [events, setEvents] = useState<ActivityEvent[]>([]);

  useEffect(() => {
    // Load initial events from Erlang event manager history
    const history = erlangSystem.getEventManager().getHistory();
    const initialEvents = history
      .filter(
        (msg) => msg.type === "status_change" || msg.type === "new_document",
      )
      .slice(-10)
      .reverse()
      .map(convertMessageToEvent);
    setEvents(initialEvents);

    // Create subscriber actor for live events
    const subscriber = erlangSystem.spawn((message: Message, state: any) => {
      if (message.type === "status_change") {
        const event = convertMessageToEvent(message);
        setEvents((prevEvents) => [event, ...prevEvents].slice(0, 10));
      } else if (message.type === "new_document") {
        const event = convertMessageToEvent(message);
        setEvents((prevEvents) => [event, ...prevEvents].slice(0, 10));
      }
      return state;
    });

    // Register as event handler
    erlangSystem.getEventManager().addHandler(subscriber);

    return () => {
      // Cleanup: remove handler on unmount
      erlangSystem.getEventManager().removeHandler(subscriber);
      subscriber.send({ type: "shutdown" });
    };
  }, []);

  const convertMessageToEvent = (message: Message): ActivityEvent => {
    if (message.type === "status_change") {
      return {
        id: `${message.data.documentId}-${message.data.timestamp}`,
        timestamp: new Date(message.data.timestamp),
        requestTitle: message.data.title,
        organization: message.data.organization,
        previousStatus: message.data.oldStatus as WOOStatus,
        newStatus: message.data.newStatus as WOOStatus,
      };
    } else if (message.type === "new_document") {
      return {
        id: `${message.data.id}-${message.data.timestamp}`,
        timestamp: new Date(message.data.timestamp),
        requestTitle: message.data.title,
        organization: message.data.organization,
        previousStatus: null,
        newStatus: message.data.status as WOOStatus,
      };
    }
    throw new Error("Invalid message type");
  };

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
        <h2>Recente Activiteit (Erlang Actor System)</h2>
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
      <h2>Recente Activiteit (Erlang Actor System)</h2>
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
                      Nieuw: {event.newStatus}
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
