import { StatusEvent, WOOStatus } from './types';

type EventListener = (event: StatusEvent) => void;

class EventSystem {
  private listeners: EventListener[] = [];
  private events: StatusEvent[] = [];

  subscribe(listener: EventListener): () => void {
    this.listeners.push(listener);

    // Return unsubscribe function
    return () => {
      this.listeners = this.listeners.filter(l => l !== listener);
    };
  }

  emit(event: StatusEvent): void {
    this.events.push(event);
    this.listeners.forEach(listener => listener(event));
  }

  getRecentEvents(limit: number = 10): StatusEvent[] {
    return [...this.events]
      .sort((a, b) => b.timestamp.getTime() - a.timestamp.getTime())
      .slice(0, limit);
  }

  getAllEvents(): StatusEvent[] {
    return [...this.events].sort((a, b) => b.timestamp.getTime() - a.timestamp.getTime());
  }

  createEvent(
    requestId: string,
    requestTitle: string,
    previousStatus: WOOStatus | null,
    newStatus: WOOStatus,
    organization: string
  ): StatusEvent {
    return {
      id: crypto.randomUUID(),
      requestId,
      requestTitle,
      previousStatus,
      newStatus,
      timestamp: new Date(),
      organization
    };
  }
}

export const eventSystem = new EventSystem();
