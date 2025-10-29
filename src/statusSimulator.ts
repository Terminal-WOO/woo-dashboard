import { WOORequest, WOOStatus } from "./types";
import { eventSystem } from "./eventSystem";
import { generateNewDocument } from "./documentGenerator";

const statusProgression: WOOStatus[] = [
  "Ontvangen",
  "In behandeling",
  "Afgerond",
];

// Maximaal aantal documenten in het dashboard
const MAX_DOCUMENTS = 12;

export class StatusSimulator {
  private intervalId: number | null = null;
  private isRunning = false;
  private currentRequests: WOORequest[] = [];
  private updateCallback: ((requests: WOORequest[]) => void) | null = null;

  start(
    requests: WOORequest[],
    onUpdate: (requests: WOORequest[]) => void,
    intervalMs: number = 5000,
  ): void {
    if (this.isRunning) return;

    this.isRunning = true;
    this.currentRequests = requests;
    this.updateCallback = onUpdate;

    this.intervalId = window.setInterval(() => {
      const updatedRequests = this.updateRandomRequest(this.currentRequests);
      if (updatedRequests) {
        this.currentRequests = updatedRequests;
        this.updateCallback!(updatedRequests);
      }
    }, intervalMs);
  }

  stop(): void {
    if (this.intervalId) {
      clearInterval(this.intervalId);
      this.intervalId = null;
      this.isRunning = false;
      this.currentRequests = [];
      this.updateCallback = null;
    }
  }

  private updateRandomRequest(requests: WOORequest[]): WOORequest[] | null {
    // Vind verzoeken die nog niet afgerond zijn
    const updatableRequests = requests.filter((r) => r.status !== "Afgerond");

    // Als er geen updatable requests zijn, voeg een nieuw document toe
    if (updatableRequests.length === 0) {
      const newDocument = generateNewDocument();

      // Emit event voor nieuw document
      const event = eventSystem.createEvent(
        newDocument.id,
        newDocument.title,
        null,
        "Ontvangen",
        newDocument.organization,
      );
      eventSystem.emit(event);

      // Voeg nieuw document toe, maar houd het totaal onder MAX_DOCUMENTS
      const updatedList = [...requests, newDocument];
      return updatedList.slice(-MAX_DOCUMENTS);
    }

    // Kies een willekeurig verzoek om te updaten
    const randomIndex = Math.floor(Math.random() * updatableRequests.length);
    const requestToUpdate = updatableRequests[randomIndex];

    // Bepaal de volgende status
    const currentStatusIndex = statusProgression.indexOf(
      requestToUpdate.status,
    );
    const nextStatus = statusProgression[currentStatusIndex + 1];

    if (!nextStatus) return null;

    // Maak een event aan
    const event = eventSystem.createEvent(
      requestToUpdate.id,
      requestToUpdate.title,
      requestToUpdate.status,
      nextStatus,
      requestToUpdate.organization,
    );

    // Emit het event
    eventSystem.emit(event);

    let updatedRequests: WOORequest[];

    // Als status 'Afgerond' wordt, verwijder het document en voeg een nieuw toe
    if (nextStatus === "Afgerond") {
      // Verwijder het afgeronde document
      updatedRequests = requests.filter((req) => req.id !== requestToUpdate.id);

      // Voeg een nieuw document toe
      const newDocument = generateNewDocument();

      // Emit event voor nieuw document
      const newDocEvent = eventSystem.createEvent(
        newDocument.id,
        newDocument.title,
        null,
        "Ontvangen",
        newDocument.organization,
      );
      eventSystem.emit(newDocEvent);

      updatedRequests = [...updatedRequests, newDocument];
    } else {
      // Update de status van het document
      updatedRequests = requests.map((req) =>
        req.id === requestToUpdate.id
          ? {
              ...req,
              status: nextStatus,
            }
          : req,
      );
    }

    return updatedRequests;
  }

  getIsRunning(): boolean {
    return this.isRunning;
  }
}

export const statusSimulator = new StatusSimulator();
