/**
 * Backend Switcher Component
 *
 * Allows users to toggle between Mock and PostgreSQL backend implementations
 */

import { useState, useEffect } from "react";
import { backendService, BackendType } from "../backendService";

interface BackendSwitcherProps {
  onBackendChange?: (backend: BackendType) => void;
}

export const BackendSwitcher = ({ onBackendChange }: BackendSwitcherProps) => {
  const [currentBackend, setCurrentBackend] = useState<BackendType>(
    backendService.getBackendType(),
  );
  const [postgresAvailable, setPostgresAvailable] = useState<boolean | null>(
    null,
  );
  const [isChecking, setIsChecking] = useState(false);

  useEffect(() => {
    checkBackends();
  }, []);

  const checkBackends = async () => {
    setIsChecking(true);
    const postgres = await backendService.checkPostgresBackendAvailable();
    setPostgresAvailable(postgres);
    setIsChecking(false);
  };

  const handleSwitch = (type: BackendType) => {
    if (type === "postgres" && !postgresAvailable) {
      alert(
        "PostgreSQL backend is niet beschikbaar!\n\n" +
          "Start de backend met:\n" +
          "cd postgres-backend && npm run dev",
      );
      return;
    }

    backendService.switchBackend(type);
    setCurrentBackend(type);
    if (onBackendChange) {
      onBackendChange(type);
    }

    // Reload page to reinitialize with new backend
    window.location.reload();
  };

  const getStatusColor = (available: boolean | null) => {
    if (isChecking) return "#9ca3af";
    if (available === null) return "#9ca3af";
    return available ? "#16a34a" : "#dc2626";
  };

  const getStatusText = (available: boolean | null) => {
    if (isChecking) return "Checking...";
    if (available === null) return "Unknown";
    return available ? "Available" : "Offline";
  };

  return (
    <div className="backend-switcher">
      <div className="backend-switcher-label">Backend:</div>
      <div className="backend-switcher-buttons">
        <button
          className={`backend-button ${currentBackend === "mock" ? "active" : ""}`}
          onClick={() => handleSwitch("mock")}
        >
          <span className="backend-icon">💾</span>
          Mock
        </button>
        <button
          className={`backend-button ${currentBackend === "postgres" ? "active" : ""}`}
          onClick={() => handleSwitch("postgres")}
          disabled={!postgresAvailable && currentBackend !== "postgres"}
        >
          <span className="backend-icon">🐘</span>
          PostgreSQL
          <span
            className="backend-status-indicator"
            style={{ backgroundColor: getStatusColor(postgresAvailable) }}
            title={`PostgreSQL Backend: ${getStatusText(postgresAvailable)}`}
          />
        </button>
      </div>
      <button
        className="backend-check-button"
        onClick={checkBackends}
        disabled={isChecking}
        title="Check backend availability"
      >
        🔄
      </button>
    </div>
  );
};
