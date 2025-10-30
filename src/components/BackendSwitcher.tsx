/**
 * Backend Switcher Component
 *
 * Allows users to toggle between Mock and Erlang backend implementations
 */

import { useState, useEffect } from "react";
import { backendService, BackendType } from "../backendService";

interface BackendSwitcherProps {
  onBackendChange?: (backend: BackendType) => void;
}

export const BackendSwitcher = ({ onBackendChange }: BackendSwitcherProps) => {
  const [currentBackend, setCurrentBackend] = useState<BackendType>(
    backendService.getBackendType()
  );
  const [erlangAvailable, setErlangAvailable] = useState<boolean | null>(null);
  const [isChecking, setIsChecking] = useState(false);

  useEffect(() => {
    checkErlangBackend();
  }, []);

  const checkErlangBackend = async () => {
    setIsChecking(true);
    const available = await backendService.checkErlangBackendAvailable();
    setErlangAvailable(available);
    setIsChecking(false);
  };

  const handleSwitch = (type: BackendType) => {
    if (type === "erlang" && !erlangAvailable) {
      alert(
        "Erlang backend is niet beschikbaar!\n\n" +
        "Start de backend met:\n" +
        "cd erlang-backend && rebar3 shell"
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

  const getStatusColor = () => {
    if (isChecking) return "#9ca3af";
    if (erlangAvailable === null) return "#9ca3af";
    return erlangAvailable ? "#16a34a" : "#dc2626";
  };

  const getStatusText = () => {
    if (isChecking) return "Checking...";
    if (erlangAvailable === null) return "Unknown";
    return erlangAvailable ? "Available" : "Offline";
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
          className={`backend-button ${currentBackend === "erlang" ? "active" : ""}`}
          onClick={() => handleSwitch("erlang")}
          disabled={!erlangAvailable && currentBackend !== "erlang"}
        >
          <span className="backend-icon">⚡</span>
          Erlang
          <span
            className="backend-status-indicator"
            style={{ backgroundColor: getStatusColor() }}
            title={`Erlang Backend: ${getStatusText()}`}
          />
        </button>
      </div>
      <button
        className="backend-check-button"
        onClick={checkErlangBackend}
        disabled={isChecking}
        title="Check Erlang backend availability"
      >
        🔄
      </button>
    </div>
  );
};
