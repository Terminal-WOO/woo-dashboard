import { useState, useEffect } from "react";
import {
  BarChart,
  Bar,
  PieChart,
  Pie,
  Cell,
  XAxis,
  YAxis,
  CartesianGrid,
  Tooltip,
  Legend,
  ResponsiveContainer,
} from "recharts";
import { StatsCard } from "./components/StatsCard";
import { RequestsTable } from "./components/RequestsTable";
import { ActivityFeed } from "./components/ActivityFeed";
import { BackendSwitcher } from "./components/BackendSwitcher";
import { DMSSimulator } from "./components/DMSSimulator";
import { EventStreamViewer } from "./components/EventStreamViewer";
import {
  calculateStats,
  getMonthlyData,
  getDetailedStatusDistribution,
} from "./data";
import { WOORequest } from "./types";
import { backendService } from "./backendService";
import "./App.css";

function App() {
  const [requests, setRequests] = useState<WOORequest[]>([]);
  const [isSimulating, setIsSimulating] = useState(false);
  const [isInitializing, setIsInitializing] = useState(true);

  const startSimulationPolling = () => {
    const interval = setInterval(async () => {
      const updatedRequests = await backendService.getAll();
      setRequests(updatedRequests);
    }, 2000);
    (window as any).__simulationInterval = interval;
  };

  const stopSimulationPolling = () => {
    if ((window as any).__simulationInterval) {
      clearInterval((window as any).__simulationInterval);
      (window as any).__simulationInterval = null;
    }
  };

  const stats = calculateStats(requests);
  const monthlyData = getMonthlyData();
  const statusDistribution = getDetailedStatusDistribution(requests);

  // Initialize database on mount
  useEffect(() => {
    const initDatabase = async () => {
      try {
        // Load initial documents from backend
        const initialRequests = await backendService.getAll();
        setRequests(initialRequests);
        setIsInitializing(false);
      } catch (error) {
        console.error("[App] Failed to initialize database:", error);
        setIsInitializing(false);
      }
    };

    console.log(
      "%c🎉 WOO Dashboard v2.2 - Dual Backend",
      "background: #16a34a; color: white; padding: 8px; font-size: 16px; font-weight: bold;",
    );
    console.log(
      "%cJenV, Financiën, Zuid-Holland & Flevoland",
      "color: #107abe; font-size: 14px;",
    );
    console.log(
      "%cMock Database + PostgreSQL Backend",
      "color: #9333ea; font-size: 12px;",
    );

    const runInit = async () => {
      await initDatabase();
      if (backendService.getBackendType() === "mock") {
        await backendService.startSimulation();
        setIsSimulating(true);
        startSimulationPolling();
      }
    };

    runInit();
  }, []);

  const handleToggleSimulation = async () => {
    if (isSimulating) {
      await backendService.stopSimulation();
      stopSimulationPolling();
      setIsSimulating(false);
    } else {
      await backendService.startSimulation();
      setIsSimulating(true);
      startSimulationPolling();
    }
  };

  // Cleanup on unmount
  useEffect(() => {
    return () => {
      stopSimulationPolling();
      backendService.stopSimulation();
    };
  }, []);

  if (isInitializing) {
    return (
      <div className="app">
        <div className="loading-container">
          <div className="loading-spinner"></div>
          <p>Database wordt geïnitialiseerd...</p>
          <p style={{ fontSize: "0.85rem", color: "#6b7280" }}>
            Woo-documenten laden (open.overheid.nl)
          </p>
        </div>
      </div>
    );
  }

  return (
    <div className="app">
      <header className="header">
        <div className="container">
          <div className="header-content">
            <div>
              <h1>Woo Dashboard</h1>
              <p className="subtitle">
                Wet open overheid — documenten via open.overheid.nl
              </p>
            </div>
            <div className="header-actions">
              <BackendSwitcher />
              <button
                className={`simulate-button ${isSimulating ? "active" : ""}`}
                onClick={handleToggleSimulation}
                disabled={isInitializing}
              >
                {isSimulating ? "⏸ Stop Simulatie" : "▶ Start Simulatie"}
              </button>
            </div>
          </div>
        </div>
      </header>

      <main className="container">
        {/* Documenten & activiteit */}
        <section className="main-grid">
          <div className="activity-section">
            <ActivityFeed requests={requests} />
          </div>
          <div className="table-section">
            <h2>Recente WOO Verzoeken</h2>
            <RequestsTable requests={requests} />
          </div>
        </section>

        {/* Statistieken Cards */}
        <section className="stats-grid">
          <StatsCard
            title="Totaal Verzoeken"
            value={stats.totalRequests}
            icon="📊"
            color="#107abe"
          />
          <StatsCard
            title="Ontvangen"
            value={stats.received}
            icon="📥"
            color="#d97706"
          />
          <StatsCard
            title="In Behandeling"
            value={stats.inProgress}
            icon="⏳"
            color="#107abe"
          />
          <StatsCard
            title="Afgerond"
            value={stats.completed}
            icon="✅"
            color="#2e7d32"
          />
        </section>

        {/* Charts */}
        <section className="charts-grid">
          <div className="chart-card">
            <h2>Verzoeken per Maand</h2>
            <ResponsiveContainer width="100%" height={300}>
              <BarChart data={monthlyData}>
                <CartesianGrid strokeDasharray="3 3" />
                <XAxis dataKey="month" />
                <YAxis />
                <Tooltip />
                <Legend />
                <Bar dataKey="requests" fill="#107abe" name="Verzoeken" />
              </BarChart>
            </ResponsiveContainer>
          </div>

          <div className="chart-card">
            <h2>Status Verdeling</h2>
            <ResponsiveContainer width="100%" height={300}>
              <PieChart>
                <Pie
                  data={statusDistribution}
                  cx="50%"
                  cy="50%"
                  labelLine={false}
                  label={({ name, percent }) =>
                    `${name}: ${(percent * 100).toFixed(0)}%`
                  }
                  outerRadius={80}
                  fill="#8884d8"
                  dataKey="value"
                >
                  {statusDistribution.map((entry, index) => (
                    <Cell key={`cell-${index}`} fill={entry.color} />
                  ))}
                </Pie>
                <Tooltip />
              </PieChart>
            </ResponsiveContainer>
          </div>
        </section>

        {/* DMS Simulator Section - Only available in development mode */}
        {import.meta.env.DEV && (
          <section
            className="dms-simulator-section"
            style={{ marginBottom: "2rem" }}
          >
            <DMSSimulator />
          </section>
        )}

        {/* Event Stream Section - Only available in development mode */}
        {import.meta.env.DEV && (
          <section
            className="event-stream-section"
            style={{ marginBottom: "2rem" }}
          >
            <EventStreamViewer />
          </section>
        )}
      </main>

      <footer className="footer">
        <div className="container">
          <p>Woo Dashboard — Wet open overheid © 2025</p>
        </div>
      </footer>
    </div>
  );
}

export default App;
