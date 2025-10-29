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
import { calculateStats, getMonthlyData, getStatusDistribution } from "./data";
import { WOORequest } from "./types";
import { erlangSimulatorV2 } from "./erlangSimulatorV2";
import "./App.css";

function App() {
  const [requests, setRequests] = useState<WOORequest[]>([]);
  const [isSimulating, setIsSimulating] = useState(false);
  const [isInitializing, setIsInitializing] = useState(true);
  const stats = calculateStats(requests);
  const monthlyData = getMonthlyData();
  const statusDistribution = getStatusDistribution(stats);

  // Initialize database on mount
  useEffect(() => {
    const initDatabase = async () => {
      try {
        await erlangSimulatorV2.initialize();
        const initialData = await erlangSimulatorV2.getStatistics();
        console.log("[App] Database initialized:", initialData);
        setIsInitializing(false);
      } catch (error) {
        console.error("[App] Failed to initialize database:", error);
        setIsInitializing(false);
      }
    };

    initDatabase();

    return () => {
      erlangSimulatorV2.stop();
    };
  }, []);

  const handleToggleSimulation = async () => {
    if (isSimulating) {
      erlangSimulatorV2.stop();
      setIsSimulating(false);
    } else {
      // Start simulation with 3 second intervals
      await erlangSimulatorV2.start((updatedRequests) => {
        setRequests([...updatedRequests]);
      }, 3000);
      setIsSimulating(true);
    }
  };

  if (isInitializing) {
    return (
      <div className="app">
        <div className="loading-container">
          <div className="loading-spinner"></div>
          <p>Database wordt geïnitialiseerd...</p>
          <p style={{ fontSize: "0.85rem", color: "#6b7280" }}>
            Gemeente Utrecht & Provincie Flevoland data laden
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
              <h1>WOO Dashboard - Utrecht & Flevoland</h1>
              <p className="subtitle">
                Wet Open Overheid - Erlang Actor System met SQLite Database
              </p>
            </div>
            <button
              className={`simulate-button ${isSimulating ? "active" : ""}`}
              onClick={handleToggleSimulation}
              disabled={isInitializing}
            >
              {isSimulating ? "⏸ Stop Simulatie" : "▶ Start Simulatie"}
            </button>
          </div>
        </div>
      </header>

      <main className="container">
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

        {/* Activity Feed & Tabel Grid */}
        <section className="main-grid">
          <div className="activity-section">
            <ActivityFeed />
          </div>

          <div className="table-section">
            <h2>Recente WOO Verzoeken</h2>
            <RequestsTable requests={requests} />
          </div>
        </section>
      </main>

      <footer className="footer">
        <div className="container">
          <p>WOO Dashboard - Wet Open Overheid © 2024</p>
        </div>
      </footer>
    </div>
  );
}

export default App;
