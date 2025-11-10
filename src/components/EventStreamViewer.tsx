import { useState, useEffect } from "react";
import {
  Activity,
  Database,
  Clock,
  FileText,
  Filter,
  TrendingUp,
  ChevronLeft,
  ChevronRight,
} from "lucide-react";

interface DocumentEvent {
  eventId: string;
  eventType: "document.uploaded" | "document.updated" | "document.deleted";
  timestamp: string;
  system: "paperless" | "alfresco";
  document: {
    id: string;
    title: string;
    type: string;
    category: string;
    author: string;
    date: string;
    tags: string[];
    size: number;
    contentType: string;
  };
  metadata: {
    simulationId?: string;
    uploadDuration?: number;
    source: string;
  };
}

interface EventStats {
  total: number;
  last24h: number;
  bySystem: {
    paperless: number;
    alfresco: number;
  };
  byType: {
    uploaded: number;
    updated: number;
    deleted: number;
  };
}

export function EventStreamViewer() {
  const [events, setEvents] = useState<DocumentEvent[]>([]);
  const [stats, setStats] = useState<EventStats | null>(null);
  const [filter, setFilter] = useState<"all" | "paperless" | "alfresco">("all");
  const [isConnected, setIsConnected] = useState(false);
  const [error, setError] = useState<string | null>(null);
  const [currentPage, setCurrentPage] = useState(1);
  const EVENTS_PER_PAGE = 10;

  const CONSUMER_URL = "http://localhost:3002";

  // Fetch initial data
  useEffect(() => {
    fetchEvents();
    fetchStats();
    const interval = setInterval(() => {
      fetchEvents();
      fetchStats();
    }, 3000);
    return () => clearInterval(interval);
  }, [filter]);

  // Reset to page 1 when filter changes
  useEffect(() => {
    setCurrentPage(1);
  }, [filter]);

  const fetchEvents = async () => {
    try {
      const url =
        filter === "all"
          ? `${CONSUMER_URL}/events?limit=100`
          : `${CONSUMER_URL}/events/system/${filter}?limit=100`;

      const response = await fetch(url);
      if (!response.ok) throw new Error("Failed to fetch events");

      const data = await response.json();
      setEvents(data.events || []);
      setIsConnected(true);
      setError(null);
    } catch (err) {
      console.error("Failed to fetch events:", err);
      setIsConnected(false);
      setError(
        "Event consumer niet bereikbaar. Start de NATS event consumer service.",
      );
    }
  };

  const fetchStats = async () => {
    try {
      const response = await fetch(`${CONSUMER_URL}/stats`);
      if (!response.ok) throw new Error("Failed to fetch stats");
      const data = await response.json();
      setStats(data);
    } catch (err) {
      console.error("Failed to fetch stats:", err);
    }
  };

  const formatSize = (bytes: number) => {
    if (bytes < 1024) return `${bytes} B`;
    if (bytes < 1048576) return `${(bytes / 1024).toFixed(1)} KB`;
    return `${(bytes / 1048576).toFixed(1)} MB`;
  };

  const formatDuration = (ms: number) => {
    if (ms < 1000) return `${ms}ms`;
    return `${(ms / 1000).toFixed(1)}s`;
  };

  const getEventIcon = (type: string) => {
    switch (type) {
      case "document.uploaded":
        return "📤";
      case "document.updated":
        return "📝";
      case "document.deleted":
        return "🗑️";
      default:
        return "📄";
    }
  };

  const getSystemBadge = (system: string) => {
    return system === "paperless"
      ? "bg-blue-100 text-blue-800 dark:bg-blue-900 dark:text-blue-200"
      : "bg-purple-100 text-purple-800 dark:bg-purple-900 dark:text-purple-200";
  };

  // Pagination logic
  const totalPages = Math.ceil(events.length / EVENTS_PER_PAGE);
  const startIndex = (currentPage - 1) * EVENTS_PER_PAGE;
  const endIndex = startIndex + EVENTS_PER_PAGE;
  const currentEvents = events.slice(startIndex, endIndex);

  const goToNextPage = () => {
    if (currentPage < totalPages) {
      setCurrentPage(currentPage + 1);
    }
  };

  const goToPreviousPage = () => {
    if (currentPage > 1) {
      setCurrentPage(currentPage - 1);
    }
  };

  const goToPage = (page: number) => {
    setCurrentPage(page);
  };

  // Generate page numbers for pagination
  const getPageNumbers = () => {
    const pages = [];
    const maxVisible = 5;

    if (totalPages <= maxVisible) {
      for (let i = 1; i <= totalPages; i++) {
        pages.push(i);
      }
    } else {
      if (currentPage <= 3) {
        for (let i = 1; i <= 4; i++) pages.push(i);
        pages.push("...");
        pages.push(totalPages);
      } else if (currentPage >= totalPages - 2) {
        pages.push(1);
        pages.push("...");
        for (let i = totalPages - 3; i <= totalPages; i++) pages.push(i);
      } else {
        pages.push(1);
        pages.push("...");
        pages.push(currentPage - 1);
        pages.push(currentPage);
        pages.push(currentPage + 1);
        pages.push("...");
        pages.push(totalPages);
      }
    }

    return pages;
  };

  return (
    <div className="bg-white dark:bg-gray-800 p-6 rounded-lg shadow-lg">
      {/* Header */}
      <div className="flex items-center justify-between mb-6">
        <div className="flex items-center gap-3">
          <Activity className="w-6 h-6 text-green-600" />
          <h2 className="text-xl font-bold text-gray-900 dark:text-white">
            Document Event Stream
          </h2>
          <div className="flex items-center gap-2">
            {isConnected ? (
              <span className="flex items-center gap-1 text-sm text-green-600">
                <span className="w-2 h-2 bg-green-600 rounded-full animate-pulse"></span>
                Live
              </span>
            ) : (
              <span className="flex items-center gap-1 text-sm text-red-600">
                <span className="w-2 h-2 bg-red-600 rounded-full"></span>
                Offline
              </span>
            )}
          </div>
        </div>
        {/* Filter */}
        <div className="flex items-center gap-2">
          <Filter className="w-4 h-4 text-gray-500" />
          <select
            value={filter}
            onChange={(e) => setFilter(e.target.value as any)}
            className="px-3 py-1 bg-gray-100 dark:bg-gray-700 rounded border border-gray-300 dark:border-gray-600 text-sm"
          >
            <option value="all">Alle systemen</option>
            <option value="paperless">Paperless-ngx</option>
            <option value="alfresco">Alfresco</option>
          </select>
        </div>
      </div>

      {/* Error Message */}
      {error && (
        <div className="mb-4 p-4 bg-yellow-50 dark:bg-yellow-900/20 border border-yellow-200 dark:border-yellow-800 rounded-lg">
          <p className="text-yellow-800 dark:text-yellow-200 text-sm">
            ⚠️ {error}
          </p>
          <p className="text-xs text-yellow-700 dark:text-yellow-300 mt-1">
            Run:{" "}
            <code className="bg-yellow-100 dark:bg-yellow-900 px-1 rounded">
              cd nats-events && docker-compose up -d
            </code>
          </p>
        </div>
      )}

      {/* Statistics */}
      {stats && (
        <div className="grid grid-cols-2 md:grid-cols-4 gap-4 mb-6">
          <div className="p-4 bg-gray-50 dark:bg-gray-700/50 rounded-lg">
            <div className="flex items-center gap-2 mb-1">
              <Database className="w-4 h-4 text-gray-500" />
              <span className="text-sm text-gray-600 dark:text-gray-400">
                Totaal Events
              </span>
            </div>
            <p className="text-2xl font-bold text-gray-900 dark:text-white">
              {stats.total}
            </p>
          </div>
          <div className="p-4 bg-gray-50 dark:bg-gray-700/50 rounded-lg">
            <div className="flex items-center gap-2 mb-1">
              <Clock className="w-4 h-4 text-gray-500" />
              <span className="text-sm text-gray-600 dark:text-gray-400">
                Laatste 24u
              </span>
            </div>
            <p className="text-2xl font-bold text-gray-900 dark:text-white">
              {stats.last24h}
            </p>
          </div>
          <div className="p-4 bg-blue-50 dark:bg-blue-900/20 rounded-lg">
            <div className="flex items-center gap-2 mb-1">
              <FileText className="w-4 h-4 text-blue-600" />
              <span className="text-sm text-blue-600">Paperless</span>
            </div>
            <p className="text-2xl font-bold text-blue-700 dark:text-blue-300">
              {stats.bySystem.paperless}
            </p>
          </div>
          <div className="p-4 bg-purple-50 dark:bg-purple-900/20 rounded-lg">
            <div className="flex items-center gap-2 mb-1">
              <FileText className="w-4 h-4 text-purple-600" />
              <span className="text-sm text-purple-600">Alfresco</span>
            </div>
            <p className="text-2xl font-bold text-purple-700 dark:text-purple-300">
              {stats.bySystem.alfresco}
            </p>
          </div>
        </div>
      )}

      {/* Event Table */}
      <div className="mb-4">
        <div className="flex items-center gap-2 mb-3">
          <TrendingUp className="w-4 h-4 text-gray-600 dark:text-gray-400" />
          <h3 className="font-medium text-sm text-gray-700 dark:text-gray-300">
            Recente Events ({events.length})
          </h3>
        </div>

        {events.length === 0 ? (
          <div className="text-center py-12 text-gray-500">
            <Activity className="w-12 h-12 mx-auto mb-2 opacity-50" />
            <p>Geen events gevonden</p>
            <p className="text-sm mt-1">
              Upload documenten via de DMS Simulator
            </p>
          </div>
        ) : (
          <>
            <div className="overflow-x-auto">
              <table className="w-full border-collapse">
                <thead>
                  <tr className="bg-gray-50 dark:bg-gray-700/50 border-b border-gray-200 dark:border-gray-600">
                    <th className="px-4 py-3 text-left text-xs font-semibold text-gray-600 dark:text-gray-300 uppercase tracking-wider">
                      Type
                    </th>
                    <th className="px-4 py-3 text-left text-xs font-semibold text-gray-600 dark:text-gray-300 uppercase tracking-wider">
                      Document
                    </th>
                    <th className="px-4 py-3 text-left text-xs font-semibold text-gray-600 dark:text-gray-300 uppercase tracking-wider">
                      Systeem
                    </th>
                    <th className="px-4 py-3 text-left text-xs font-semibold text-gray-600 dark:text-gray-300 uppercase tracking-wider">
                      Categorie
                    </th>
                    <th className="px-4 py-3 text-left text-xs font-semibold text-gray-600 dark:text-gray-300 uppercase tracking-wider">
                      Grootte
                    </th>
                    <th className="px-4 py-3 text-left text-xs font-semibold text-gray-600 dark:text-gray-300 uppercase tracking-wider">
                      Upload Tijd
                    </th>
                    <th className="px-4 py-3 text-left text-xs font-semibold text-gray-600 dark:text-gray-300 uppercase tracking-wider">
                      Timestamp
                    </th>
                  </tr>
                </thead>
                <tbody className="divide-y divide-gray-200 dark:divide-gray-700">
                  {currentEvents.map((event) => (
                    <tr
                      key={event.eventId}
                      className="hover:bg-gray-50 dark:hover:bg-gray-700/30 transition-colors"
                    >
                      <td className="px-4 py-3">
                        <span className="text-2xl" title={event.eventType}>
                          {getEventIcon(event.eventType)}
                        </span>
                      </td>
                      <td className="px-4 py-3">
                        <div className="max-w-xs">
                          <p
                            className="font-medium text-gray-900 dark:text-white truncate"
                            title={event.document.title}
                          >
                            {event.document.title}
                          </p>
                          <p className="text-xs text-gray-500 dark:text-gray-400">
                            {event.document.author}
                          </p>
                        </div>
                      </td>
                      <td className="px-4 py-3">
                        <span
                          className={`inline-flex px-2 py-1 text-xs font-medium rounded ${getSystemBadge(event.system)}`}
                        >
                          {event.system}
                        </span>
                      </td>
                      <td className="px-4 py-3 text-sm text-gray-700 dark:text-gray-300">
                        {event.document.category}
                      </td>
                      <td className="px-4 py-3 text-sm text-gray-700 dark:text-gray-300">
                        {formatSize(event.document.size)}
                      </td>
                      <td className="px-4 py-3 text-sm text-gray-700 dark:text-gray-300">
                        {event.metadata.uploadDuration
                          ? formatDuration(event.metadata.uploadDuration)
                          : "-"}
                      </td>
                      <td className="px-4 py-3 text-xs text-gray-500 dark:text-gray-400">
                        {new Date(event.timestamp).toLocaleString("nl-NL", {
                          day: "2-digit",
                          month: "2-digit",
                          year: "2-digit",
                          hour: "2-digit",
                          minute: "2-digit",
                        })}
                      </td>
                    </tr>
                  ))}
                </tbody>
              </table>
            </div>

            {/* Pagination */}
            {totalPages > 1 && (
              <div className="mt-4 flex items-center justify-between">
                <div className="text-sm text-gray-600 dark:text-gray-400">
                  Pagina {currentPage} van {totalPages} ({events.length} events)
                </div>
                <div className="flex items-center gap-2">
                  <button
                    onClick={goToPreviousPage}
                    disabled={currentPage === 1}
                    className="px-3 py-1 rounded border border-gray-300 dark:border-gray-600 bg-white dark:bg-gray-800 hover:bg-gray-50 dark:hover:bg-gray-700 disabled:opacity-50 disabled:cursor-not-allowed transition-colors"
                  >
                    <ChevronLeft className="w-4 h-4" />
                  </button>

                  <div className="flex gap-1">
                    {getPageNumbers().map((pageNum, idx) =>
                      pageNum === "..." ? (
                        <span
                          key={`ellipsis-${idx}`}
                          className="px-3 py-1 text-gray-500"
                        >
                          ...
                        </span>
                      ) : (
                        <button
                          key={pageNum}
                          onClick={() => goToPage(pageNum as number)}
                          className={`px-3 py-1 rounded border transition-colors ${
                            currentPage === pageNum
                              ? "bg-blue-600 text-white border-blue-600"
                              : "border-gray-300 dark:border-gray-600 bg-white dark:bg-gray-800 hover:bg-gray-50 dark:hover:bg-gray-700"
                          }`}
                        >
                          {pageNum}
                        </button>
                      ),
                    )}
                  </div>

                  <button
                    onClick={goToNextPage}
                    disabled={currentPage === totalPages}
                    className="px-3 py-1 rounded border border-gray-300 dark:border-gray-600 bg-white dark:bg-gray-800 hover:bg-gray-50 dark:hover:bg-gray-700 disabled:opacity-50 disabled:cursor-not-allowed transition-colors"
                  >
                    <ChevronRight className="w-4 h-4" />
                  </button>
                </div>
              </div>
            )}
          </>
        )}
      </div>

      {/* Info */}
      <div className="mt-6 p-4 bg-gray-50 dark:bg-gray-700/30 rounded-lg">
        <h4 className="font-medium text-sm text-gray-900 dark:text-white mb-2 flex items-center gap-2">
          <Database className="w-4 h-4" />
          NATS JetStream Event System
        </h4>
        <p className="text-sm text-gray-600 dark:text-gray-400">
          Alle document uploads worden real-time geregistreerd als events in
          NATS JetStream. Events bevatten volledige metadata en zijn persistent
          opgeslagen voor 7 dagen.
        </p>
        <div className="mt-2 flex gap-4 text-xs text-gray-500">
          <span>📊 Stream: DOCUMENT_EVENTS</span>
          <span>🔄 Subjects: document.*</span>
          <span>💾 Retention: 7 dagen</span>
        </div>
      </div>
    </div>
  );
}
