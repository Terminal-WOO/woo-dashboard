import { useState, useEffect } from 'react';
import { Activity, Database, FileText, Clock, TrendingUp, Filter } from 'lucide-react';

interface DocumentEvent {
  eventId: string;
  eventType: 'document.uploaded' | 'document.updated' | 'document.deleted';
  timestamp: string;
  system: 'paperless' | 'alfresco';
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
  const [filter, setFilter] = useState<'all' | 'paperless' | 'alfresco'>('all');
  const [isConnected, setIsConnected] = useState(false);
  const [error, setError] = useState<string | null>(null);

  const CONSUMER_URL = 'http://localhost:3002';

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

  const fetchEvents = async () => {
    try {
      const url = filter === 'all'
        ? `${CONSUMER_URL}/events?limit=50`
        : `${CONSUMER_URL}/events/system/${filter}?limit=50`;

      const response = await fetch(url);
      if (!response.ok) throw new Error('Failed to fetch events');

      const data = await response.json();
      setEvents(data.events || []);
      setIsConnected(true);
      setError(null);
    } catch (err) {
      console.error('Failed to fetch events:', err);
      setIsConnected(false);
      setError('Event consumer niet bereikbaar. Start de NATS event consumer service.');
    }
  };

  const fetchStats = async () => {
    try {
      const response = await fetch(`${CONSUMER_URL}/stats`);
      if (!response.ok) throw new Error('Failed to fetch stats');

      const data = await response.json();
      setStats(data);
    } catch (err) {
      console.error('Failed to fetch stats:', err);
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
      case 'document.uploaded':
        return '📤';
      case 'document.updated':
        return '📝';
      case 'document.deleted':
        return '🗑️';
      default:
        return '📄';
    }
  };

  const getSystemColor = (system: string) => {
    return system === 'paperless' ? 'text-blue-600' : 'text-purple-600';
  };

  const getSystemBg = (system: string) => {
    return system === 'paperless'
      ? 'bg-blue-50 dark:bg-blue-900/20 border-blue-200 dark:border-blue-800'
      : 'bg-purple-50 dark:bg-purple-900/20 border-purple-200 dark:border-purple-800';
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
            Run: <code className="bg-yellow-100 dark:bg-yellow-900 px-1 rounded">cd nats-events && docker-compose up -d</code>
          </p>
        </div>
      )}

      {/* Statistics */}
      {stats && (
        <div className="grid grid-cols-2 md:grid-cols-4 gap-4 mb-6">
          <div className="p-4 bg-gray-50 dark:bg-gray-700/50 rounded-lg">
            <div className="flex items-center gap-2 mb-1">
              <Database className="w-4 h-4 text-gray-500" />
              <span className="text-sm text-gray-600 dark:text-gray-400">Totaal Events</span>
            </div>
            <p className="text-2xl font-bold text-gray-900 dark:text-white">{stats.total}</p>
          </div>

          <div className="p-4 bg-gray-50 dark:bg-gray-700/50 rounded-lg">
            <div className="flex items-center gap-2 mb-1">
              <Clock className="w-4 h-4 text-gray-500" />
              <span className="text-sm text-gray-600 dark:text-gray-400">Laatste 24u</span>
            </div>
            <p className="text-2xl font-bold text-gray-900 dark:text-white">{stats.last24h}</p>
          </div>

          <div className="p-4 bg-blue-50 dark:bg-blue-900/20 rounded-lg">
            <div className="flex items-center gap-2 mb-1">
              <FileText className="w-4 h-4 text-blue-600" />
              <span className="text-sm text-blue-600">Paperless</span>
            </div>
            <p className="text-2xl font-bold text-blue-700 dark:text-blue-300">{stats.bySystem.paperless}</p>
          </div>

          <div className="p-4 bg-purple-50 dark:bg-purple-900/20 rounded-lg">
            <div className="flex items-center gap-2 mb-1">
              <FileText className="w-4 h-4 text-purple-600" />
              <span className="text-sm text-purple-600">Alfresco</span>
            </div>
            <p className="text-2xl font-bold text-purple-700 dark:text-purple-300">{stats.bySystem.alfresco}</p>
          </div>
        </div>
      )}

      {/* Event List */}
      <div className="space-y-2 max-h-96 overflow-y-auto">
        <h3 className="font-medium text-sm text-gray-700 dark:text-gray-300 sticky top-0 bg-white dark:bg-gray-800 py-2 flex items-center gap-2">
          <TrendingUp className="w-4 h-4" />
          Recente Events ({events.length})
        </h3>

        {events.length === 0 ? (
          <div className="text-center py-8 text-gray-500">
            <Activity className="w-12 h-12 mx-auto mb-2 opacity-50" />
            <p>Geen events gevonden</p>
            <p className="text-sm mt-1">Upload documenten via de DMS Simulator</p>
          </div>
        ) : (
          events.map((event) => (
            <div
              key={event.eventId}
              className={`p-3 rounded-lg border ${getSystemBg(event.system)} transition-all hover:shadow-md`}
            >
              <div className="flex items-start gap-3">
                <span className="text-2xl">{getEventIcon(event.eventType)}</span>

                <div className="flex-1 min-w-0">
                  {/* Title and System */}
                  <div className="flex items-center gap-2 mb-1">
                    <p className="font-medium text-gray-900 dark:text-white truncate">
                      {event.document.title}
                    </p>
                    <span className={`text-xs px-2 py-0.5 rounded ${getSystemColor(event.system)} bg-white dark:bg-gray-800`}>
                      {event.system}
                    </span>
                  </div>

                  {/* Metadata */}
                  <div className="flex flex-wrap gap-2 text-xs text-gray-600 dark:text-gray-400 mb-2">
                    <span className="flex items-center gap-1">
                      📁 {event.document.category}
                    </span>
                    <span>•</span>
                    <span className="flex items-center gap-1">
                      👤 {event.document.author}
                    </span>
                    <span>•</span>
                    <span className="flex items-center gap-1">
                      📏 {formatSize(event.document.size)}
                    </span>
                    {event.metadata.uploadDuration && (
                      <>
                        <span>•</span>
                        <span className="flex items-center gap-1">
                          ⏱️ {formatDuration(event.metadata.uploadDuration)}
                        </span>
                      </>
                    )}
                  </div>

                  {/* Tags */}
                  {event.document.tags.length > 0 && (
                    <div className="flex flex-wrap gap-1 mb-2">
                      {event.document.tags.slice(0, 5).map((tag, i) => (
                        <span
                          key={i}
                          className="text-xs px-2 py-0.5 bg-gray-200 dark:bg-gray-700 text-gray-700 dark:text-gray-300 rounded"
                        >
                          {tag}
                        </span>
                      ))}
                      {event.document.tags.length > 5 && (
                        <span className="text-xs text-gray-500">
                          +{event.document.tags.length - 5}
                        </span>
                      )}
                    </div>
                  )}

                  {/* Timestamp */}
                  <p className="text-xs text-gray-500">
                    {new Date(event.timestamp).toLocaleString('nl-NL')}
                  </p>
                </div>

                {/* Document ID */}
                <div className="text-xs text-gray-400 font-mono">
                  #{event.document.id}
                </div>
              </div>
            </div>
          ))
        )}
      </div>

      {/* Info */}
      <div className="mt-6 p-4 bg-gray-50 dark:bg-gray-700/30 rounded-lg">
        <h4 className="font-medium text-sm text-gray-900 dark:text-white mb-2 flex items-center gap-2">
          <Database className="w-4 h-4" />
          NATS JetStream Event System
        </h4>
        <p className="text-sm text-gray-600 dark:text-gray-400">
          Alle document uploads worden real-time geregistreerd als events in NATS JetStream.
          Events bevatten volledige metadata en zijn persistent opgeslagen voor 7 dagen.
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
