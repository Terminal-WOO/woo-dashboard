import { useState, useEffect } from 'react';
import { FileText, Upload, CheckCircle, XCircle, Loader2, Play, Database } from 'lucide-react';

interface SimulationDocument {
  id: string;
  title: string;
  system: string;
  status: string;
}

interface SimulationStatus {
  total: number;
  completed: number;
  failed: number;
  status: 'running' | 'completed' | 'failed';
  progress: number;
  startTime: string;
  documents: SimulationDocument[];
}

interface ConnectionStatus {
  paperless: boolean;
  alfresco: boolean;
}

export function DMSSimulator() {
  const [count, setCount] = useState(5);
  const [systems, setSystems] = useState<Array<'paperless' | 'alfresco'>>(['paperless']);
  const [isSimulating, setIsSimulating] = useState(false);
  const [simulationId, setSimulationId] = useState<string | null>(null);
  const [status, setStatus] = useState<SimulationStatus | null>(null);
  const [connections, setConnections] = useState<ConnectionStatus>({ paperless: false, alfresco: false });
  const [isCheckingConnections, setIsCheckingConnections] = useState(false);

  const SIMULATOR_URL = 'http://localhost:3001';

  // Check connections on mount
  useEffect(() => {
    checkConnections();
  }, []);

  // Poll simulation status
  useEffect(() => {
    if (!simulationId) return;

    const interval = setInterval(async () => {
      try {
        const response = await fetch(`${SIMULATOR_URL}/simulate/${simulationId}`);
        const data = await response.json();
        setStatus(data);

        if (data.status === 'completed' || data.status === 'failed') {
          setIsSimulating(false);
          clearInterval(interval);
        }
      } catch (error) {
        console.error('Failed to fetch simulation status:', error);
      }
    }, 1000);

    return () => clearInterval(interval);
  }, [simulationId]);

  const checkConnections = async () => {
    setIsCheckingConnections(true);
    try {
      const response = await fetch(`${SIMULATOR_URL}/test-connections`);
      const data = await response.json();
      setConnections(data);
    } catch (error) {
      console.error('Failed to check connections:', error);
    } finally {
      setIsCheckingConnections(false);
    }
  };

  const startSimulation = async () => {
    if (systems.length === 0) {
      alert('Selecteer minimaal één DMS systeem');
      return;
    }

    setIsSimulating(true);
    setStatus(null);
    setSimulationId(null);

    try {
      const response = await fetch(`${SIMULATOR_URL}/simulate`, {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({ count, systems }),
      });

      const data = await response.json();
      setSimulationId(data.simulationId);
    } catch (error) {
      console.error('Failed to start simulation:', error);
      setIsSimulating(false);
      alert('Fout bij starten simulatie. Zorg dat de DMS Simulator draait op port 3001');
    }
  };

  const toggleSystem = (system: 'paperless' | 'alfresco') => {
    if (systems.includes(system)) {
      setSystems(systems.filter(s => s !== system));
    } else {
      setSystems([...systems, system]);
    }
  };

  return (
    <div className="bg-white dark:bg-gray-800 p-6 rounded-lg shadow-lg">
      <div className="flex items-center justify-between mb-6">
        <div className="flex items-center gap-3">
          <Database className="w-6 h-6 text-blue-600" />
          <h2 className="text-xl font-bold text-gray-900 dark:text-white">
            DMS Simulator
          </h2>
        </div>
        <button
          onClick={checkConnections}
          disabled={isCheckingConnections}
          className="px-3 py-1 text-sm bg-gray-100 hover:bg-gray-200 dark:bg-gray-700 dark:hover:bg-gray-600 rounded flex items-center gap-2"
        >
          {isCheckingConnections ? <Loader2 className="w-4 h-4 animate-spin" /> : <Database className="w-4 h-4" />}
          Test Verbinding
        </button>
      </div>

      {/* Connection Status */}
      <div className="grid grid-cols-2 gap-4 mb-6">
        <div className={`p-4 rounded-lg border-2 ${connections.paperless ? 'border-green-500 bg-green-50 dark:bg-green-900/20' : 'border-red-500 bg-red-50 dark:bg-red-900/20'}`}>
          <div className="flex items-center justify-between">
            <span className="font-medium">Paperless-ngx</span>
            {connections.paperless ? (
              <CheckCircle className="w-5 h-5 text-green-600" />
            ) : (
              <XCircle className="w-5 h-5 text-red-600" />
            )}
          </div>
          <p className="text-sm text-gray-600 dark:text-gray-400 mt-1">
            {connections.paperless ? 'Verbonden' : 'Niet bereikbaar'}
          </p>
        </div>

        <div className={`p-4 rounded-lg border-2 ${connections.alfresco ? 'border-green-500 bg-green-50 dark:bg-green-900/20' : 'border-red-500 bg-red-50 dark:bg-red-900/20'}`}>
          <div className="flex items-center justify-between">
            <span className="font-medium">Alfresco</span>
            {connections.alfresco ? (
              <CheckCircle className="w-5 h-5 text-green-600" />
            ) : (
              <XCircle className="w-5 h-5 text-red-600" />
            )}
          </div>
          <p className="text-sm text-gray-600 dark:text-gray-400 mt-1">
            {connections.alfresco ? 'Verbonden' : 'Niet bereikbaar'}
          </p>
        </div>
      </div>

      {/* Configuration */}
      <div className="space-y-4 mb-6">
        <div>
          <label className="block text-sm font-medium text-gray-700 dark:text-gray-300 mb-2">
            Aantal documenten: {count}
          </label>
          <input
            type="range"
            min="1"
            max="50"
            value={count}
            onChange={(e) => setCount(parseInt(e.target.value))}
            disabled={isSimulating}
            className="w-full"
          />
          <div className="flex justify-between text-xs text-gray-500 mt-1">
            <span>1</span>
            <span>25</span>
            <span>50</span>
          </div>
        </div>

        <div>
          <label className="block text-sm font-medium text-gray-700 dark:text-gray-300 mb-2">
            Upload naar:
          </label>
          <div className="flex gap-4">
            <label className="flex items-center gap-2 cursor-pointer">
              <input
                type="checkbox"
                checked={systems.includes('paperless')}
                onChange={() => toggleSystem('paperless')}
                disabled={isSimulating || !connections.paperless}
                className="w-4 h-4"
              />
              <span className={!connections.paperless ? 'text-gray-400' : ''}>
                Paperless-ngx
              </span>
            </label>
            <label className="flex items-center gap-2 cursor-pointer">
              <input
                type="checkbox"
                checked={systems.includes('alfresco')}
                onChange={() => toggleSystem('alfresco')}
                disabled={isSimulating || !connections.alfresco}
                className="w-4 h-4"
              />
              <span className={!connections.alfresco ? 'text-gray-400' : ''}>
                Alfresco
              </span>
            </label>
          </div>
        </div>
      </div>

      {/* Start Button */}
      <button
        onClick={startSimulation}
        disabled={isSimulating || systems.length === 0}
        className="w-full bg-blue-600 hover:bg-blue-700 disabled:bg-gray-400 text-white font-medium py-3 px-4 rounded-lg flex items-center justify-center gap-2 transition-colors"
      >
        {isSimulating ? (
          <>
            <Loader2 className="w-5 h-5 animate-spin" />
            Bezig met uploaden...
          </>
        ) : (
          <>
            <Play className="w-5 h-5" />
            Start Simulatie
          </>
        )}
      </button>

      {/* Progress */}
      {status && (
        <div className="mt-6 space-y-4">
          <div>
            <div className="flex justify-between text-sm mb-2">
              <span className="font-medium">Voortgang</span>
              <span>{status.progress}%</span>
            </div>
            <div className="w-full bg-gray-200 dark:bg-gray-700 rounded-full h-3 overflow-hidden">
              <div
                className="bg-blue-600 h-full transition-all duration-300 ease-out"
                style={{ width: `${status.progress}%` }}
              />
            </div>
            <div className="flex justify-between text-xs text-gray-600 dark:text-gray-400 mt-2">
              <span>✓ {status.completed} geslaagd</span>
              <span>✗ {status.failed} mislukt</span>
              <span>Totaal: {status.total}</span>
            </div>
          </div>

          {/* Documents List */}
          {status.documents.length > 0 && (
            <div className="max-h-60 overflow-y-auto space-y-2">
              <h3 className="font-medium text-sm text-gray-700 dark:text-gray-300 sticky top-0 bg-white dark:bg-gray-800 py-2">
                Geüploade Documenten:
              </h3>
              {status.documents.map((doc, index) => (
                <div
                  key={`${doc.id}-${index}`}
                  className="flex items-center gap-2 p-2 bg-gray-50 dark:bg-gray-700/50 rounded text-sm"
                >
                  {doc.status === 'success' ? (
                    <CheckCircle className="w-4 h-4 text-green-600 flex-shrink-0" />
                  ) : (
                    <XCircle className="w-4 h-4 text-red-600 flex-shrink-0" />
                  )}
                  <FileText className="w-4 h-4 text-gray-400 flex-shrink-0" />
                  <div className="flex-1 min-w-0">
                    <p className="truncate text-gray-900 dark:text-white">{doc.title}</p>
                    <p className="text-xs text-gray-500">
                      {doc.system === 'paperless' ? 'Paperless-ngx' : 'Alfresco'}
                    </p>
                  </div>
                </div>
              ))}
            </div>
          )}

          {status.status === 'completed' && (
            <div className="bg-green-50 dark:bg-green-900/20 border border-green-200 dark:border-green-800 rounded-lg p-4">
              <p className="text-green-800 dark:text-green-200 font-medium">
                ✓ Simulatie voltooid!
              </p>
              <p className="text-sm text-green-700 dark:text-green-300 mt-1">
                {status.completed} documenten succesvol geüpload
                {status.failed > 0 && `, ${status.failed} mislukt`}
              </p>
            </div>
          )}
        </div>
      )}

      {/* Instructions */}
      <div className="mt-6 p-4 bg-blue-50 dark:bg-blue-900/20 rounded-lg">
        <h4 className="font-medium text-sm text-blue-900 dark:text-blue-200 mb-2">
          💡 Hoe te gebruiken:
        </h4>
        <ol className="text-sm text-blue-800 dark:text-blue-300 space-y-1 list-decimal list-inside">
          <li>Start de DMS Simulator: <code className="bg-blue-100 dark:bg-blue-900 px-1 rounded">cd dms-simulator && npm run dev</code></li>
          <li>Zorg dat Paperless en/of Alfresco draaien</li>
          <li>Kies aantal documenten en systemen</li>
          <li>Klik "Start Simulatie"</li>
          <li>Bekijk de documenten in Paperless (port 8000) of Alfresco (port 8081)</li>
        </ol>
      </div>
    </div>
  );
}
