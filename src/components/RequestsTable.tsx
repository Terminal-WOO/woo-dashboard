import { WOORequest } from "../types";

interface RequestsTableProps {
  requests: WOORequest[];
}

export const RequestsTable = ({ requests }: RequestsTableProps) => {
  const getStatusClass = (status: string) => {
    switch (status) {
      case "Ontvangen":
        return "status-received";
      case "In behandeling":
        return "status-in-progress";
      case "1e Concept":
        return "status-concept-1";
      case "2e Concept":
        return "status-concept-2";
      case "Definitief":
        return "status-definitive";
      case "Gepubliceerd":
        return "status-published";
      case "Afgerond":
        return "status-completed";
      default:
        return "";
    }
  };

  const formatDate = (dateString: string) => {
    return new Date(dateString).toLocaleDateString("nl-NL", {
      year: "numeric",
      month: "short",
      day: "numeric",
    });
  };

  const getOrgTypeIcon = (type: string) => {
    switch (type) {
      case "gemeente":
        return "🏛️";
      case "provincie":
        return "🗺️";
      case "ministerie":
        return "🏢";
      default:
        return "📋";
    }
  };

  return (
    <div className="table-container">
      <table className="requests-table">
        <thead>
          <tr>
            <th>ID</th>
            <th>Titel</th>
            <th>Organisatie</th>
            <th>Categorie</th>
            <th>Status</th>
            <th>Ingediend</th>
            <th>Behandelaar</th>
          </tr>
        </thead>
        <tbody>
          {requests.map((request) => (
            <tr key={request.id}>
              <td className="id-cell">
                <code>{request.id}</code>
              </td>
              <td className="title-cell" title={request.subject}>
                {request.title}
              </td>
              <td>
                <span style={{ marginRight: "4px" }}>
                  {getOrgTypeIcon(request.organizationType)}
                </span>
                {request.organization}
              </td>
              <td>{request.category}</td>
              <td>
                <span
                  className={`status-badge ${getStatusClass(request.status)}`}
                >
                  {request.status}
                </span>
              </td>
              <td>{formatDate(request.submittedDate)}</td>
              <td className="handler-cell">{request.handler || "-"}</td>
            </tr>
          ))}
        </tbody>
      </table>
    </div>
  );
};
