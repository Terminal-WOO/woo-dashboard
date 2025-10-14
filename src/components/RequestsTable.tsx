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

  return (
    <div className="table-container">
      <table className="requests-table">
        <thead>
          <tr>
            <th>Titel</th>
            <th>Organisatie</th>
            <th>Categorie</th>
            <th>Status</th>
            <th>Ingediend</th>
            <th>Besloten</th>
          </tr>
        </thead>
        <tbody>
          {requests.map((request) => (
            <tr key={request.id}>
              <td className="title-cell">{request.title}</td>
              <td>{request.organization}</td>
              <td>{request.category}</td>
              <td>
                <span
                  className={`status-badge ${getStatusClass(request.status)}`}
                >
                  {request.status}
                </span>
              </td>
              <td>{formatDate(request.submittedDate)}</td>
              <td>
                {request.decidedDate ? formatDate(request.decidedDate) : "-"}
              </td>
            </tr>
          ))}
        </tbody>
      </table>
    </div>
  );
};
