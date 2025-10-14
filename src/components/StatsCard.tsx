interface StatsCardProps {
  title: string;
  value: number | string;
  icon: string;
  color: string;
}

export const StatsCard = ({ title, value, icon, color }: StatsCardProps) => {
  return (
    <div className="stats-card" style={{ borderLeftColor: color }}>
      <div className="stats-icon" style={{ backgroundColor: `${color}15` }}>
        {icon}
      </div>
      <div className="stats-content">
        <h3>{title}</h3>
        <p className="stats-value">{value}</p>
      </div>
    </div>
  );
};
