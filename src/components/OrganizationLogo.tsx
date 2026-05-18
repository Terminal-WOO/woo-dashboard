import { getOrganizationLogoUrl } from "../organizationLogos";

interface OrganizationLogoProps {
  organization: string;
  size?: number;
  className?: string;
}

export function OrganizationLogo({
  organization,
  size = 28,
  className = "",
}: OrganizationLogoProps) {
  const src = getOrganizationLogoUrl(organization);

  if (!src) {
    return (
      <span
        className={`org-logo org-logo-fallback ${className}`.trim()}
        style={{ width: size, height: size, fontSize: size * 0.45 }}
        title={organization}
        aria-hidden
      >
        {organization.charAt(0)}
      </span>
    );
  }

  return (
    <img
      src={src}
      alt=""
      height={size}
      className={`org-logo ${className}`.trim()}
      title={organization}
      loading="lazy"
      decoding="async"
    />
  );
}
