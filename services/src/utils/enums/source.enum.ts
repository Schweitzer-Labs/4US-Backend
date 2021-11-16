export enum Source {
  DONATE_FORM = "donate_form",
  DASHBOARD = "dashboard",
  FINICITY = "finicity",
  ACTBLUE = "ActBlue",
}

export enum ExternalSource {
    FINICITY = "finicity",
    ACTBLUE = "ActBlue",
}

export const sources: Source[] = [
  Source.DONATE_FORM,
  Source.DASHBOARD,
  Source.FINICITY,
];
