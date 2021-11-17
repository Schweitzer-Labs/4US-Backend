export enum Source {
  DONATE_FORM = "donate_form",
  DASHBOARD = "dashboard",
  FINICITY = "finicity",
  ActBlue = "ActBlue",
}

export enum ExternalSource {
  FINICITY = "finicity",
  ActBlue = "ActBlue",
}

export const sources: Source[] = [
  Source.DONATE_FORM,
  Source.DASHBOARD,
  Source.FINICITY,
  Source.ActBlue,
];
