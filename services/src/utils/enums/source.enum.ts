export enum Source {
  DONATE_FORM = "donate_form",
  DASHBOARD = "dashboard",
  FINICITY = "finicity",
  ActBlue = "ActBlue",
  WinRed = "WinRed",
}

export enum ExternalSource {
  FINICITY = "finicity",
  ActBlue = "ActBlue",
  WinRed = "WinRed",
}

export const sources: Source[] = [
  Source.DONATE_FORM,
  Source.DASHBOARD,
  Source.FINICITY,
  Source.ActBlue,
  Source.WinRed,
];
