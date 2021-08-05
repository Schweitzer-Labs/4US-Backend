export const enumToKeys = <t>(e: t): string[] => Object.keys(e);
export const enumToValues = <t>(e: t): string[] =>
  Object.keys(e).map((key) => e[key]);
