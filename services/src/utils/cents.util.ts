export const dollarStrToCents = (dollarStr: string): number =>
  Math.round(parseFloat(dollarStr) * 100);
