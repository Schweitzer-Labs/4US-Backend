export const dashToUnderscore = (str: string) =>
  str
    .split("")
    .map((char) => (char === "-" ? "_" : char))
    .join("");
