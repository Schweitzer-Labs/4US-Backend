import parser from "ua-parser-js";

export const parseUA = (uaStr) => {
  return parser(uaStr);
};
