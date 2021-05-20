const parser = require('ua-parser-js');

export const parseUA = (uaStr) => {
  return parser(uaStr);
}
