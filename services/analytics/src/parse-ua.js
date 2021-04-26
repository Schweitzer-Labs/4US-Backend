const parser = require('ua-parser-js');

module.exports = (uaStr) => {
  return parser(uaStr);
}
