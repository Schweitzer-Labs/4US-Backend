const parser = require('ua-parser-js');

module.exports = (uaStr) => {
  const res = parser(uaStr)
  console.log(res)
  return res;
}
