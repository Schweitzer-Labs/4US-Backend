const allow_origin = "https://" + process.env.DNS_URL

export default {
  "Access-Control-Allow-Headers": "Content-Type",
  "Access-Control-Allow-Origin": allow_origin,
  "Access-Control-Allow-Methods": "OPTIONS,POST,GET",
};
