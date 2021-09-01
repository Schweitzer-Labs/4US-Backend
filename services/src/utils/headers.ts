export const headers = (allowOrigin: string) => ({
  "access-control-allow-headers": "Content-Type",
  "access-control-allow-origin": allowOrigin,
  "access-control-allow-methods": "OPTIONS,POST,GET",
});
