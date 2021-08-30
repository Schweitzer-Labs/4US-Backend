addEventListener("fetch", (event) => {
  event.respondWith(
    handleRequest(event.request).catch(
      (err) => new Response(err.stack, { status: 500 })
    )
  );
});

/**
 * Respond with hello worker text
 * @param {Request} request
 */
async function handleRequest(request) {
  const redirectCode = 301;

  const url = new URL(request.url)
    , { searchParams } = url
    , committee = searchParams.get('committeeId');
  ;

  if(searchParams.has('committeeId')) {
    searchParams.delete('committeeId');
  }

  /* Rewrite the pathname and query string */
  url.pathname = `/committee/${committee}`;
  url.search = searchParams.toString();

  return new Response(
        ''
      , { status: redirectCode, headers: { 'Location': url.pathname + url.search } }
    )
  ;
}
