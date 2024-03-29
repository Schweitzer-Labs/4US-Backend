export const genGraphQLProxy = (
  query: string,
  userName: string,
  variables = {}
) => {
  const body = {
    variables,
    operationName: null,
    query,
  };
  const encodedBody = JSON.stringify(body);
  return {
    body: encodedBody,
    ...proxyTemplate(userName),
  };
};

const proxyTemplate = (userName: string) => ({
  resource: "/api/committee/graphql",
  path: "/api/committee/graphql/",
  httpMethod: "POST",
  headers: {
    "Accept-Encoding": "gzip, deflate, br",
    Authorization:
      "Bearer eyJraWQiOiJiR1BDY0Jhcng5dWdZcVoyVXVuSXptRnVOT0hwY1lYSXdYVjBuT0JSS1BBPSIsImFsZyI6IlJTMjU2In0.eyJhdF9oYXNoIjoiclhmRXE0Nm1QRi1jcjB6N0xyZzhoUSIsInN1YiI6ImUxYjRiOGRiLWEyNGQtNGI1Yy1hZjE3LTBiNWVlNmVmNmI2NyIsImF1ZCI6IjZsdTlqdHRsYjc0MHM4YWJmMjY4MzU4M2IiLCJlbWFpbF92ZXJpZmllZCI6dHJ1ZSwidG9rZW5fdXNlIjoiaWQiLCJhdXRoX3RpbWUiOjE2MjI3NDI2MTUsImlzcyI6Imh0dHBzOlwvXC9jb2duaXRvLWlkcC51cy13ZXN0LTIuYW1hem9uYXdzLmNvbVwvdXMtd2VzdC0yX0pTanE3SFBXeSIsImNvZ25pdG86dXNlcm5hbWUiOiJlMWI0YjhkYi1hMjRkLTRiNWMtYWYxNy0wYjVlZTZlZjZiNjciLCJleHAiOjE2MjI3NDYyMTUsImlhdCI6MTYyMjc0MjYxNSwiZW1haWwiOiJldmFuQHNjaHdlaXR6ZXJsYWJzLmNvbSJ9.7Vl7FcmeN7QjKgN45GGFFJnFKYMKNEngjI85O_6nLLBfAgq9gNmClyuaFuaP7k3dvr-xWchoMUmSpo2WIirGnyqhxDtedbILLSt5iy-Hri1QN8RDLeex80bOmi70cKezyfPMnzyFRDe7mcY1tnBiA9gzKFiHaedf7r3f1hXt0JOEU88o8gdL0EK2W1D3aqgkP46pzxHhJMup82i7P0oeeaLmhQ8X1weBJ7rpaOHmlsQqCcS1_B6pIHg8MRwq5mLcThQUrHLysDbVqvVjq9RIezrNyxYxOXWLqAaUkqJ70-mtYq5rfWwfY-AWrFXAafUyjvNQCCYrexo_nVxgfPgtwg",
    "cache-control": "no-cache",
    "CloudFront-Forwarded-Proto": "https",
    "CloudFront-Is-Desktop-Viewer": "true",
    "CloudFront-Is-Mobile-Viewer": "false",
    "CloudFront-Is-SmartTV-Viewer": "false",
    "CloudFront-Is-Tablet-Viewer": "false",
    "CloudFront-Viewer-Country": "US",
    "content-type": "application/json",
    Host: "re6vqpuki7.execute-api.us-west-2.amazonaws.com",
    origin: "http://localhost:3000",
    pragma: "no-cache",
    "User-Agent": "Amazon CloudFront",
    Via: "1.1 ffb3cace5d647f21fdf8c68c16a8f2fa.cloudfront.net (CloudFront), 1.1 e5accc89e6f6f7fa6c73134d02aeb429.cloudfront.net (CloudFront)",
    "X-Amz-Cf-Id": "B5q-zLRaTNp0dMepWQucJNCfnFWTthzqGwN7KImq-e6Xjb-J3LB9IA==",
    "X-Amzn-Trace-Id": "Root=1-60b91659-65ed534e1021ce9d5f07b023",
    "X-Forwarded-For": "173.52.219.239, 130.176.116.155, 130.176.28.146",
    "X-Forwarded-Port": "443",
    "X-Forwarded-Proto": "https",
  },
  multiValueHeaders: {
    "Accept-Encoding": ["gzip, deflate, br"],
    Authorization: [
      "Bearer eyJraWQiOiJiR1BDY0Jhcng5dWdZcVoyVXVuSXptRnVOT0hwY1lYSXdYVjBuT0JSS1BBPSIsImFsZyI6IlJTMjU2In0.eyJhdF9oYXNoIjoiclhmRXE0Nm1QRi1jcjB6N0xyZzhoUSIsInN1YiI6ImUxYjRiOGRiLWEyNGQtNGI1Yy1hZjE3LTBiNWVlNmVmNmI2NyIsImF1ZCI6IjZsdTlqdHRsYjc0MHM4YWJmMjY4MzU4M2IiLCJlbWFpbF92ZXJpZmllZCI6dHJ1ZSwidG9rZW5fdXNlIjoiaWQiLCJhdXRoX3RpbWUiOjE2MjI3NDI2MTUsImlzcyI6Imh0dHBzOlwvXC9jb2duaXRvLWlkcC51cy13ZXN0LTIuYW1hem9uYXdzLmNvbVwvdXMtd2VzdC0yX0pTanE3SFBXeSIsImNvZ25pdG86dXNlcm5hbWUiOiJlMWI0YjhkYi1hMjRkLTRiNWMtYWYxNy0wYjVlZTZlZjZiNjciLCJleHAiOjE2MjI3NDYyMTUsImlhdCI6MTYyMjc0MjYxNSwiZW1haWwiOiJldmFuQHNjaHdlaXR6ZXJsYWJzLmNvbSJ9.7Vl7FcmeN7QjKgN45GGFFJnFKYMKNEngjI85O_6nLLBfAgq9gNmClyuaFuaP7k3dvr-xWchoMUmSpo2WIirGnyqhxDtedbILLSt5iy-Hri1QN8RDLeex80bOmi70cKezyfPMnzyFRDe7mcY1tnBiA9gzKFiHaedf7r3f1hXt0JOEU88o8gdL0EK2W1D3aqgkP46pzxHhJMup82i7P0oeeaLmhQ8X1weBJ7rpaOHmlsQqCcS1_B6pIHg8MRwq5mLcThQUrHLysDbVqvVjq9RIezrNyxYxOXWLqAaUkqJ70-mtYq5rfWwfY-AWrFXAafUyjvNQCCYrexo_nVxgfPgtwg",
    ],
    "cache-control": ["no-cache"],
    "CloudFront-Forwarded-Proto": ["https"],
    "CloudFront-Is-Desktop-Viewer": ["true"],
    "CloudFront-Is-Mobile-Viewer": ["false"],
    "CloudFront-Is-SmartTV-Viewer": ["false"],
    "CloudFront-Is-Tablet-Viewer": ["false"],
    "CloudFront-Viewer-Country": ["US"],
    "content-type": ["application/json"],
    Host: ["re6vqpuki7.execute-api.us-west-2.amazonaws.com"],
    origin: ["http://localhost:3000"],
    pragma: ["no-cache"],
    "User-Agent": ["Amazon CloudFront"],
    Via: [
      "1.1 ffb3cace5d647f21fdf8c68c16a8f2fa.cloudfront.net (CloudFront), 1.1 e5accc89e6f6f7fa6c73134d02aeb429.cloudfront.net (CloudFront)",
    ],
    "X-Amz-Cf-Id": ["B5q-zLRaTNp0dMepWQucJNCfnFWTthzqGwN7KImq-e6Xjb-J3LB9IA=="],
    "X-Amzn-Trace-Id": ["Root=1-60b91659-65ed534e1021ce9d5f07b023"],
    "X-Forwarded-For": ["173.52.219.239, 130.176.116.155, 130.176.28.146"],
    "X-Forwarded-Port": ["443"],
    "X-Forwarded-Proto": ["https"],
  },
  queryStringParameters: null,
  multiValueQueryStringParameters: null,
  pathParameters: null,
  stageVariables: null,
  requestContext: {
    resourceId: "ytf2xu",
    authorizer: {
      claims: {
        at_hash: "rXfEq46mPF-cr0z7Lrg8hQ",
        sub: "e1b4b8db-a24d-4b5c-af17-0b5ee6ef6b67",
        aud: "6lu9jttlb740s8abf2683583b",
        email_verified: "true",
        token_use: "id",
        auth_time: "1622742615",
        iss: "https://cognito-idp.us-west-2.amazonaws.com/us-west-2_JSjq7HPWy",
        "cognito:username": userName,
        exp: "Thu Jun 03 18:50:15 UTC 2021",
        iat: "Thu Jun 03 17:50:15 UTC 2021",
        email: "evan@schweitzerlabs.com",
      },
    },
    resourcePath: "/api/committee/graphql",
    httpMethod: "POST",
    extendedRequestId: "AXBuDGfbvHcFeYw=",
    requestTime: "03/Jun/2021:17:50:17 +0000",
    path: "/qa/api/committee/graphql/",
    accountId: "396408709989",
    protocol: "HTTP/1.1",
    stage: "qa",
    domainPrefix: "re6vqpuki7",
    requestTimeEpoch: 1622742617716,
    requestId: "81b9e585-6a5f-4c0c-a54e-bfd1f7745ac3",
    identity: {
      cognitoIdentityPoolId: null,
      accountId: null,
      cognitoIdentityId: null,
      caller: null,
      sourceIp: "130.176.116.155",
      principalOrgId: null,
      accessKey: null,
      cognitoAuthenticationType: null,
      cognitoAuthenticationProvider: null,
      userArn: null,
      userAgent: "Amazon CloudFront",
      user: null,
    },
    domainName: "re6vqpuki7.execute-api.us-west-2.amazonaws.com",
    apiId: "re6vqpuki7",
  },
  isBase64Encoded: false,
});
