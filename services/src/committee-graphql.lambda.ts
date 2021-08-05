import "reflect-metadata";
import { ApolloServer } from "apollo-server-lambda";
import { buildSchemaSync } from "type-graphql";
import { AppResolver } from "./resolvers/app.resolver";
import headers from "./utils/headers";

const schema = buildSchemaSync({
  resolvers: [AppResolver],
});

const server = new ApolloServer({
  schema,
  introspection: true,
  context: ({ event }, context) => {
    console.log("graphql event", JSON.stringify(event));
    const currentUser =
      event?.requestContext?.authorizer?.claims["cognito:username"];
    return {
      currentUser,
    };
  },
});

export default server.createHandler();
