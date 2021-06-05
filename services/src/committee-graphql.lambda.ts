import "reflect-metadata";
import { ApolloServer } from "apollo-server-lambda";
import { buildSchemaSync } from "type-graphql";
import { Container } from "typedi";
import { AppResolver } from "./resolvers/app.resolver";

const schema = buildSchemaSync({
  resolvers: [AppResolver],
  container: Container,
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
