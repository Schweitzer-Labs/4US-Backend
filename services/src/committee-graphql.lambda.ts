import "reflect-metadata";
import { ApolloServer, gql } from "apollo-server-lambda";
import { buildSchema } from "type-graphql";
import { Container } from "typedi";
import { GraphQLSchema } from "graphql";
import { AppResolver } from "./resolvers/app.resolver";

let schema: GraphQLSchema;

const createHandler = async () => {
  if (!schema) {
    schema = await buildSchema({
      resolvers: [AppResolver],
      container: Container,
    });
  }

  const server = new ApolloServer({
    schema,
    playground: true,
    introspection: true,
  });

  return server.createHandler();
};

export default (event, context, callback) => {
  createHandler().then((handler: any) => {
    return handler(event, context, callback);
  });
};