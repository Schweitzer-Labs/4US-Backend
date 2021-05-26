import "reflect-metadata";
import { ApolloServer } from "apollo-server-lambda";
import { buildSchema } from "type-graphql";
import { Container } from "typedi";
import { GraphQLSchema } from "graphql";
import { AppResolver } from "./resolvers/app.resolver";
import { forbiddenGraphqlMemberProxy } from "../tests/events/forbidden-graphql-member.proxy";
import { ApplicationError } from "./utils/application-error";
import { StatusCodes } from "http-status-codes";

let schema: GraphQLSchema;

const createHandler = async (event) => {
  const currentUser =
    event?.requestContext?.authorizer?.claims["cognito:username"];
  if (!currentUser) {
    // @Todo break execution when user is not found
    new ApplicationError("Invalid request", {}).toResponse();
  }

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
    context: {
      currentUser,
    },
  });

  return server.createHandler();
};

export default (event, context, callback) => {
  createHandler(event).then((handler: any) => {
    const currentUser =
      event?.requestContext?.authorizer?.claims["cognito:username"];
    return handler(event, { currentUser }, callback);
  });
};
