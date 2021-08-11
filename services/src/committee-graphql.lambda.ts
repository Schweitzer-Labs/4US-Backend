import "reflect-metadata";
import { ApolloServer } from "apollo-server-lambda";
import { buildSchemaSync } from "type-graphql";
import { AppResolver } from "./resolvers/app.resolver";
import * as dotenv from "dotenv";

dotenv.config();

const corsOrigin = process.env.CORS_ORIGIN;

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

const lambda = server.createHandler({
  cors: {
    origin: corsOrigin,
  },
});

export default lambda;
