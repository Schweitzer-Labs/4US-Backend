import "reflect-metadata";
import { ApolloServer } from "apollo-server";
import { buildSchema } from "type-graphql";
import { AppResolver } from "../../src/resolvers/app.resolver";
import * as dotenv from "dotenv";

dotenv.config();

const GRAPH_QL_PORT = process.env.PORT || 4000;
const AUTH_REDIRECT_PORT = process.env.PORT || 4500;

export async function bootstrapGql() {
  // ... Building schema here

  const schema = await buildSchema({
    resolvers: [AppResolver],
  });

  // Create the GraphQL server
  const server = new ApolloServer({
    schema,
    introspection: true,
    context: ({ event }, context) => {
      const currentUser = process.env.COGNITO_USER_ID;
      return {
        currentUser,
      };
    },
  });

  // Start the server
  const { url } = await server.listen(GRAPH_QL_PORT);
  console.log(`Server is running, GraphQL Playground available at ${url}`);
}
