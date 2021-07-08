import "reflect-metadata";
import { ApolloServer } from "apollo-server";
import { buildSchema } from "type-graphql";
import { AppResolver } from "../../src/resolvers/app.resolver";

const GRAPH_QL_PORT = process.env.PORT || 4000;
const AUTH_REDIRECT_PORT = process.env.PORT || 4500;

async function bootstrap() {
  // ... Building schema here

  const schema = await buildSchema({
    resolvers: [AppResolver],
  });

  // Create the GraphQL server
  const server = new ApolloServer({
    schema,
    playground: true,
    context: ({ event }, context) => {
      console.log("hello");
      const currentUser = "380d0179-d813-445d-9032-fc25249b4de7";
      return {
        currentUser,
      };
    },
  });

  // Start the server
  const { url } = await server.listen(GRAPH_QL_PORT);
  console.log(`Server is running, GraphQL Playground available at ${url}`);
}

bootstrap();
