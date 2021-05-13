import { ApolloServer, gql } from "apollo-server-lambda";

// Construct a schema, using GraphQL schema language
const typeDefs = gql`
  type Query {
    transactions: [Transaction!]!
  }
`;

// Provide resolver functions for your schema fields
const resolvers = {
  Query: {
    hello: () => "Hello me!",
  },
};

const createHandler = async () => {
  const server = new ApolloServer({
    typeDefs,
    resolvers,
    playground: true,
    introspection: true,
  });

  return server.createHandler();
};

export default (event, context, callback) => {
  createHandler().then((handler: any) => handler(event, context, callback));
};
