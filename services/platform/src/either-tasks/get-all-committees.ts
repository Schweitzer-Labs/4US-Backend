import { Client } from "elasticsearch";

export const getAllCommittees = (esClient: Client) => {
  esClient.search();
};
