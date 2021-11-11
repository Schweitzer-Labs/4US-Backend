import * as AWS from "aws-sdk";

import * as dotenv from "dotenv";

dotenv.config();

AWS.config.apiVersions = {
  dynamodb: "2012-08-10",
};

export default async () => {
  return "Actblue bank sync from invoked";
};
