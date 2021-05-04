import config from "./enums";
import * as AWS from "aws-sdk";

AWS.config.update({
  region: "us-east-1",
});

const ps = new AWS.SSM();

export default {
  get: async (env, name) => {
    switch (name) {
      case config.stripeApiKey:
        const fullKey = `/${env}${name}`;
        console.log('Config look up initiated on key: ', fullKey)
        const res = await ps
          .getParameter({
            Name: fullKey,
            WithDecryption: true,
          })
          .promise();
        return res.Parameter.Value;
      default:
        console.error('Config not found')
        throw new Error("Config not found");
    }
  },
};
