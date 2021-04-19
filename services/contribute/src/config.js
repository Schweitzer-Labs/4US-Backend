const { configKey } = require("./enums");
const AWS = require("aws-sdk");
AWS.config.update({
  region: "us-east-1",
});

const ps = new AWS.SSM();

module.exports = {
  get: async (env, name) => {
    switch (name) {
      case configKey.stripeApiKey:
        const fullKey = `/${env}${name}`;
        const res = await ps
          .getParameter({
            Name: fullKey,
          })
          .promise();
        return res.Parameter.Value;
      default:
        throw new Error("Config not found");
    }
  },
};
