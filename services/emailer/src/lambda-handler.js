const AWS = require("aws-sdk");
AWS.config.update({ region: process.env.REGION });


module.exports = async (event, context) => {
  console.log(event);

  ddbrecord = event.Records[0].dynamodb

  console.log("ddbrecord", ddbrecord)
};
