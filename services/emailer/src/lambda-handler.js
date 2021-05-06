const AWS = require("aws-sdk");
AWS.config.update({ region: process.env.REGION });
const ses = new AWS.SES({ region: process.env.REGION });

module.exports = async (event, context) => {
  let ddbrecord = event.Records[0].dynamodb;
  console.log("ddbrecord", ddbrecord);

  let params = {
    Destination: {
      ToAddresses: ['seemant@schweitzerlabs.com'],
    },
    Message: {
      Body: {
        Text: { Data: "Ping" },
      },

      Subject: { Data: "New Contribution received" },
    },
    Source: "notification@policapital.net",
  };
 
  return ses.sendEmail(params).promise()
};
