const AWS = require("aws-sdk");
AWS.config.update({ region: process.env.REGION });
const ses = new AWS.SES({ region: process.env.REGION });

module.exports = async (event, context) => {
  const ddbrecord = event.Records[0].dynamodb;
  const timestamp = ddbrecord.ApproximateCreationDateTime
  const newrecord = ddbrecord.NewImage;
  const firstName = newrecord.firstName.S
      , lastName = newrecord.lastName.S
      , amount = newrecord.amount.N

  console.log("ddb record", ddbrecord);
  console.log("new record", newrecord);
  console.log("name", firstName, lastName);

  let templateData = {};
  templateData.committee = newrecord.committee.S;
  templateData.timestamp = timestamp;
  templateData.donor = [firstName, lastName].join(' ')
  templateData.email = newrecord.email.S;
  templateData.address1 = newrecord.addressLine1.S;
  templateData.address2 = newrecord.addressLine2.S;
  templateData.city = newrecord.city.S;
  templateData.state = newrecord.state.S.toUpperCase();
  templateData.pin = newrecord.postalCode.S;
  templateData.amount = amount;


  let params = {
      Source: "notification@policapital.net",
      Template: 'ContributionNotification',
      Destination: {
        ToAddresses: ['seemant@schweitzerlabs.com'],
      },
      TemplateData: JSON.stringify(templateData)
    };

  console.log("sending email");
  return ses.sendTemplatedEmail(params).promise()
};
