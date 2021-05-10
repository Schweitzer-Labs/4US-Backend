const AWS = require("aws-sdk");
AWS.config.update({ region: process.env.REGION });
const ses = new AWS.SES({ region: process.env.REGION });

module.exports = async (event, context) => {
  const ddbrecord = event.Records[0].dynamodb;
  const timestamp = ddbrecord.ApproximateCreationDateTime * 1000 // convert to milliseconds
      , newrecord = ddbrecord.NewImage
      , donorName = [newrecord.firstName.S, newrecord.lastName.S].join(' ')
  ;
  const fromAddress = 'notification@policapital.net';

  console.log("ddb record", ddbrecord);
  console.log("new record", newrecord);

  let templateData = {
    committee: newrecord.committee.S
    , timestamp: new Date(timestamp).toLocaleString('en-US')
    , donor    : donorName
    , email    : newrecord.email.S
    , address1 : newrecord.addressLine1.S
    , address2 : newrecord.addressLine2.S
    , city     : newrecord.city.S
    , state    : newrecord.state.S.toUpperCase()
    , zip      : newrecord.postalCode.S
    , amount   : newrecord.amount.N / 100
    , stripe   : newrecord.stripePaymentIntentId
  };

  let params = {
      Source: fromAddress
      , Template: process.env.POS_TEMPLATE
      , Destination: {
          ToAddresses: ['seemant@schweitzerlabs.com'],
        },
        TemplateData: JSON.stringify(templateData)
    };

  console.log("sending email");
  return ses.sendTemplatedEmail(params).promise()
};
