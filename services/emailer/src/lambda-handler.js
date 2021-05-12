const AWS = require("aws-sdk");
AWS.config.update({ region: process.env.REGION });
const sns = new AWS.SNS({ apiVersion: '2010-03-31'})
    , ses = new AWS.SES({ region: process.env.REGION })
;
const from_address = 'notification@policapital.net';

/*
 * Helper Functions
 */
const informAdmins = (message) => {
  const params = {
        Message: message
      , TopicArn: process.env.SNS_Topic
    }
  ;
  const resp = await sns.publish(params).promise();
  console.log("send SNS", resp);
} // informAdmins()


const emailDonor = (address, message) => {
  const params = {
        Source      : from_address
      , Template    : process.env.POS_TEMPLATE
      , Destination : {
          ToAddresses: [address]
        }
      , TemplateData: message
      , ConfigurationSetName: 'SNSDebugging'
    }
  ;
  const resp = await ses.sendTemplatedEmail(params).promise();
  console.log("sent SES", resp);
} // emailDonor()


/*
 * Main Function
 */
module.exports = async (event, context) => {
  const ddb_record = event.Records[0].dynamodb.NewImage
      , time_stamp = ddb_record.ApproximateCreationDateTime * 1000 //convert to milliseconds
  ;
  console.log("new record", ddb_record);

  const data = {
        committee  : ddb_record.committee.S
      , time_stamp : new Date(time_stamp).toLocaleString('en-US')
      , donor      : [ddb_record.firstName.S, ddb_record.lastName.S].join(' ')
      , email      : ddb_record.email.S
      , occupation : ddb_record.occupation.S
      , employer   : ddb_record.employer.S
      , address1   : ddb_record.addressLine1.S
      , address2   : ddb_record.addressLine2.S
      , city       : ddb_record.city.S
      , state      : ddb_record.state.S.toUpperCase()
      , zip        : ddb_record.postalCode.S
      , phone      : ddb_record.phoneNumber.S
      , amount     : (ddb_record.amount.N / 100).toFixed(2)
      , transaction: ddb_record.stripePaymentIntentId.S
      , refcode    : ddb_record.refCode.S || 'N/A'
      , card       : ddb_record.cardNumberLastFourDigits.S
    }
  ;
  const message = JSON.stringify(data);
  informAdmins(message);
  emailDonor(data.email, message);
};
