const AWS = require("aws-sdk");
AWS.config.update({ region: process.env.REGION });
const sns = new AWS.SNS({ apiVersion: '2010-03-31'})
    , ses = new AWS.SES()
;
const from_address = 'notification@policapital.net';

/*
 * Helper Functions
 */
const informAdmins = async (message) => {
    const params = {
        Message: message
      , TopicArn: process.env.SNS_TOPIC
    };
    const resp = await sns.publish(params).promise();
    console.log("send SNS", resp);
  } // informAdmins()
;

const emailDonor = async (address, message) => {
    const params = {
        Source      : from_address
      , Template    : process.env.POS_TEMPLATE
      , Destination : {
          ToAddresses: [address]
        }
      , TemplateData: message
      , ConfigurationSetName: 'SNSDebugging'
    };
    const resp = await ses.sendTemplatedEmail(params).promise();
    console.log("sent SES", resp);
  } // emailDonor()
;

/*
 * Main Function
 */
module.exports = async (event, context) => {
    const stream   = event.Records[0].dynamodb
        , record   = stream.NewImage
        , date     = new Date(stream.ApproximateCreationDateTime * 1000)
        , timeopts = { timeZone: 'America/New_York', timeStyle: 'short', dateStyle: 'long' }
    ;
    const data = {
        committee  : record.committee.S
      , timestamp  : date.toLocaleString('en-US', timeopts)
      , donor      : [record.firstName.S, ddb_record.lastName.S].join(' ')
      , email      : record.email.S
      , occupation : record.occupation.S
      , employer   : record.employer.S
      , address1   : record.addressLine1.S
      , address2   : record.addressLine2.S
      , city       : record.city.S
      , state      : record.state.S.toUpperCase()
      , zip        : record.postalCode.S
      , phone      : record.phoneNumber.S
      , amount     : (record.amount.N / 100).toFixed(2)
      , transaction: record.stripePaymentIntentId.S.slice(-8)
      , refcode    : record.refCode.S || 'N/A'
      , card       : record.cardNumberLastFourDigits.S
    };
    const message = JSON.stringify(data);
    await informAdmins(message);
    await emailDonor(data.email, message);
  }
;
