const AWS = require("aws-sdk");
AWS.config.update({ region: process.env.REGION });
const sns = new AWS.SNS({ apiVersion: '2010-03-31'})
    , ses = new AWS.SES({ region: process.env.REGION })
;
const from_address = 'notification@policapital.net';

/*
 * Helper Functions
 */
const informAdmins = async (message) => {
    const params = {
          Message: message
        , TopicArn: process.env.SNS_TOPIC
      }
    ;
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
      }
    ;
    const resp = await ses.sendTemplatedEmail(params).promise();
    console.log("sent SES", resp);
  } // emailDonor()
;

/*
 * Main Function
 */
module.exports = async (event, context) => {
    const ddb_stream = event.Records[0].dynamodb
	, ddb_record = ddb_stream.NewImage
        , time_stamp = ddb_stream.ApproximateCreationDateTime * 1000 //milliseconds
    ;
	  const date = new Date(time_stamp);
    console.log(date);

    const data = {
          committee  : ddb_record.committee.S
        , date       : date.toLocaleString('en-US')
        , time       : date.toLocaleString('en-US')
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
        , transaction: ddb_record.stripePaymentIntentId.S.slice(-8)
        , refcode    : ddb_record.refCode.S || 'N/A'
        , card       : ddb_record.cardNumberLastFourDigits.S
      }
    ;
    const message = JSON.stringify(data);
    await informAdmins(message);
    await emailDonor(data.email, message);
  }
;
