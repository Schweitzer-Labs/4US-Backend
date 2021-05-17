const AWS = require("aws-sdk");
AWS.config.update({ region: process.env.REGION });
const ses = new AWS.SES()
  , sns = new AWS.SNS()
;
const from_address = 'notification@policapital.net';

/*
 * Helper Functions
 */
const notifyAdmins = async (message) => {
    const params = {
        Message: JSON.stringify(message)
      , TopicArn: process.env.SNS_TOPIC
    };
    const resp = await sns.publish(params).promise();
    console.log("send SNS", resp);
}; // notifyAdmins()

const sendEmail = async (message, parameters) => {
    const params = {
        ...parameters
      , Source              : from_address
      , TemplateData        : JSON.stringify(message)
      , ConfigurationSetName: 'SNSDebugging'
    };
    console.log("params", params);
    const resp = await ses.sendTemplatedEmail(params).promise();
    console.log("sent SES", resp);
    return resp;
}; // sendEmail()

const emailDonor = async (message) => {
    const params = {
        Template    : process.env.POS_RECEIPT
      , Destination : {
          ToAddresses: [message.email]
        }
    };
    await sendEmail(message, params);
}; // emailDonor()

const emailCommittee = async (message, attrs) => {
    const params = {
        Template   : process.env.POS_RECORD
      , Destination: {
          ToAddresses: attrs.committee.stringValue.split(',')
        }
    };
    await sendEmail(message, params);
}; // emailCommittee()

/*
 * Main Function
 */
module.exports = async (event, context) => {
    for (const record of event.Records) {
        let data  = JSON.parse(record.body)
          , attrs = record.messageAttributes
        ;
        console.log(data);

        await emailDonor(data);
        await emailCommittee(data, attrs);
    };

    return;
};
