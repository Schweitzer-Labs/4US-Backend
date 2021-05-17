const AWS = require("aws-sdk");
AWS.config.update({ region: process.env.REGION });
const ses = new AWS.SES();
const from_address = 'notification@policapital.net';

/*
 * Helper Functions
 */
const informAdmins = async (message) => {
    const params = {
        Message: JSON.stringify(message)
      , TopicArn: process.env.SNS_TOPIC
    };
    // const resp = await sns.publish(params).promise();
    console.log("send SNS", resp);
}; // informAdmins()

const sendEmail = async (addresses, message, template) => {
    const params = {
        Source      : from_address
      , Template    : template
      , Destination : {
          ToAddresses: addresses
        }
      , TemplateData: JSON.stringify(message)
      , ConfigurationSetName: 'SNSDebugging'
    };
    const resp = await ses.sendTemplatedEmail(params).promise();
    console.log("sent SES", resp);
    return resp;
}; // sendEmail()

const emailDonor = async (message) => {
}; // emailDonor()

const emailCommittee = async (message) => {
}; // emailCommittee()

/*
 * Main Function
 */
module.exports = async (event, context) => {
    for (const receipt in event.Records) {
        let data = receipt.body;
        console.log(receipt.messageAttributes, data);

        emailDonor(data);
        emailCommittee(data);
    };

    return;
};
