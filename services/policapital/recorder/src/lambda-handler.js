const AWS = require("aws-sdk");
AWS.config.update({ region: process.env.REGION });
const sqs = new AWS.SQS({ apiVersion: '2012-11-05'})
;
/*
 * Helper Functions
 */
const getCommitteeDetails = async (committee) => {
    const emails = {
          'angel-cruz'   : ['seemant@schweitzerlabs.com', 'evan@schweitzerlabs.com']
        , 'john-safford' : ['awsadmin@schweitzerlabs.com']
      }
    ;
    console.log(emails[committee]);
    return { emails: emails[committee], timezone: "America/New_York" };
}; // getCommitteeDetails()

const sendQueue = async (message) => {
    const {emails, timezone} = await getCommitteeDetails(message.committee)
      , params = {
          MessageAttributes: {
            committee: {
                  DataType: "String"
                , StringValue: emails.join(',')
              }
            , tzcommittee: {
                  DataType: "String"
                , StringValue: timezone
              }
          }
        , MessageBody: JSON.stringify(message)
        , MessageDeduplicationId: message.id
        , MessageGroupId: message.committee
        , QueueUrl: process.env.SQSQUEUE
      }
    ;
    console.log("sending to", params.QueueUrl);
    const resp = await sqs.sendMessage(params).promise();
    console.log("sending SQS", resp);
}; // sendQueue()

/*
 * Main Function
 */
module.exports = async (event, context) => {
    const stream   = event.Records[0].dynamodb
        , record   = stream.NewImage
        , date     = new Date(stream.ApproximateCreationDateTime * 1000)
    ;
    console.log("stream", stream);
    const data = {
        id         : stream.id
      , committee  : record.committee.S
      , timestamp  : date
      , donor      : [record.firstName.S, record.lastName.S].join(' ')
      , timezone   : "America/New_York"
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
      , transaction: record.stripePaymentIntentId.S
      , receipt    : record.stripePaymentIntentId.S.slice(-8)
      , refcode    : record.refCode.S || 'N/A'
      , card       : record.cardNumberLastFourDigits.S
    };

    await sendQueue(data);
};
