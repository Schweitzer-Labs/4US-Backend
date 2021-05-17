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
    for (const stream of event.Records) {
        const record = stream.dynamodb
            , row    = record.NewImage
            , date   = new Date(record.ApproximateCreationDateTime * 1000)
        ;
        console.log("record", record);

        const data = {
            id         : record.id
          , committee  : row.committee.S
          , timestamp  : date
          , donor      : [row.firstName.S, row.lastName.S].join(' ')
          , timezone   : "America/New_York"
          , email      : row.email.S
          , occupation : row.occupation.S
          , employer   : row.employer.S
          , address1   : row.addressLine1.S
          , address2   : row.addressLine2.S
          , city       : row.city.S
          , state      : row.state.S.toUpperCase()
          , zip        : row.postalCode.S
          , phone      : row.phoneNumber.S
          , amount     : (row.amount.N / 100).toFixed(2)
          , transaction: row.stripePaymentIntentId.S
          , receipt    : row.stripePaymentIntentId.S.slice(-8)
          , refcode    : row.refCode.S || 'N/A'
          , card       : row.cardNumberLastFourDigits.S
        };

        await sendQueue(data);
    }
};
