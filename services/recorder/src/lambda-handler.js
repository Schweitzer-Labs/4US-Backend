const AWS = require("aws-sdk");
AWS.config.update({ region: process.env.REGION });
const sqs = new AWS.SQS({ apiVersion: '2012-11-05'})
;
/*
 * Helper Functions
 */
const getCommitteeEmail = async (committee) => {
    const emails = {
          'angel-cruz'   : ['seemant@schweitzerlabs.com', 'evan@schweitzerlabs.com']
        , 'john-safford' : ['awsadmin@schweitzerlabs.com']
      }
    ;
    return emails[committee].join(',');
}; // getCommitteeEmail()

const queueEmail = async (message, recip) => {
    let dest = recip == 'committee'
      ? await getCommitteeEmail(message.committee)
      : message.email
    ;
    let params = {
      MessageAttributes: {
        recipient: {
          DataType: "String",
          StringValue: recip
        },
        email: {
          DataType: "String",
          StringValue: dest
        },
      },
      MessageBody: JSON.stringify(message),
      MessageDeduplicationId: message.id,
      MessageGroupId: message.committee,
      QueueUrl: process.env.SQSQUEUE
    };

    console.log("sending to", params.QueueUrl);
    const resp = await sqs.sendMessage(params).promise();
    console.log("sending SQS", resp);
}; // queueEmail()

/*
 * Main Function
 */
module.exports = async (event, context) => {
    const stream   = event.Records[0].dynamodb
        , record   = stream.NewImage
        , date     = new Date(stream.ApproximateCreationDateTime * 1000)
        , timeopts = { timeZone: 'America/New_York', timeStyle: 'short', dateStyle: 'long' }
    ;
    console.log("stream", stream);
    const data = {
        id         : stream.id
      , committee  : record.committee.S
      , timestamp  : date.toLocaleString('en-US', timeopts)
      , donor      : [record.firstName.S, record.lastName.S].join(' ')
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

    await queueEmail(data, "donor");
    await queueEmail(data, "committee");
};
