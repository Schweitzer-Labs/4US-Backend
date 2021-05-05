const payloadVersion = '1.0.0';
var zlib = require('zlib');
var YAML = require('js-yaml');

require('dotenv').config()

const AWS = require('aws-sdk');
AWS.config.update({region: process.env.REGION});

const ddb = new AWS.DynamoDB({apiVersion: '2012-08-10'})
    , tableName = process.env.DDBTABLE
;

const writeDB = async (items) => {
    let params = {
      TableName: tableName,
      Item: items
    };
    try {
      let data = await ddb.putItem(params).promise();
      return {'message': "Success"}
    } catch(err) {
      return new Error("DynamoDB putItem failed", err)
    };
}

const extractMessage = (message) => {
  return message
        .split('{')[1]
        .split('}')[0]
        .split(',')
        .map(d => d.trim())
        .join('\n')
}

const ddbRecord = (message) => {
  let record = {}

  for (const attr in message) {
    const attrType = attr == 'amount' ? 'N' : 'S'
    record[attr] = {}
    record[attr][attrType] = message[attr].toString()
  }
  return record;
}


module.exports = async (event, context) => {
  const payload   = Buffer.from(event.awslogs.data, 'base64')
  const logEvent  = JSON.parse(zlib.unzipSync(payload).toString()).logEvents[0]

  const message = extractMessage(logEvent.message)
  const yaml    = YAML.load(message)
  yaml.id = logEvent.id
  const row     = ddbRecord(yaml)

  console.log("Writing to DDB", row);
  let result = await writeDB(row);
  console.log("Result", result);
  return;
};
