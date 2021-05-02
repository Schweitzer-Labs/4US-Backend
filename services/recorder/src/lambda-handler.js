const payloadVersion = '1.0.0';
var zlib = require('zlib');
var YAML = require('js-yaml');

require('dotenv').config()

const runenv = process.env.RUNENV

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
      return ("Success", data)
    } catch(err) {
      return("Error", err);
    };
}


module.exports = async (event, context) => {
  const payload = Buffer.from(event.awslogs.data, 'base64');
  const logEvents = JSON.parse(zlib.unzipSync(payload).toString()).logEvents;
  
  console.log(logEvents);

  const msg = logEvents[0].message.split('{')[1].split('}')[0].split(',').map(d => d.trim()).join('\n');

  const message = YAML.load(msg);

  let ddbItems = { 'id': { 'S': logEvents[0].id } };
  for (const attr in message) {
    if(attr == 'amount') {
      ddbItems[attr] = { 'N': message[attr].toString() }
    } else {
      ddbItems[attr] = { 'S': message[attr].toString() }
    }
  }

  console.log("Writing", ddbItems);
  let result = await writeDB(ddbItems);
  console.log("Result", result);
  return;
};
