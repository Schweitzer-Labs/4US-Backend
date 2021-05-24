import * as AWS from "aws-sdk";
import {
  ITransaction,
  Transaction,
} from "./queries/search-transactions.decoder";
import { isLeft } from "fp-ts/Either";
import { ApplicationError } from "./utils/application-error";
import { PathReporter } from "io-ts/PathReporter";
import * as t from "io-ts";
import { SQSEvent } from "aws-lambda";
import { SendTemplatedEmailResponse } from "aws-sdk/clients/ses";

const MessageAttributes = t.type({
  committeeEmailAddress: t.type({
    stringValue: t.string,
  }),
  committeeTzDatabaseName: t.type({
    stringValue: t.string,
  }),
  committeeName: t.type({
    stringValue: t.string,
  }),
});

type IMessageAttributes = t.TypeOf<typeof MessageAttributes>;

const ses = new AWS.SES(),
  sns = new AWS.SNS();
const from_address = "notification@policapital.net";
const posReceiptTemplate: any = process.env.POS_RECEIPT;
const posRecordTemplate: any = process.env.POS_RECORD;

/*
 * Main Function
 */
export default async (event: SQSEvent, context): Promise<any[]> => {
  console.log(JSON.stringify(event));
  let res = [];

  for (const record of event.Records) {
    const data = JSON.parse(record.body);

    const eitherTxn = Transaction.decode(data);
    const eitherAttrs = MessageAttributes.decode(record.messageAttributes);

    if (isLeft(eitherTxn) || isLeft(eitherAttrs)) {
      throw new ApplicationError("Invalid sqs event data", {
        transaction: PathReporter.report(eitherTxn),
        attributes: PathReporter.report(eitherAttrs),
      });
    }

    const txn = eitherTxn.right;
    const attrs = eitherAttrs.right;

    const donorRes = await emailDonor(posReceiptTemplate)(txn, attrs);
    const committeeRes = await emailCommittee(posRecordTemplate)(txn, attrs);
    res.push({
      donorRes,
      committeeRes,
    });
  }

  return res;
};

const notifyAdmins = async (message) => {
  const params = {
    Message: JSON.stringify(message),
    TopicArn: process.env.SNS_TOPIC,
  };
  const resp = await sns.publish(params).promise();
};

const txnToTemplateData = (txn: ITransaction) => ({
  ...txn,
  timezone: "America/New_York",
  amount: (txn.amount / 100).toFixed(2),
  state: txn.state.toUpperCase(),
  receipt: txn.id,
  timestamp: new Date(parseInt(txn.initiatedTimestamp)).toLocaleString(
    "en-US",
    {
      timeZone: "America/New_York",
    }
  ),
  refCode: txn.refCode || "N/A",
  email: txn.emailAddress,
  addressLine2: txn.addressLine2 || "",
});

const sendEmail = async (txn: ITransaction, parameters, committee: string) => {
  const templateData = {
    ...txnToTemplateData(txn),
    committee,
  };
  const params = {
    ...parameters,
    Source: from_address,
    TemplateData: JSON.stringify(templateData),
    ConfigurationSetName: "SNSDebugging",
  };
  return await ses.sendTemplatedEmail(params).promise();
};

const emailDonor =
  (posReceiptTemplate: string) =>
  async (
    txn: ITransaction,
    attrs: IMessageAttributes
  ): Promise<SendTemplatedEmailResponse> => {
    const params = {
      Template: posReceiptTemplate,
      Destination: {
        ToAddresses: [txn.emailAddress],
      },
    };
    console.log("donor mail params", params);
    return await sendEmail(txn, params, attrs.committeeName.stringValue);
  };

const emailCommittee =
  (posRecordTemplate: string) =>
  async (
    txn: ITransaction,
    attrs: IMessageAttributes
  ): Promise<SendTemplatedEmailResponse> => {
    const params = {
      Template: posRecordTemplate,
      Destination: {
        ToAddresses: attrs.committeeEmailAddress.stringValue.split(","),
      },
    };
    console.log("committee mail params", params);
    return await sendEmail(txn, params, attrs.committeeName.stringValue);
  };
