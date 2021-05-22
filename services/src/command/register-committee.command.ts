import * as t from 'io-ts'

import {DynamoDB} from "aws-sdk";

const RegisterCommitteePayload = t.type({

})


export const createCommittee = (dynamoDB: DynamoDB) => async (registerCommitteePayload: any) => {

}
