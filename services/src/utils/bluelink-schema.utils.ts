import Joi from "joi";
import { stringOpt, stringReq } from "./joi.utils";
import { enumToValues } from "./enums/poly.util";
import { State } from "./enums/state.enum";
import { employmentStatuses } from "./enums/employment-status";
import { DataSource, dataSources } from "./enums/data-source.enum";

export const bluelinkTxnSchema = Joi.object({
  recipientId: stringReq(),
  source: Joi.string()
    .valid(...dataSources)
    .required(),
  recipientGovId: stringOpt(),
  paymentDate: Joi.number().integer().required(),
  amount: Joi.number().integer().min(0).max(2000000000).required(),
  firstName: stringReq(),
  middleName: stringOpt(),
  reportType: stringOpt(),
  lastName: stringReq(),
  addressLine1: stringReq(),
  city: stringReq(),
  state: Joi.string()
    .valid(...enumToValues(State))
    .required(),
  country: stringReq(),
  postalCode: stringReq(5, 10),
  emailAddress: Joi.string().email(),
  employer: stringOpt(),
  employmentStatus: Joi.string().valid(...employmentStatuses),
  occupation: stringOpt(),
  refCode: stringOpt(),
  addressLine2: stringOpt(),
  phoneNumber: stringOpt(5, 15),
  metadata: Joi.any(),
});

export const bluelinkSchema = Joi.object({
  transactions: Joi.array().items(bluelinkTxnSchema).min(1).required(),
}).required();
