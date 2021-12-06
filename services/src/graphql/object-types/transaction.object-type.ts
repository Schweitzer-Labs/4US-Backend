import { Field, ID, InputType, ObjectType } from "type-graphql";

import { registerEnumType } from "type-graphql";
import { Direction } from "../../utils/enums/direction.enum";
import {
  InKindType,
  PaymentMethod,
} from "../../utils/enums/payment-method.enum";
import { PurposeCode } from "../../utils/enums/purpose-code.enum";
import { EntityType } from "../../utils/enums/entity-type.enum";
import { TransactionType } from "../../utils/enums/transaction-type.enum";
import { EmploymentStatus } from "../../utils/enums/employment-status";
import { State } from "../../utils/enums/state.enum";
import { ITransaction } from "../../model/transaction.type";
import { Source } from "../../utils/enums/source.enum";
import { IRule, IRuleResult, Verdict } from "../../model/rule.type";

registerEnumType(Direction, {
  name: "Direction",
});

registerEnumType(PaymentMethod, {
  name: "PaymentMethod",
});

registerEnumType(Source, {
  name: "Source",
});

registerEnumType(PurposeCode, {
  name: "PurposeCode",
});

registerEnumType(EntityType, {
  name: "EntityType",
  description: "Type of entity involved in the transaction",
});

registerEnumType(TransactionType, {
  name: "TransactionType",
});

registerEnumType(EmploymentStatus, {
  name: "EmploymentStatus",
  description: "Employment status of donor",
});

registerEnumType(State, {
  name: "State",
  description: "State location of donor",
});

registerEnumType(InKindType, {
  name: "InKindType",
  description: "Type of In-kind contribution",
});

registerEnumType(Verdict, {
  name: "Verdict",
  description: "The verdict of a rule run",
});

@ObjectType()
export class Rule implements IRule {
  @Field()
  code: string;

  @Field({ nullable: true })
  state: string;

  @Field({ nullable: true })
  scope: string;

  @Field({ nullable: true })
  party: string;

  @Field({ nullable: true })
  race: string;

  @Field({ nullable: true })
  district: string;

  @Field({ nullable: true })
  county: string;

  @Field({ nullable: true })
  officeType: string;

  @Field({ nullable: true })
  ruleVersion: string;

  @Field({ nullable: true })
  entityType: string;

  @Field({ nullable: true })
  aggregateDuration: string;

  @Field((type) => [String])
  fields: string[];

  @Field({ nullable: true })
  limit?: number;
}

@ObjectType()
export class OwnerSchema {
  @Field()
  firstName: string;

  @Field()
  lastName: string;

  @Field()
  addressLine1: string;

  @Field({ nullable: true })
  addressLine2?: string;

  @Field()
  city: string;

  @Field((type) => State)
  state: string;

  @Field()
  postalCode: string;

  @Field()
  percentOwnership: string;
}

@ObjectType()
export class RuleResult implements IRuleResult {
  @Field()
  balanceAtRuleRun: number;

  @Field()
  remaining: number;

  @Field((type) => Verdict)
  verdict: Verdict;

  @Field((type) => Rule)
  rule: Rule;
}

@ObjectType()
export class ProcessorFeeData {
  @Field()
  amount: number;

  @Field((type) => PaymentMethod)
  paymentMethod: PaymentMethod;

  @Field()
  entityName: string;

  @Field()
  paymentDate: number;

  @Field()
  addressLine1: string;

  @Field({ nullable: true })
  addressLine2?: string;

  @Field()
  city: string;

  @Field((type) => State)
  state: State;

  @Field()
  postalCode: string;

  @Field()
  country: string;

  @Field({ nullable: true })
  checkNumber?: string;
}

@ObjectType()
export class Transaction implements ITransaction {
  @Field((type) => ID)
  id: string;

  @Field()
  committeeId: string;

  @Field((type) => Direction)
  direction: string;

  @Field()
  amount: number;

  @Field((type) => PaymentMethod)
  paymentMethod: string;

  @Field()
  bankVerified: boolean;

  @Field()
  ruleVerified: boolean;

  @Field()
  initiatedTimestamp: number;

  @Field((type) => Source, { nullable: true })
  source: string;

  @Field({ nullable: true })
  bankVerifiedTimestamp?: number;

  @Field({ nullable: true })
  ruleVerifiedTimestamp?: number;

  @Field((type) => PurposeCode, { nullable: true })
  purposeCode?: string;

  @Field((type) => [OwnerSchema], { nullable: true })
  owners?: OwnerSchema[];

  @Field({ nullable: true })
  refCode?: string;

  // Donor Props
  @Field({ nullable: true })
  firstName?: string;

  @Field({ nullable: true })
  middleName?: string;

  @Field({ nullable: true })
  lastName?: string;

  @Field({ nullable: true })
  addressLine1?: string;

  @Field({ nullable: true })
  addressLine2?: string;

  @Field({ nullable: true })
  city?: string;

  @Field((type) => State, { nullable: true })
  state?: string;

  @Field({ nullable: true })
  postalCode?: string;

  @Field({ nullable: true })
  employer?: string;

  @Field({ nullable: true })
  occupation?: string;

  @Field((type) => EntityType, { nullable: true })
  entityType?: string;

  @Field({ nullable: true })
  entityName?: string;

  @Field({ nullable: true })
  companyName?: string;

  @Field({ nullable: true })
  phoneNumber?: string;

  @Field({ nullable: true })
  emailAddress?: string;

  @Field({ nullable: true })
  attestsToBeingAnAdultCitizen?: boolean;

  @Field((type) => TransactionType, { nullable: true })
  transactionType?: string;

  @Field({ nullable: true })
  checkNumber?: string;

  @Field({ nullable: true })
  stripePaymentIntentId?: string;

  @Field({ nullable: true })
  cardNumberLastFourDigits?: string;

  @Field({ nullable: true })
  donorId?: string;

  @Field({ nullable: true })
  isSubcontracted?: boolean;

  @Field({ nullable: true })
  isPartialPayment?: boolean;

  @Field({ nullable: true })
  isExistingLiability?: boolean;

  @Field((type) => EmploymentStatus, { nullable: true })
  employmentStatus?: string;

  @Field({ nullable: true })
  finicityNormalizedPayeeName?: string;

  @Field({ nullable: true })
  finicityCategory?: string;

  @Field({ nullable: true })
  finicityBestRepresentation?: string;

  @Field({ nullable: true })
  finicityDescription?: string;

  @Field({ nullable: true })
  finicityPostedDate?: number;

  @Field({ nullable: true })
  finicityTransactionDate?: number;

  @Field((type) => PaymentMethod, { nullable: true })
  finicityPaymentMethod?: string;

  @Field({ nullable: true })
  paymentDate: number;

  @Field({ nullable: true })
  donorVerificationScore?: number;

  @Field({ nullable: true })
  businessIdVerificationScore?: string;

  @Field((type) => InKindType, { nullable: true })
  inKindType?: string;

  @Field({ nullable: true })
  inKindDescription?: string;

  @Field((type) => ProcessorFeeData, { nullable: true })
  processorFeeData?: ProcessorFeeData;

  @Field({ nullable: true })
  externalTransactionId?: string;

  @Field({ nullable: true })
  externalTransactionPayoutId?: string;

  @Field((type) => RuleResult, { nullable: true })
  ruleResult?: RuleResult;
}
