export enum PaymentMethod {
  Ach = "Ach",
  Wire = "Wire",
  Check = "Check",
  Debit = "Debit",
  Credit = "Credit",
  OnlineProcessor = "OnlineProcessor",
  Transfer = "Transfer",
  InKind = "InKind",
  Cash = "Cash",
  Other = "Other",
}

export const paymentMethods: PaymentMethod[] = [
  PaymentMethod.Ach,
  PaymentMethod.Wire,
  PaymentMethod.Check,
  PaymentMethod.Debit,
  PaymentMethod.OnlineProcessor,
  PaymentMethod.Credit,
  PaymentMethod.Transfer,
  PaymentMethod.InKind,
  PaymentMethod.Cash,
  PaymentMethod.Other,
];

export enum InKindDescription {
  ServicesFacilitiesProvided = "ServicesFacilitiesProvided",
  CampaignExpensesPaid = "CampaignExpensesPaid",
  PropertyGiven = "PropertyGiven",
}
