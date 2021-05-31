export enum PaymentMethod {
  ACH = "ach",
  Wire = "wire",
  Check = "check",
  Debit = "debit",
  Credit = "credit",
  Transfer = "transfer",
  InKind = "in-kind",
}

export const paymentMethods: PaymentMethod[] = [
  PaymentMethod.ACH,
  PaymentMethod.Wire,
  PaymentMethod.Check,
  PaymentMethod.Debit,
  PaymentMethod.Credit,
  PaymentMethod.Transfer,
  PaymentMethod.InKind,
];
