export enum PaymentMethod {
  Ach = "Ach",
  Wire = "Wire",
  Check = "Check",
  Debit = "Debit",
  Credit = "Credit",
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
  PaymentMethod.Credit,
  PaymentMethod.Transfer,
  PaymentMethod.InKind,
  PaymentMethod.Cash,
  PaymentMethod.Other,
];
