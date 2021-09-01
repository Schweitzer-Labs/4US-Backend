import { now } from "./time.utils";
const hashLength = 6;

export const randomString = (length) => {
  const chars = "123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNPQRSTUVWXYZ";
  let result = "";
  for (let i = length; i > 0; --i)
    result += chars[Math.floor(Math.random() * chars.length)];
  return result;
};

export const dateToTxnId = (date: number): string => {
  return `${date}-${randomString(hashLength)}`;
};

export const genTxnId = (): string => {
  return `${now()}-${randomString(hashLength)}`;
};
