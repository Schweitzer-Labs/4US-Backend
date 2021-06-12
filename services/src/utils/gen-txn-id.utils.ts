import { now } from "./time.utils";

export const randomString = (length) => {
  const chars = "123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNPQRSTUVWXYZ";
  let result = "";
  for (let i = length; i > 0; --i)
    result += chars[Math.floor(Math.random() * chars.length)];
  return result;
};

export const genTxnId = (): string => {
  return `${now()}-${randomString(6)}`;
};
