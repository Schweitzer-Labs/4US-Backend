import { headers } from "./headers";
import { flow } from "fp-ts/function";

const mapHeaders =
  (corsOrigin: string) =>
  (res: Promise<any>): Promise<any> => {
    return new Promise(function (resolve, reject) {
      res
        .then((result) =>
          resolve({
            ...result,
            headers: headers(corsOrigin),
          })
        )
        .catch(reject);
    });
  };

export const lambdaWithResHeaders = (corsOrigin: string) => (lambda) =>
  flow(lambda, mapHeaders(corsOrigin));
