import fetch from "node-fetch";
import { isLeft } from "fp-ts/Either";
import * as D from "io-ts";
import { ApplicationError } from "../utils/application-error";
import {
  FinicityTransaction,
  GetFinicityTransactionsResponse,
} from "./finicity.decoders";

export class Finicity {
  constructor(
    private readonly partnerId: string,
    private readonly partnerSecret: string,
    private readonly appKey: string,
    private readonly onboardingUrl: string
  ) {}
  async getFinicityConnectUrl() {
    const finicityConnectGenUrl =
      "https://api.finicity.com/connect/v2/generate";
    const token = await this.getFinicityToken();

    const body = {
      partnerId: this.partnerId,
      customerId: "5003801680",
      redirectUri: this.onboardingUrl,
    };

    const linkRes = await fetch(finicityConnectGenUrl, {
      method: "POST",
      headers: {
        "Finicity-App-Key": this.appKey,
        "Finicity-App-Token": token,
        "Content-Type": "application/json",
        Accept: "application/json",
      },
      body: JSON.stringify(body),
    });

    const { link } = await linkRes.json();

    return link;
  }

  private async getFinicityToken() {
    const finicityAuthUrl =
      "https://api.finicity.com/aggregation/v2/partners/authentication";

    const body = {
      partnerId: this.partnerId,
      partnerSecret: this.partnerSecret,
    };
    const tokenRes = await fetch(finicityAuthUrl, {
      method: "POST",
      headers: {
        "Finicity-App-Key": this.appKey,
        "Content-Type": "application/json",
        Accept: "application/json",
      },
      body: JSON.stringify(body),
    });
    const { token } = await tokenRes.json();
    return token;
  }

  async getTransactions(
    customerId: string,
    accountId: string,
    epochFrom: number,
    epochTo: number
  ): Promise<FinicityTransaction[]> {
    const qs = `?fromDate=${epochFrom}&toDate=${epochTo}`;
    const apiUrl = `https://api.finicity.com/aggregation/v3/customers/${customerId}/accounts/${accountId}/transactions${qs}`;

    console.log(apiUrl);
    const token = await this.getFinicityToken();

    const linkRes = await fetch(apiUrl, {
      method: "GET",
      headers: {
        "Finicity-App-Key": this.appKey,
        "Finicity-App-Token": token,
        "Content-Type": "application/json",
        Accept: "application/json",
      },
    });

    const webRes = await linkRes.json();
    const parsedRes = GetFinicityTransactionsResponse.decode(webRes);

    if (isLeft(parsedRes)) {
      console.log(parsedRes.left[0]);
      throw new ApplicationError("Unexpected finicity transaction response.");
    }
    console.log(parsedRes.right);
    return parsedRes.right.transactions;
  }
}
