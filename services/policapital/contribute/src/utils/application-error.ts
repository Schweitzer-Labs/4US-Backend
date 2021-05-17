import headers from "./headers";
import {HttpStatus} from "aws-sdk/clients/lambda";

export class ApplicationError {
  constructor(public readonly message: string, public readonly statusCode: HttpStatus) {}
  toResponse() {
    return {
      statusCode: this.statusCode,
      body: JSON.stringify({
        message: this.message,
      }),
      headers,
    };
  }
}
