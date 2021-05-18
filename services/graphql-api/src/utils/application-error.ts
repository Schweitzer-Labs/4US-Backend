import headers from "./headers";
import { StatusCodes } from "http-status-codes";

export class ApplicationError extends Error {
  constructor(
    public readonly message: string,
    public readonly data: any,
    public readonly statusCode?: StatusCodes
  ) {
    super();
    console.error(message, data);
  }
  toResponse() {
    return {
      statusCode: this.statusCode || 500,
      body: JSON.stringify({
        message: this.message,
      }),
      headers,
    };
  }
}
