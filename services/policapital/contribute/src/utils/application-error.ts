import headers from "./headers";

export class ApplicationError {
  constructor(public readonly message, public readonly statusCode) {}
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
