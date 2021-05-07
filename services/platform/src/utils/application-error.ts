import headers from "./headers";

export class ApplicationError {
  constructor(
    public readonly message: string,
    public readonly statusCode?: string
  ) {}
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
