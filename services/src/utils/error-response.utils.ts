import { HttpStatus } from "aws-sdk/clients/lambda";

export interface IErrorResponseConfig {
  statusCode: HttpStatus;
  body: object;
}

export const errorResponse = ({ statusCode, body }) => {
  return {
    statusCode: statusCode || 500,
    body: JSON.stringify(body),
  };
};
