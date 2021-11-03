import { HttpStatus } from "aws-sdk/clients/lambda";
import {ApplicationError} from "./application-error";

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

export const appErrorToResp = (appError: ApplicationError) => {
  return {
    statusCode: appError.statusCode || 500,
    body: JSON.stringify({
      message: appError.message
    }),
  };
};

