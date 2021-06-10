import { StatusCodes } from "http-status-codes";
import headers from "./headers";

export interface SuccessResponse {
  statusCode: StatusCodes;
  body: any;
  headers: any;
}

export const successResponse = {
  statusCode: StatusCodes.OK,
  body: JSON.stringify({
    message: "success",
  }),
  headers,
};
