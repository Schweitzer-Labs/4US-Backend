import { StatusCodes } from "http-status-codes";
import headers from "./headers";

export const successResponse = {
  statusCode: StatusCodes.OK,
  body: JSON.stringify({
    message: "success",
  }),
  headers,
};
