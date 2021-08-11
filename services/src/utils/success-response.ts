import { StatusCodes } from "http-status-codes";

export const successResponse = {
  statusCode: StatusCodes.OK,
  body: JSON.stringify({
    message: "success",
  }),
};
