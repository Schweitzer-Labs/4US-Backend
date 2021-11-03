import Joi from "joi";

export const stringOpt = (min = 1, max = 200) => Joi.string().min(min).max(max);
export const stringReq = (min = 1, max = 200) =>
    Joi.string().min(min).max(max).required();