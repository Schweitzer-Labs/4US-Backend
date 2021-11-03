import j2s from "joi-to-swagger";
import { bluelinkSchema } from "./bluelink-schema.utils";

const { swagger, components } = j2s(bluelinkSchema, {});

console.log("Swagger");
console.log(JSON.stringify(swagger));
