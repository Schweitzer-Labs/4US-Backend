import j2s from "joi-to-swagger";
import { externalDataSchema } from "./external-data-schema.utils";

const { swagger, components } = j2s(externalDataSchema, {});

console.log("Swagger");
console.log(JSON.stringify(swagger));
