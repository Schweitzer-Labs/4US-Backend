export const logPrefix = "Lexis-nexis";

export const instantIdEndpoint =
  "https://wsonline.seisint.com/WsIdentity/InstantID?&ver_=2.80&_product_code=false&json_test_";

export const businessIdEndpoint =
  "https://wsonline.seisint.com/WsIdentity/BusinessInstantID2?ver_2.60&_product_code=false&ver_=2.8&json_test_";

export const dueDiligenceAttributesEndpoint =
  "https://wsonline.seisint.com/WsIdentity/DueDiligenceAttributes?_product_code=false&ver_=2.8&json_test_";

export interface ILexisNexisConfig {
  username: string;
  password: string;
}
