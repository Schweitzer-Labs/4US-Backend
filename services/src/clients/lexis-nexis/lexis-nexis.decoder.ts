import * as t from "io-ts";

export const InstantIdResponse = t.type({
  InstantIDResponseEx: t.type({
    response: t.type({
      Result: t.type({
        UniqueId: t.string,
        ComprehensiveVerification: t.type({
          ComprehensiveVerificationIndex: t.number,
        }),
      }),
    }),
  }),
});

export type IInstantIdResponse = t.TypeOf<typeof InstantIdResponse>;
