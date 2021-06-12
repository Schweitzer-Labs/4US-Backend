import { createParamDecorator } from "type-graphql";

export default function CurrentUser() {
  return createParamDecorator(({ context }: any) => {
    return context.currentUser;
  });
}
