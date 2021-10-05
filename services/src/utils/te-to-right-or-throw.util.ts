import { TaskEither } from "fp-ts/TaskEither";
import { isLeft } from "fp-ts/Either";

export const teToRightOrThrow = async <a, b>(
  t: TaskEither<a, b>
): Promise<b> => {
  const res = await t();
  if (isLeft(res)) {
    throw res.left;
  } else {
    return res.right;
  }
};
