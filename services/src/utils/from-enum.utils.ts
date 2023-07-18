import * as t from "io-ts";

// @ToDo Document this function.
// https://github.com/gcanti/io-ts/issues/216#issuecomment-737219444
export function fromEnum<EnumType>(
  enumName: string,
  theEnum: Record<string, string | number>
) {
  const isEnumValue = (input: unknown): input is EnumType =>
    Object.values<unknown>(theEnum).includes(input);

  return new t.Type<EnumType>(
    enumName,
    isEnumValue,
    (input, context) =>
      isEnumValue(input) ? t.success(input) : t.failure(input, context),
    t.identity
  );
}
