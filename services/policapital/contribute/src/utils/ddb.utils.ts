export const stringToDDBString = (prop: string, val?: string) =>
  !val
    ? {}
    : {
        [prop]: {
          S: val + "",
        },
      };

export const numberToDDBNumber = (prop: string, val?: number) =>
  !val
    ? {}
    : {
        [prop]: {
          N: val + "",
        },
      };

export const boolToDDBBool = (prop: string, val: boolean) => ({
  [prop]: {
    BOOL: val,
  },
});
