export const stringToDDBString = (prop: string, val: string) => ({
  [prop]: {
    S: val,
  },
});

export const numberToDDBNumber = (prop: string, val: number) => ({
  [prop]: {
    N: val + "",
  },
});

export const boolToDDBBool = (prop: string, val: boolean) => ({
  [prop]: {
    BOOL: val,
  },
});
