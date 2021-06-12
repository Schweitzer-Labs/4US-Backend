export const now = () => new Date().getTime();
export const milliToEpoch = (time: number) => Math.round(time / 1000);
export const epochToMilli = (time: number) => time * 1000;
