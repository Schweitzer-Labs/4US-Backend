export const now = () => new Date().getTime();
export const milliToEpoch = (time: number) => Math.floor(time / 1000);
export const epochToMilli = (time: number) => time * 1000;
export const midnightLastNight = () => new Date().setUTCHours(0, 0, 0, 0);
