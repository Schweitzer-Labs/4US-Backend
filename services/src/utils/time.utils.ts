export const now = () => new Date().getTime();
export const milliToEpoch = (time: number) => Math.floor(time / 1000);
export const epochToMilli = (time: number) => time * 1000;
export const midnightLastNight = () => new Date().setUTCHours(0, 0, 0, 0);
export const millisToYear = (date: number): number =>
  new Date(date).getFullYear();
const yearToYearMillis = (year: number): number =>
  new Date(year, 0, 1).getTime();

export const millisToYearStart = (date: number): number =>
  yearToYearMillis(millisToYear(date));

export const toISODate = (time: number): string =>
  new Date(time).toISOString().split("T")[0];

export const nMonthsAgo =
  (monthsAgo: number) =>
  (time: number): number => {
    const date = new Date(time);
    date.setMonth(date.getMonth() - monthsAgo);
    return date.getTime();
  };
