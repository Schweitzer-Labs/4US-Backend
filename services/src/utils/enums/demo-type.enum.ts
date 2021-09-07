export enum DemoType {
  Clean = "Clean",
  Reconciled = "Reconciled",
}

export const isClean = (demoType: DemoType): boolean =>
  demoType === DemoType.Clean;

export const isReconciled = (demoType: DemoType): boolean =>
  demoType === DemoType.Reconciled;
