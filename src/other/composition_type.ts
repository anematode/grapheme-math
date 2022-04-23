import { Color } from "./color";

// Discarded at compilation time, but used to help ensure that composition types adhere to this specification
export interface CompositionType<Self, PartialSpecification> {
  compose: (...args: PartialSpecification[]) => Self;
  create: (params: PartialSpecification) => Self;
  default: () => Self
  fromObj: (o: any, throwOnErr: boolean) => Self | null
}
