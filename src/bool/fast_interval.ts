export class FastBooleanInterval {
  min: boolean;
  max: boolean;
  info: number;

  constructor (min, max, info) {
    this.min = !!min
    this.max = !!max
    this.info = info | 0
  }
}
