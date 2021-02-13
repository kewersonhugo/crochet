import * as Util from "util";

export function unreachable(x: never, message: string) {
  console.error(message, x);
  throw new Error(message);
}

export function show(x: any) {
  return Util.inspect(x, false, null, true);
}

export function pick<A>(xs: A[]): A | null {
  if (xs.length === 0) {
    return null;
  } else {
    return xs[Math.floor(Math.random() * xs.length)];
  }
}

export function delay(ms: number) {
  return new Promise<void>((resolve) => setTimeout(() => resolve(), ms));
}
