import * as Fs from "fs";
import { parse } from "./compiler/syntax/parser";
import { show } from "./utils/utils";
import { CrochetInteger, CrochetText, nothing } from "./vm-js/intrinsics";
import { ForeignInterface } from "./vm-js/primitives";
import { CrochetVM } from "./vm-js/vm";

const [file, trace] = process.argv.slice(2);
const source = Fs.readFileSync(
  file || __dirname + "../examples/hello.crochet",
  "utf-8"
);
const ast = parse(file, source);
console.log(`${file}\n${"-".repeat(file.length)}`);
console.log(show(ast));

const ir = ast.compile();
console.log("-".repeat(72));
console.log(show(ir));

const ffi = new ForeignInterface();
const vm = new CrochetVM(ffi);

ffi.add("Concat", 2, (vm: CrochetVM, activation, x, y) => {
  vm.assert_text(activation, x);
  vm.assert_text(activation, y);
  activation.push(new CrochetText(x.value + y.value));
  return activation;
});

ffi.add("Show", 1, (vm: CrochetVM, activation, value) => {
  console.log(show(value.to_js()));
  activation.push(nothing);
  return activation;
});

ffi.add("ShowDb", 0, (vm: CrochetVM, activation) => {
  console.log(show(vm.database));
  activation.push(nothing);
  return activation;
});

ffi.add("ToText", 1, (vm: CrochetVM, activation, value) => {
  if (value instanceof CrochetInteger) {
    activation.push(new CrochetText(value.value.toString()));
    return activation;
  } else {
    throw new Error(`Invalid type: ${value.type}`);
  }
});

ffi.add("Say", 1, (vm: CrochetVM, activation, phrase) => {
  vm.assert_text(activation, phrase);
  console.log(">>", phrase.value);
  activation.push(nothing);
  return activation;
});

ffi.add("Wait", 1, (vm: CrochetVM, activation, time) => {
  vm.assert_integer(activation, time);
  console.log(`[Wait ${time.value}]`);
  activation.push(nothing);
  return activation;
});

ffi.add("ShowFront", 1, (vm: CrochetVM, activation, image) => {
  vm.assert_text(activation, image);
  console.log(`[Show ${image.value}]`);
  activation.push(nothing);
  return activation;
});

console.log("-".repeat(72));
vm.trace(trace === "--trace");
vm.load_module(ir);
vm.run().catch((error) => console.error(error.stack));
