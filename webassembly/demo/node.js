const fs = require('fs').promises;
const util = require('util');

async function run() {
  async function createWebAssembly(bytes) {
    const memory = new WebAssembly.Memory({ initial: 256, maximum: 256 });
    const env = {
      abortStackOverflow: (err) => { throw new Error(`overflow: ${err}`); },
      table: new WebAssembly.Table({ initial: 0, maximum: 0, element: 'anyfunc' }),
      __table_base: 0,
      memory,
      __memory_base: 1024,
      STACKTOP: 0,
      STACK_MAX: memory.buffer.byteLength,
    };
    return WebAssembly.instantiate(bytes, { env });
  }

  const result = await createWebAssembly(new Uint8Array(await fs.readFile('./example.wasm')));
  console.log(util.inspect(result, true, 0));
  console.log(result.instance.exports._add(9, 9));
}
run();

