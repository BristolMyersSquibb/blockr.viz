// Run heatmap-block.js's row assembler against a model written by R, so the
// two renderers over hmb_cell_model() can be compared byte for byte. The two
// pure functions are sliced out of the real source: no DOM, no bundler, and
// no second copy of the code to drift.
//
// argv: <path to heatmap-block.js> <path to model JSON> <path to write rows>
const fs = require('fs');
const [srcPath, modelPath, outPath] = process.argv.slice(2);
const src = fs.readFileSync(srcPath, 'utf8');
const start = src.indexOf('  /** @param {string} x */\n  function esc(');
const endMark = "    return out.join('');\n  }";
const end = src.indexOf(endMark) + endMark.length;
if (start < 0 || end < endMark.length) {
  console.error('could not slice esc/assembleRows out of ' + srcPath);
  process.exit(1);
}
const mod = new Function(
  src.slice(start, end) + '\nreturn { assembleRows: assembleRows };'
)();
fs.writeFileSync(outPath,
  mod.assembleRows(JSON.parse(fs.readFileSync(modelPath, 'utf8'))));
