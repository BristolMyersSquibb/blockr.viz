// Dead CSS: classes in inst/css selectors that no R or JS source emits.
//
//   node tools/dead-css.js [pkg-dir]
//
// The corpus is every class-like token in R/ and inst/js/. A token ending
// in "-" ('vz-tt-' + kind) counts as a prefix of every class starting with
// it. With no CSS or JS in R strings the corpus is complete. Exits 1 if any
// class is not emitted.
const fs = require('node:fs');
const path = require('node:path');
const postcss = require('postcss');
const sp = require('postcss-selector-parser');

const pkg = process.argv[2] || '.';
const skip = (p) => /snapdom\.js$|[\\/]vendor[\\/]/.test(p);
const list = (dir, ext) => fs.readdirSync(dir, { recursive: true })
  .map((f) => path.join(dir, f))
  .filter((p) => p.endsWith(ext) && !skip(p));

const tokens = new Set();
const prefixes = new Set();
for (const f of [...list(path.join(pkg, 'R'), '.R'), ...list(path.join(pkg, 'inst/js'), '.js')]) {
  for (const t of fs.readFileSync(f, 'utf8').match(/[A-Za-z_][\w-]*/g) || []) {
    tokens.add(t);
    if (t.endsWith('-')) prefixes.add(t);
  }
}
const emitted = (c) => tokens.has(c) || [...prefixes].some((p) => c.startsWith(p));

const dead = new Map();
let total = 0;
const seen = new Set();
for (const f of list(path.join(pkg, 'inst/css'), '.css')) {
  postcss.parse(fs.readFileSync(f, 'utf8')).walkRules((rule) => {
    sp((sel) => sel.walkClasses((c) => {
      if (!seen.has(c.value)) { seen.add(c.value); total++; }
      if (!emitted(c.value) && !dead.has(c.value)) {
        dead.set(c.value, `${path.relative(pkg, f)}:${rule.source.start.line}`);
      }
    })).processSync(rule.selector);
  });
}
console.log(`dead CSS: ${dead.size} of ${total} classes no R or JS emits`);
for (const [c, at] of dead) console.log(`  .${c.padEnd(40)} ${at}`);
process.exitCode = dead.size ? 1 : 0;
