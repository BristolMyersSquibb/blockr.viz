// The bundles the page loads: one script and one stylesheet per view,
// concatenated from the sources in the order their lists give.
//
//   node tools/bundle.js [pkg-dir]
//   node tools/bundle.js --check [pkg-dir]   # exit 1 if a bundle is stale
//
// Writes inst/js/dist/{core,chart,table}.js and inst/css/dist/{core,table}.css.
// Run it after every edit under inst/js or inst/css; the R test
// tests/testthat/test-bundle.R fails while a bundle is stale.
//
// The lists: inst/js/<view>/scripts.txt for the scripts (core also takes
// the generated specs, chart the theme register first) and
// inst/css/styles.txt for the stylesheets. R's dependency functions read the
// same lists when `options(blockr.viz.dev_js = TRUE)` serves the sources.
//
// Each source goes in as it is (every one is its own IIFE), with carriage
// returns dropped, after a line naming it. A script bundle comes with a
// line-level source map (<bundle>.js.map, read only by the browser's
// devtools), so an error there names the source file and line. A browser
// honours only the last `sourceURL` comment of a script, so per-file
// sourceURLs would not do that. The first line holds an md5 of the sources
// and the second their names; the R test recomputes both. No minifier, no
// dependencies: the same sources give the same bytes.
const fs = require('node:fs');
const path = require('node:path');
const crypto = require('node:crypto');

const argv = process.argv.slice(2);
const check = argv.includes('--check');
const pkg = path.resolve(argv.filter((a) => a !== '--check')[0] || path.join(__dirname, '..'));
const inst = path.join(pkg, 'inst');

/** The non-comment lines of a list file. */
const lines = (file) => fs.readFileSync(file, 'utf8').split('\n')
  .map((l) => l.trim()).filter((l) => l && !l.startsWith('#'));

const scripts = (dir) => lines(path.join(inst, 'js', dir, 'scripts.txt'))
  .map((f) => dir + '/' + f);

/** inst/css/styles.txt: `<bundle> <file> <file> ...` per line. */
const styles = (name) => {
  const row = lines(path.join(inst, 'css', 'styles.txt'))
    .map((l) => l.split(/\s+/)).find((r) => r[0] === name);
  if (!row) throw new Error('inst/css/styles.txt has no line for ' + name);
  return row.slice(1);
};

// Paths are relative to inst/js or inst/css, as R's dependencies list them.
const BUNDLES = [
  { out: 'js/dist/core.js', dir: 'js',
    files: [...scripts('core'), 'spec/chart.js', 'spec/table.js'] },
  { out: 'js/dist/chart.js', dir: 'js',
    files: ['drilldown-theme-register.js', ...scripts('chart')] },
  { out: 'js/dist/table.js', dir: 'js', files: scripts('table') },
  { out: 'css/dist/core.css', dir: 'css', files: styles('core') },
  { out: 'css/dist/table.css', dir: 'css', files: styles('table') }
];

/** A source's text without carriage returns. */
const read = (dir, f) => fs.readFileSync(path.join(inst, dir, f), 'utf8').replace(/\r/g, '');

/** The md5 the first line carries: of every source's name line and text, in
 * order. tests/testthat/test-bundle.R computes the same. */
const hashOf = (dir, files) => {
  const h = crypto.createHash('md5');
  for (const f of files) h.update('// ' + f + '\n' + read(dir, f));
  return h.digest('hex');
};

const B64 = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/';
/** A number as a source map's base64 VLQ. */
const vlq = (n) => {
  let v = n < 0 ? (-n << 1) | 1 : n << 1;
  let out = '';
  do {
    let digit = v & 31;
    v >>>= 5;
    if (v) digit |= 32;
    out += B64[digit];
  } while (v);
  return out;
};

/** The bundle's text and, for a script, its source map: each source line
 * maps to the same line of its file, column 0. */
const build = (b) => {
  const css = b.dir === 'css';
  const comment = (s) => (css ? '/* ' + s + ' */' : '// ' + s);
  const out = [
    comment('blockr.viz bundle ' + hashOf(b.dir, b.files) +
            ', built by tools/bundle.js. Do not edit.'),
    comment('sources: ' + b.files.join(' '))
  ];
  const map = ['', ''];
  let prevSrc = 0;
  let prevLine = 0;
  b.files.forEach((f, i) => {
    out.push(comment('--- ' + f));
    map.push('');
    const text = read(b.dir, f).replace(/\n$/, '').split('\n');
    text.forEach((line, j) => {
      out.push(line);
      map.push('A' + vlq(i - prevSrc) + vlq(j - prevLine) + 'A');
      prevSrc = i;
      prevLine = j;
    });
  });
  const name = path.basename(b.out);
  if (css) return { text: out.join('\n') + '\n' };
  out.push('//# sourceMappingURL=' + name + '.map');
  const sm = { version: 3, file: name, sources: b.files.map((f) => '../' + f),
               mappings: map.join(';') };
  return { text: out.join('\n') + '\n', map: JSON.stringify(sm) + '\n' };
};

/** Writes `text` to `file` unless it holds it already (with --check, only
 * compares); says which. */
const put = (file, text) => {
  const old = fs.existsSync(file) ? fs.readFileSync(file, 'utf8') : null;
  if (old === text) return 'same ';
  if (check) return 'STALE';
  fs.writeFileSync(file, text);
  return 'wrote';
};

let stale = 0;
for (const b of BUNDLES) {
  const out = path.join(inst, b.out);
  if (!check) fs.mkdirSync(path.dirname(out), { recursive: true });
  const { text, map } = build(b);
  let said = put(out, text);
  if (map && put(out + '.map', map) === 'STALE') said = 'STALE';
  if (said === 'STALE') stale++;
  console.log(said + '  inst/' + b.out + ' (' + b.files.length + ' files, ' +
              Buffer.byteLength(text) + ' bytes)');
}
if (stale) {
  console.log(stale + ' bundle(s) stale: run `node tools/bundle.js` and commit inst/js/dist, inst/css/dist');
  process.exit(1);
}
