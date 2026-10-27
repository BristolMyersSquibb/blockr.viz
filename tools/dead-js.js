// Dead JS: functions defined in inst/js that nothing references.
//
//   node tools/dead-js.js [pkg-dir]
//
// Definitions: function declarations, `const f = () => ...`, function-valued
// object properties and class methods, `X.name = function ...`. References:
// every identifier and non-computed member name in any file, every word in
// a string literal, and every word in R/ (R writes names into HTML). Names
// that Shiny or the browser call are allowed. Name-based across files, so it
// is a floor; eslint's no-unused-vars covers the locals of a file exactly.
// Exits 1 if anything is unreferenced.
const fs = require('node:fs');
const path = require('node:path');
const acorn = require('acorn');
const walk = require('acorn-walk');

const pkg = process.argv[2] || '.';
const skip = (p) => /snapdom\.js$|[\\/]spec[\\/]|[\\/]vendor[\\/]/.test(p);
const list = (dir, ext) => fs.readdirSync(dir, { recursive: true })
  .map((f) => path.join(dir, f))
  .filter((p) => p.endsWith(ext) && !skip(p));

const hooks = new Set([
  // Shiny input and output bindings
  'find', 'getId', 'getValue', 'setValue', 'subscribe', 'unsubscribe',
  'initialize', 'receiveMessage', 'getState', 'getRatePolicy', 'renderValue',
  'renderError', 'clearError', 'resize',
  // the language and the browser
  'constructor', 'toJSON', 'handleEvent', 'onload', 'onerror'
]);

const defs = [];
const refs = new Set();
const isFn = (n) => n && /Function/.test(n.type);
const words = (s) => s.match(/[A-Za-z_$][\w$]*/g) || [];

for (const f of list(path.join(pkg, 'inst/js'), '.js')) {
  const ast = acorn.parse(fs.readFileSync(f, 'utf8'),
    { ecmaVersion: 2022, sourceType: 'script', locations: true });
  const named = new Set();
  const def = (name, node, key) => {
    defs.push({ name, at: `${path.relative(pkg, f)}:${node.loc.start.line}` });
    named.add(key);
  };
  walk.full(ast, (n) => {
    if (n.type === 'FunctionDeclaration' && n.id) def(n.id.name, n, n.id);
    if (n.type === 'VariableDeclarator' && n.id.type === 'Identifier' && isFn(n.init)) {
      def(n.id.name, n, n.id);
    }
    if ((n.type === 'Property' || n.type === 'MethodDefinition') && !n.computed &&
        n.key && isFn(n.value)) {
      def(n.key.name || n.key.value, n, n.key);
    }
    if (n.type === 'AssignmentExpression' && n.left.type === 'MemberExpression' &&
        !n.left.computed && isFn(n.right)) {
      def(n.left.property.name, n, n.left.property);
    }
  });
  walk.full(ast, (n) => {
    if (n.type === 'Identifier' && !named.has(n)) refs.add(n.name);
    if (n.type === 'MemberExpression' && !n.computed && !named.has(n.property)) {
      refs.add(n.property.name);
    }
    if (n.type === 'Property' && n.shorthand) refs.add(n.key.name);
    if (n.type === 'Literal' && typeof n.value === 'string') words(n.value).forEach((w) => refs.add(w));
    if (n.type === 'TemplateElement') words(n.value.cooked || '').forEach((w) => refs.add(w));
  });
}
for (const f of list(path.join(pkg, 'R'), '.R')) {
  words(fs.readFileSync(f, 'utf8')).forEach((w) => refs.add(w));
}

const dead = defs.filter((d) => !hooks.has(d.name) && !refs.has(d.name));
console.log(`dead JS: ${dead.length} of ${defs.length} functions unreferenced`);
for (const d of dead) console.log(`  ${d.name.padEnd(36)} ${d.at}`);
process.exitCode = dead.length ? 1 : 0;
