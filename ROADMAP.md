# CIDER Roadmap (as of June, 2026)

A high-level roadmap for CIDER. It focuses on the directions we consider
most important over the next year or so.

It's meant to give users a general idea about where the project is headed,
and collaborators a list of high-impact tasks worth tackling. It's not a
promise or a release plan - priorities shift, and good PRs have a way of
reordering everything.

## Where we are

The big architectural push of the last couple of years was decoupling
CIDER's client code from nREPL and cleaning up the foundations. That work is
largely done: the connection and session layers are now split (see
`cider-session.el` and `cider-connection.el`), a lot of internal cruft got
audited away module by module, and `compat` lets us keep supporting Emacs 28+
without littering the code with version checks. CIDER 1.22 ("São Miguel")
shipped on top of that cleaner base.

With the internals in better shape, the focus shifts back to features and UX.

## A note on socket REPLs, prepl and non-Clojure languages

Older versions of this roadmap had a prominent "support socket REPLs (plain,
unrepl, prepl) and make CIDER somewhat Clojure-agnostic" section. That itch
got scratched, just not inside CIDER.

The research into prepl and language-agnostic tooling led to two separate,
focused projects:

- [Port](https://batsov.com/articles/2026/05/12/port-a-minimalist-prepl-client-for-emacs/) - a minimalist prepl client for Emacs.
- [Neat](https://batsov.com/articles/2026/05/20/neat-a-language-agnostic-nrepl-client-for-emacs/) - a language-agnostic nREPL client for Emacs.

Those are the right home for "talk to any REPL" experiments. CIDER stays what
it's always been best at: a deep, opinionated nREPL-based IDE for Clojure and
the Clojure family of languages. Supporting arbitrary REPLs or non-Clojure
languages inside CIDER is **not** a priority going forward.

The one nuance worth keeping is the *transport*: we'd still like CIDER to speak
more than just the default nREPL transport (see HTTP/EDN transport below).

## Editor integration and modernization

### Transient-based command UI

Adopt [transient](https://github.com/magit/transient) for CIDER's command
surface, the way Magit does. Discoverable, self-documenting popups for
jack-in, the debugger, the inspector, test running, and the various prefix-arg
heavy commands would be a huge UX win over the current mix of keybindings and
menus. This also gives us a much nicer home for the growing pile of options
that today are awkward prefix-argument combinations.

### First-class `clojure-ts-mode` support

`clojure-ts-mode` (tree-sitter based) is the future of Clojure editing in
Emacs. CIDER should treat it as a first-class citizen alongside `clojure-mode`,
not an afterthought - font-locking, indentation hooks, navigation, and all the
mode-detection logic need to handle both cleanly.

### Revisit session management (sesman)

CIDER leans on [sesman](https://github.com/vspinu/sesman) for session
management. It's served us well, but it's an external dependency we don't
control, its development has stalled, and a fair amount of CIDER's
session-association complexity exists to work around its model. We should
seriously evaluate in-housing session management - what it would cost, what we'd
gain in control and simplicity, and whether a CIDER-specific implementation
could be meaningfully better for our use cases.

### Project-scoped ancillary buffers

Doc buffers, inspector buffers, error buffers and friends are currently global
singletons. Work in two projects at once and they stomp on each other - open a
doc in project A, then project B, and the first is gone. We should be able to
scope ancillary buffers to a project (or session), so each context keeps its
own. This dovetails with the session-pinning work already done for popup
buffers.

## Clojure-family languages

### Better support for Clojure-like languages

Babashka, nbb, [let-go](https://github.com/nooga/let-go) and friends speak
nREPL and are close enough to Clojure that CIDER mostly works with them -
until it doesn't, usually because some command assumes a capability the runtime
doesn't have. We want these to be genuinely well-supported, not "works if you're
lucky."

The model worth borrowing here is [Geiser's](https://www.nongnu.org/geiser/):
a documented capability/backend contract where a leaner runtime lights up the
subset of features it can support, and gracefully degrades on the rest, instead
of erroring out or pretending to be full Clojure. Pair that with clear "this
command isn't supported on this runtime" messaging and the experience gets a lot
less surprising.

## Transport

### Additional nREPL transports (nice to have)

Right now CIDER only speaks the default bencode-over-socket transport. Support
for additional transports would be a nice-to-have - the HTTP transport in
particular is appealing for environments where a raw socket isn't practical
(hosted/remote setups, browsers, restricted networks), and EDN-encoded messages
are an option there too. Not a priority, but a natural extension now that the
connection layer is cleanly separated, so worth keeping on the radar.

## Interactive debugging and introspection

This is where the venerable Common Lisp tooling (SLIME/SLY) still has ideas
worth stealing.

### Non-intrusive value recording ("stickers")

SLY's [stickers](https://joaotavora.github.io/sly/) let you annotate
expressions in your source, then *replay* every value that flowed through them
as the code ran - print-debugging without touching your code's logic, and
without the output getting lost in a stream. A Clojure/nREPL take on this would
be a genuinely novel and powerful debugging surface.

### Richer live debugger

CIDER's debugger is good, but the CL debuggers go further: evaluating
expressions in the lexical context of any stack frame (against that frame's
locals), forcing a frame to return a value, restarting a frame after
redefining a function. Worth seeing how much of that we can bring to the
nREPL-based debugger.

### Structured, interactive tracing

Replace the text-dump output of `cider-trace` with a structured trace dialog
(SLY-style): an interactive call tree where every argument and return value is
a clickable, inspectable object you can send back to the REPL.

### Stepwise macroexpansion

In-place, stepwise macro expansion (à la
[macrostep](https://github.com/joddie/macrostep)) shipped in CIDER 2.0:
`cider-macrostep-expand` expands the form at point one level inline, you can
step into nested forms and collapse back (`cider-macrostep-mode`), the
further-expandable heads are highlighted with `n`/`p` navigation, and the
gensyms a macro introduces are colorized.  It builds on the
`cider/classify-symbols` cider-nrepl op.  See
[#1850](https://github.com/clojure-emacs/cider/issues/1850).

Potential follow-ups:

- Expanding inline functions / compiler macros, not just macros - the classify
  op already tags them, but `macroexpand-1` can't expand an inline, so it needs
  new middleware support (applying the `:inline` expander).
- Recognizing namespace-qualified macros in the ClojureScript classifier (only
  the common simple/referred/core cases are handled today).
- Bringing the expandable-head highlighting (and perhaps the gensym coloring)
  to the legacy separate-buffer macroexpansion commands, or eventually retiring
  those in favor of the inline flow.

## REPL and evaluation UX

### Inspector improvements

The inspector is one of CIDER's best features and has room to grow: evaluating
expressions with the inspected object bound (so you can drill in with arbitrary
code), an opt-in "auto-inspect every eval result" mode feeding a persistent,
lazily-expanded tree view (Calva does this nicely, see
[#3636](https://github.com/clojure-emacs/cider/issues/3636)), and better lazy
expansion so huge or infinite structures never lock things up.

### Multiple named REPLs per connection

nREPL already supports multiple sessions over one connection. We could expose
that the way SLY's mREPL does: spin up additional, independently-named REPLs on
the same connection, each free to sit in its own namespace, so you can run side
experiments without disturbing your main session.

### Better handling of huge / high-volume output

Chatty stdout and enormous result structures can still bog down the REPL
buffer. A dedicated, high-performance output sink and more flexible output
routing (results vs. stdout/stderr vs. system messages) would help people
working with noisy programs or big data. Related long-standing pain points:
[#2635](https://github.com/clojure-emacs/cider/issues/2635),
[#3232](https://github.com/clojure-emacs/cider/issues/3232).

### Rich-comment / notebook-friendly workflow

The `(comment ...)` rich-comment workflow is central to how a lot of Clojurists
work. We can support it better: more flexible eval-to-comment (choose between a
line comment, an `#_` form, or a `(comment ...)` block), and exploring a
notebook-style surface where rich-comment blocks act as evaluable cells.

### Persistent REPL history improvements

Smarter, more durable REPL history - better persistence, dedup, and recall.
See [#2363](https://github.com/clojure-emacs/cider/issues/2363).

## Onboarding and discoverability

### Guided "getting started" experience

`cider-jack-in` already works outside a project (it falls back to an
auto-detected tool, asking for confirmation by default), and
`cider-jack-in-universal` offers a "no project here, pick a type" prompt. What's
missing is the *guided* part. Calva's "fire up the getting started REPL" spins
up a REPL plus a few tutorial buffers with only Java installed - no project, no
build tool, no decisions. A similar guided, zero-config first-run experience
(scaffolding a throwaway session and walking a newcomer through evaluation,
paredit, etc.) would lower the barrier a lot.

### ClojureDocs examples in the doc buffer

Pull crowd-sourced examples and "see also" links from ClojureDocs directly into
`cider-doc`'s buffer, with one-key actions to drop a runnable example into a
rich comment or the REPL. See
[#3744](https://github.com/clojure-emacs/cider/issues/3744).

### Expanded cross-referencing

CIDER has find-references; SLIME's xref suite goes further with who-binds,
who-sets (var writes), who-references (var reads) and macro use-sites. Some of
these map well onto what the analysis tooling can already tell us.

## Refactoring and clojure-lsp cooperation

### Bring more clj-refactor functionality into CIDER

A long-standing goal (still listed in our FAQ): move the most useful bits of our
sibling project [clj-refactor.el](https://github.com/clojure-emacs/clj-refactor.el)
into CIDER proper - specifically the features that *don't* require building a
full project AST. Two good candidates:

- **Dependency hotloading.** It's been broken in clj-refactor for a while;
  reimplementing it in CIDER would be a clear win.
- **`ns` cleanup.** Useful and self-contained (and potentially doable by shelling
  out to an external tool).

The AST-heavy refactorings can stay in clj-refactor; we only want the things that
fit CIDER's "lean on the running REPL" model.

### Better cooperation with clojure-lsp

A lot of Clojurists run CIDER and clojure-lsp side by side, and the two overlap
in places (xref, ElDoc, completion, find-usages). Today the cooperation is
mostly "configure who wins" (xref backend precedence, handing ElDoc to one or the
other). We can do better: clearer guidance and defaults for the combined setup,
smarter division of labor (let LSP do static project-wide analysis, let CIDER do
the runtime-aware things it's uniquely good at), and avoiding redundant or
conflicting behavior when both are active.

### ClojureScript parity

Reaching feature parity between Clojure and ClojureScript is an evergreen goal -
a fair amount of functionality is still Clojure-only, and running a Clojure-only
command against a cljs REPL tends to fail in confusing ways. Closing those gaps
(and at minimum giving clear "not supported under ClojureScript" messaging, see
[#2198](https://github.com/clojure-emacs/cider/issues/2198)) stays on the list.

## Internal improvements

### Keep paying down the foundations

The module-by-module audit and the nREPL/CIDER decoupling paid off; we should
keep that momentum - finishing the decoupling odds and ends, tightening up
callback handling
([#1099](https://github.com/clojure-emacs/cider/issues/1099)), and continuing
to prefer built-ins and shed dead code as we touch each module.

### Graceful handling of unloaded namespaces

A lot of CIDER commands silently do nothing (or behave confusingly) when the
current namespace hasn't been evaluated/loaded into the REPL, or when a
referenced var is unresolved - macroexpansion is one example, but the pattern
is broad. We should detect these cases centrally and give an actionable hint
(e.g. "evaluate the buffer first") instead of a silent no-op, ideally via a
shared helper rather than ad-hoc checks scattered across commands.

The shared helpers now exist in `cider-client.el` - `cider-ns-loaded-p`,
`cider-resolution-failure-message` and `cider-ensure-macro` distinguish an
unloaded namespace from a genuine typo/missing require, and the macroexpansion,
`cider-find-var`, `cider-doc`, ClojureDocs, xref and `cider-browse-ns` commands
route through them. The remaining work is to keep extending that coverage to the
other commands that still fail quietly (e.g. inspector and completion paths).
