# prepl support

A planning doc for adding prepl as a second connection backend in CIDER.

This is a design sketch, not a contract. Open questions are flagged
inline.

## Goals

- Connect to a Clojure prepl (`clojure.core.server/io-prepl`) over TCP.
- Eval Clojure code, see `:out`/`:err`/`:tap`/`:ret`/`:exception`
  responses in the REPL, route them to the standard editor surfaces
  (REPL output, error buffer, etc).
- Provide eval-form fallbacks for the cider-nrepl ops that don't have
  middleware available -- doc, source, apropos, completion, ns
  browsing, basic test running. Same recipe CIDER used pre-cider-nrepl.
- Connect via `cider-connect` and (where the user has set it up)
  `cider-jack-in`.

## Non-goals

- **Interrupt.** prepl has no out-of-band interrupt mechanism.  Users
  who need it stay on nREPL.
- **Bundled clj+cljs sessions.** See "ClojureScript" below.
- **Structured stacktrace, inspector, debugger, profiler.** These are
  nREPL-middleware-shaped features that don't translate cleanly to
  pure eval. nREPL remains the recommended backend for users who want
  the full CIDER feature set.
- **Socket REPL** (the unstructured Clojure socket REPL). Strictly
  less than prepl on every axis; if someone has a use case we revisit.

## Architecture

### Where the existing code already helps

After #3892 (response-side decoupling for #1099), the response
boundary is reasonably backend-agnostic. `nrepl-make-eval-handler` is
a slot-based dispatcher (`:on-value` / `:on-stdout` / `:on-stderr` /
`:on-done` / `:on-status` / ...). prepl's response shape -- one EDN
form per response, tagged `:out` / `:err` / `:ret` / `:tap` /
`:exception` -- maps cleanly onto these slots:

| prepl tag    | eval-handler slot |
|--------------|-------------------|
| `:out`       | `:on-stdout`      |
| `:err`       | `:on-stderr`      |
| `:ret`       | `:on-value` + synthesized `:on-done` |
| `:tap`       | (new slot, or routed through stdout) |
| `:exception` | `:on-eval-error` + synthesized `:on-done` |

So a prepl process filter that incrementally reads EDN forms and
fans them out to the existing handler slots is most of the integration.

### Where we still have work to do

The **request side** is fully nREPL-shaped today. Every feature in
CIDER calls `cider-nrepl-send-request` (or its sync variant) with an
op name + params. Adding prepl means:

1. Introducing a generic dispatch surface in CIDER (so call sites
   stop hard-coding the nREPL request function).
2. Implementing that surface for prepl.
3. Providing eval-form fallbacks for ops that don't exist in prepl.

These map to the four steps below.

## Plan

### Step 1: request-side dispatch surface (refactor)

Symmetric to the response-side work for #1099. Introduce two
generics:

```elisp
(cl-defgeneric cider-send-eval (conn code handler &key ns line column)
  "Send CODE for evaluation; HANDLER is built with `cider-make-eval-handler'.")

(cl-defgeneric cider-send-op (conn op params handler)
  "Send a non-eval op to CONN.  Throws `cider-conn-op-unsupported' if
the connection type cannot satisfy the op without middleware.")
```

plus thinner ones:

```elisp
(cl-defgeneric cider-send-eval-sync (conn code &key ns))
(cl-defgeneric cider-supports-op-p (conn op))
(cl-defgeneric cider-backend-interrupt (conn))
(cl-defgeneric cider-backend-close (conn))
```

### Naming notes

- `cider-send-*` for anything that puts something on the wire.
  Mirrors `nrepl-send-request`; the verb `send` distinguishes these
  connection primitives from the densely-populated `cider-eval-*`
  namespace of interactive editor commands (`cider-eval-region`,
  `cider-eval-defun-at-point`, etc.).
- `cider-supports-op-p` for the predicate. No backend prefix because
  every CIDER function takes a connection somewhere; flagging it adds
  noise.
- `cider-backend-interrupt` and `cider-backend-close` keep a prefix
  to avoid colliding with the user-facing interactive commands
  `cider-interrupt` and `cider-close-buffer`. Lifecycle methods carry
  the namespace tag; wire methods don't.

### File layout

- `lisp/cider-backend.el` -- the `cl-defgeneric`s, the
  `cider-backend-type` buffer-local, and the
  `cider-backend-op-unsupported` error symbol.  This is the boundary;
  it knows nothing about specific protocols.
- `lisp/cider-connection.el` -- existing connection-management
  surface (cider-quit/restart/list, cider-repl-create) plus the
  nREPL `cl-defmethod` block at the tail.  The nREPL methods are thin
  wrappers around the existing `nrepl-client.el` functions, so they
  belong next to the rest of CIDER's nREPL-side glue rather than in
  their own file.
- `lisp/cider-prepl.el` -- prepl protocol decoding plus the prepl
  `cl-defmethod` block.  prepl is a self-contained protocol
  implementation, not just glue, so it gets its own file.

There is intentionally no `cider-conn.el` (would be confusable with
`cider-connection.el`) and no `cider-backend-nrepl.el` (would just be
glue and is tiny enough to live with the rest of `cider-connection.el`).

The nREPL implementation is a thin wrapper over the existing
`cider-nrepl-send-request` / `cider-nrepl-send-sync-request`. Existing
callers migrate from `cider-nrepl-send-request` to `cider-send-eval`
or `cider-send-op` depending on what they actually want. Old
`cider-nrepl-send-request` is kept as an obsolete shim.

This step is **independently valuable** and should ship to master
incrementally regardless of whether prepl ever lands. It clarifies the
public API even for nREPL-only users.

Estimated: 3-4 medium PRs. Pure refactor, behavior-preserving.

### Step 2: prepl backend

New file `lisp/cider-prepl.el` (or similar). Components:

1. **Connection setup.** A prepl process is started server-side via
   the `clojure.core.server/start-server` form (`{:accept
   clojure.core.server/io-prepl ...}`). On the CIDER side, connect to
   the host/port and send a Clojure form that arms io-prepl.

2. **Process filter.** Reads bytes from the socket, decodes
   incremental EDN forms (parseedn or a hand-rolled reader on the
   prepl form boundary), emits responses to a per-connection callback.

3. **Eval send + correlation.** prepl has no request ids. Correlation
   is by ordering: when we send eval N, the next `:ret` (or
   `:exception`) is the response for it. Maintain a FIFO queue of
   pending eval handlers; `:out` / `:err` / `:tap` go to the head
   handler, `:ret` / `:exception` close it and dequeue.

4. **Generic methods** — `cider-conn-send-eval`, sync variant,
   `cider-conn-close`. `cider-conn-send-op` raises
   `cider-conn-op-unsupported`. `cider-conn-interrupt` is a user-
   facing error explaining the limitation.

5. **Wiring.** `cider-connect-prepl` interactive command. Optional
   `cider-jack-in` extension if there's a clean way to spawn a prepl
   server (likely a deps.edn alias that runs
   `clojure.core.server/start-server`).

Estimated: ~500 LOC of new code, 1-2 PRs.

### Step 3: eval-form fallbacks for the ops we want on prepl

For each op-shaped feature we want to keep working on prepl, write a
small Clojure form that produces the same data. Pattern is the legacy
`cider-info-form` / `cider-eldoc-info-form` etc. that we removed in
the cider-nrepl-0.59 cycle -- working code, just deemed unnecessary
once we made the middleware mandatory.

Priority list:

| Feature             | Op                    | Fallback form                                  |
|---------------------|-----------------------|-----------------------------------------------|
| Doc lookup          | `cider/info`          | `(clojure.repl/doc 'foo)` + parse             |
| Source lookup       | `cider/info`          | `(clojure.repl/source 'foo)`                  |
| Apropos             | `cider/apropos`       | `(clojure.repl/apropos #"...")`               |
| Namespace browse    | `cider/ns-vars`       | `(ns-publics 'foo)` / `(ns-interns 'foo)`     |
| Namespace list      | `cider/ns-list`       | `(map ns-name (all-ns))`                      |
| Completion          | `cider/complete`      | reflection over `(ns-map *ns*)` + clojure.core |
| Test running        | `cider/test-var-query`| `(clojure.test/run-tests 'foo)` + parse       |
| Macroexpand         | `cider/macroexpand`   | `(macroexpand-1 '(...))`                      |
| Format              | `cider/format-code`   | (skip -- requires cljfmt at minimum)          |

Each becomes a small focused PR. The hard ones (structured stacktrace,
inspector, debugger) get an explicit "this requires nREPL" message and
stay nREPL-only.

### Step 4: Socket REPL (deferred)

Skip unless someone explicitly asks. Socket REPL has no structured
output (stdout / stderr / result are all interleaved on the same
stream); supporting it well requires heuristic prompt parsing that's
fragile. prepl is strictly more capable for the same use case.

## Built-in nREPL op reproduction

For reference -- mapping the eight built-in nREPL ops (the ones that
work without cider-nrepl middleware) onto a prepl backend:

| nREPL op       | prepl handling                                                   |
|----------------|------------------------------------------------------------------|
| `eval`         | Native (it's literally what prepl does).                         |
| `load-file`    | Eval `(load-file "...")`.                                        |
| `clone`        | No-op. prepl has no sessions; we synthesize a stub session id    |
|                | so the rest of CIDER's session-aware code keeps working.         |
| `close`        | Close the TCP socket.                                            |
| `describe`     | Synthesize a capability dict from "what we know prepl supports". |
| `interrupt`    | **Not supported.** User-facing error. (Acceptable per non-goals.)|
| `stdin`        | If the running form is reading from `*in*`, write the user's     |
|                | input directly to the socket. prepl reads from the same socket.  |
| `ls-sessions`  | Return the synthesized stub session.                             |

So all eight are reproducible except interrupt -- matching the user's
expectation.

## ClojureScript

This is the trickiest part. **Recommendation: support clj only at
launch; cljs is a separate, harder problem.**

### The problem

ClojureScript doesn't have a wire protocol of its own. The cljs REPL
is `cljs.repl/repl` -- a Clojure function that takes over stdin/stdout
inside a host Clojure REPL and starts a read-eval-print loop that
*evaluates ClojureScript*. There is no "ClojureScript socket REPL" or
"ClojureScript prepl" that ships with the language.

The way nREPL handles this is **Piggieback** -- middleware that
intercepts eval requests on a session, marks the session as
cljs-active, and routes evals through the embedded cljs REPL. The
client sees the same nREPL session/op surface; the cljs-ness is hidden
in the middleware. This works precisely because nREPL has sessions,
ops, and middleware.

prepl has none of those things. Three ways to think about cljs over
prepl:

### Option 1: separate prepl for cljs

shadow-cljs / figwheel-main expose nREPL out of the box. They don't
expose prepl. Without an upstream prepl-flavored cljs REPL, this
option requires *us or someone* to build one -- e.g. wire `cljs.repl`
into `clojure.core.server/io-prepl` somehow. Possible but a bigger
project than CIDER alone should take on.

If/when such a thing exists, CIDER would treat it as a separate
connection (separate TCP socket), with its own `cider-prepl` session,
language `cljs`. No bundling with the Clojure prepl in the same
session.

### Option 2: cljs.repl mid-stream

Eval `(require 'cljs.repl.node) (cljs.repl/repl (cljs.repl.node/repl-env))`
inside an active Clojure prepl. The cljs.repl/repl call takes over the
socket -- subsequent reads go to it, subsequent prints come from it.

Problems:
- The Clojure prepl's response for that form is whatever cljs.repl
  prints, which is *not* EDN. The wire format silently changes
  mid-stream.
- There's no "switch back" signal -- you'd have to send `:cljs/quit`
  and parse for a Clojure prompt re-emerging.
- All the tagged channels we rely on (`:out`, `:err`, `:ret`) go
  away.

This is fundamentally not workable with the same response parser. We
would have to build a separate prepl-vs-raw-stream mode and switch
between them on demand. Not worth it.

### Option 3: don't bundle clj+cljs over prepl

What I'd recommend: **prepl is clj-only.**

- `cider-connect-prepl` connects to a Clojure prepl. That's it.
- `cider-jack-in-cljs` and `cider-jack-in-clj&cljs` stay nREPL-only;
  the CLJS REPL story rides on Piggieback as it does today.
- If a user wants cljs, they jack in via nREPL.

This matches the user's intuition: bundled clj+cljs in the same CIDER
session is an nREPL/Piggieback artifact. Without that infrastructure,
the bundling doesn't make sense, and pretending otherwise creates
edge cases. Better to be explicit about the limitation.

If a standalone cljs prepl ever materializes upstream, we can add
`cider-connect-prepl-cljs` as a sibling. Until then: clj-only.

## Open questions

0. **`cl-defmethod` dispatch by backend type.** *(Surfaced by the
   prototype.)* Both the nREPL and prepl methods specialize the
   connection arg as `(conn buffer)`, so the cl dispatch table picks
   whichever was *defined last*, not whichever matches the runtime
   `cider-backend-type` of the buffer. Today the prepl methods happen
   to win because `cider-prepl.el` loads after `cider-connection.el`,
   but that's load-order luck, not a real dispatch.

   Two reasonable fixes:

   a. Wrap each generic with an outer dispatcher that consults
      `cider-backend-type` and forwards to a backend-specific helper:

      ```elisp
      (cl-defgeneric cider-send-eval--impl (backend-type conn code handler ...))
      (cl-defmethod cider-send-eval--impl ((_ (eql 'nrepl)) conn ...) ...)
      (cl-defmethod cider-send-eval--impl ((_ (eql 'prepl)) conn ...) ...)
      (defun cider-send-eval (conn code handler &rest args)
        (apply #'cider-send-eval--impl (cider-backend-type conn) conn code handler args))
      ```

   b. Wrap each connection in a typed struct (`cider-nrepl-conn`,
      `cider-prepl-conn`) that holds the buffer, and dispatch on the
      struct type. Cleanest CLOS-shape but every caller now has to
      construct the struct.

   Lean toward (a) -- minimal change, the existing `buffer` argument
   stays a buffer, dispatch becomes correct. Worth doing before any
   in-tree call sites migrate to the new generics.

1. **Sync request semantics on prepl.** nREPL's `nrepl-send-sync-request`
   blocks until the `done` status arrives. On prepl, "done" is implicit
   in `:ret`/`:exception`. The sync API will need to be adapted to that
   ordering-based model. Probably fine, but worth prototyping early.

2. **`:tap` channel.** prepl has a `:tap` tag for `tap>` output. nREPL
   has no equivalent in eval responses. Do we route to stdout (loses
   distinction), add a new `:on-tap` slot to `nrepl-make-eval-handler`,
   or expose it as a separate user-facing buffer? Lean toward a new
   slot.

3. **Spawning a prepl from `cider-jack-in`.** Easy for tools.deps via an
   alias. Less obvious for Leiningen. Skip Lein support initially?

4. **Existing nREPL `cider-jack-in-tools` registry** vs. a new prepl
   registry. The existing registry assumes nREPL semantics in several
   places (e.g. dependency injection of cider-nrepl). Probably needs a
   `:protocol` key on tool entries: `nrepl` (default) vs `prepl`. New
   keys mean a small migration but the registry refactor (#3884) was
   designed to absorb exactly this kind of extension.

5. **EDN reading.** parseedn is already a CIDER dependency. Does it
   support incremental reading from a stream / queue, or do we need a
   small ring-buffer wrapper? Worth confirming early.

## Risks

- Step 1 will surface request-site issues we haven't seen yet, just as
  the response-side decoupling did. Plan to treat those as the same
  sort of audit-level finds, not blockers.
- Step 3 fallbacks will need to handle real-world Clojure code (e.g.
  `clojure.repl/doc` printing to `*out*` instead of returning); some
  parsing is unavoidable. Each fallback is bounded, but the cumulative
  surface is "the things CIDER assumed cider-nrepl gave it for free."
- Performance: eval-fallback ops are inherently slower than dedicated
  middleware ops. Acceptable for prepl; we should make sure we don't
  accidentally route nREPL traffic through them too.
