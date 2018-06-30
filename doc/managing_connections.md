Because connections map one-to-one to REPL buffers, for the purpose of this
section we use "REPL" and "connection" interchangeably.

## Sessions

CIDER maintains a grouped view of opened connections through [Sesman]
sessions. Each session is a collection of connections which share same nREPL
server.

Start new sessions with

   - <kbd>C-c M-j</kbd> `cider-jack-in-clj`
   - <kbd>C-c M-J</kbd> `cider-jack-in-cljs`
   - <kbd>C-c M-c</kbd> `cider-connect-clj`
   - <kbd>C-c M-C</kbd> `cider-connect-cljs`
   - <kbd>M-x</kbd> `cider-jack-in-clj&cljs`

Add new REPLs to the current session with

   - <kbd>C-c M-s</kbd> `cider-connect-sibling-clj`
   - <kbd>C-c M-S</kbd> `cider-connect-sibling-cljs`

Session life-cycle management commands live in the [Sesman] map (<kbd>C-c
C-s</kbd>)

   - <kbd>C-c C-s s</kbd> `sesman-start`
   - <kbd>C-c C-s r</kbd> `sesman-restart`
   - <kbd>C-c C-s q</kbd> `sesman-quit`

where `sesman-start` is similar to `cider-jack-in-clj&cljs` except that the
`cljs` REPL is started only if ClojureScript requirements have been met.

To quit or restart individual connections use cider commands

  - <kbd>C-c C-q</kbd> `cider-quit`
  - <kbd>C-c M-r</kbd> `cider-restart`


## Context Links

Sessions can be linked to contexts (projects, directories and buffers)

  - <kbd>C-c C-s b</kbd> `sesman-link-with-buffer`
  - <kbd>C-c C-s d</kbd> `sesman-link-with-directory`
  - <kbd>C-c C-s p</kbd> `sesman-link-with-project`
  - <kbd>C-c C-s u</kbd> `sesman-unlink`

## Current Session

All CIDER commands (evaluation, completion, switching to REPL etc.) operate on
the current REPL within the current session. Current session is the most
"relevant" session among all linked session. Session relevance is decided by the
specificity of the linked contexts and recency of the REPL buffers.

If the current context is linked to a single session then that session is the
current one. If multiple sessions are linked to a context (say, a project) then
the current session is the one containing the most recently viewed REPL.

Links to more specific context have precedence. For example, if you have two
session linked to the same project and another to a directory within that
project, then the session linked to the directory is the current session. Thus,
again, there is no ambiguity.

By default [Sesman] allows multiple simultaneous links to projects and
directories, but only one link per buffer. See `sesman-single-link-contexts` if
you would like to change that.

Retrieve info on the current session with <kbd>C-c C-s i</kbd>
(`sesman-show-session-info`). With a single prefix <kbd>C-u</kbd> it shows info
on all linked sessions and with double prefix <kbd>C-u C-u</kbd> on all CIDER
sessions. Display active links with <kbd>C-c C-s l</kbd> (`sesman-show-links`).


## Current REPL

Current REPL is the most relevant REPL from the current session. REPL relevance
is determined by the type of the current buffer. For example if the current
buffer is a `clj` buffer a `clj` REPL is selected. Ambiguous situations could
arise when, for instance, there are multiple `clj` REPLs within a session, or
the current buffer is a `cljc` buffer and both `clj` and `cljs` REPLs are part
of the session. In such cases the current REPL is the most recently viewed REPL
of the relevant type.

Switch to the current REPL buffer with <kbd>C-c C-z</kbd>. You can then use the
same key combination to switch back to the Clojure(Script) buffer that you came
from.

The single prefix <kbd>C-u C-c C-z</kbd>, will switch you to the current REPL
buffer and set the namespace in that buffer based on namespace in the current
Clojure(Script) buffer.


[Sesman]: https://github.com/vspinu/sesman
