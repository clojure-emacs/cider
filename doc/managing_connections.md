!!! Note

    Because connections map one-to-one to REPL buffers, for the purpose of this
    section we use "REPL" and "connection" interchangeably.

## Sessions

CIDER maintains a grouped view of opened connections through [Sesman]
sessions. Each session is a collection of connections which share the same nREPL
server.

Start new sessions with

   - <kbd>C-c C-x j j</kbd> `cider-jack-in-clj`
   - <kbd>C-c C-x j s</kbd> `cider-jack-in-cljs`
   - <kbd>C-c C-x j m</kbd> `cider-jack-in-clj&cljs`

   - <kbd>C-c C-x c j</kbd> `cider-connect-clj`
   - <kbd>C-c C-x c s</kbd> `cider-connect-cljs`
   - <kbd>C-c C-x c m</kbd> `cider-connect-clj&cljs`

Add new REPLs to the current session with

   - <kbd>C-c C-x s j</kbd> `cider-connect-sibling-clj`
   - <kbd>C-c C-x s s</kbd> `cider-connect-sibling-cljs`

Session life-cycle management commands live in the [Sesman] map (<kbd>C-c
C-s</kbd>)

   - <kbd>C-c C-s s</kbd> `sesman-start`
   - <kbd>C-c C-s r</kbd> `sesman-restart`
   - <kbd>C-c C-s q</kbd> `sesman-quit`

The command `sesman-start` wraps around all of the aforementioned `jack-in` and
`connect` commands. You can also invoke same functionality with <kbd>M-x</kbd>
`cider` or <kbd>C-c M-x</kbd>.

To quit or restart individual connections use cider commands

  - <kbd>C-c C-q</kbd> `cider-quit`
  - <kbd>C-c M-r</kbd> `cider-restart`


## Context Links

Sessions can be linked to contexts (projects, directories and buffers)

  - <kbd>C-c C-s b</kbd> `sesman-link-with-buffer`
  - <kbd>C-c C-s d</kbd> `sesman-link-with-directory`
  - <kbd>C-c C-s p</kbd> `sesman-link-with-project`
  - <kbd>C-c C-s u</kbd> `sesman-unlink`

## Displaying Session Info

Retrieve info on all linked with the current context sessions with <kbd>C-c C-s
i</kbd> (`sesman-info`). On <kbd>C-u</kbd>, display info for all CIDER
sessions. For the connection specific information use CIDER's built-in
`cider-describe-connection` (<kbd>C-c M-d</kbd>).

An interactive view of all CIDER sessions is available through the
`sesman-browser` (<kbd>C-c C-s w</kbd>).

## Current Session

All CIDER commands (evaluation, completion, switching to REPL etc.) operate on
the relevant REPL within the current session. The current session is the most
relevant session among all linked session. Session relevance is decided by the
specificity of the linked contexts and recency of the REPL buffers.

If the current context is linked to a single session then that session is the
current one. If multiple sessions are linked to a context (say, a project) then
the current session is the one containing the most recently viewed REPL.

Links to more specific contexts have precedence. For example, if you have two
sessions linked to the same project and another to a directory within that
project, then the session linked to the directory is the current session. Thus,
again, there is no ambiguity.

By default [Sesman] allows multiple simultaneous links to projects and
directories, but only one link per buffer. See `sesman-single-link-contexts` if
you would like to change that.

## Current REPL

The current REPL is the most relevant REPL from the current session. REPL relevance
is determined by the type of the current buffer. For example if the current
buffer is a `clj` buffer then a `clj` REPL is selected. Ambiguous situations could
arise when, for instance, there are multiple `clj` REPLs within a session, or
the current buffer is a `cljc` buffer and both `clj` and `cljs` REPLs exist in
the session. In such cases the current REPL is the most recently viewed REPL of
the relevant type.

Switch to the current REPL buffer with <kbd>C-c C-z</kbd>. You can then use the
same key combination to switch back to the Clojure(Script) buffer that you came
from.

The single prefix <kbd>C-u C-c C-z</kbd>, will switch to the current REPL buffer
and set the namespace in that buffer based on namespace in the current
Clojure(Script) buffer.

## Customizing Session and REPL Names

By default session names consist of abbreviated project name, host and port
(e.g. `project/dir:localhost:1234`). REPL buffer name consist of the session
name and the REPL type specification post-fix
(e.g. `*project/dir:localhost:1234(cljs:node)*`).

You can customize session names with `cider-session-name-template` and REPL
names with `nrepl-repl-buffer-name-template`. See also
`cider-format-connection-params` for available formats.


[Sesman]: https://github.com/vspinu/sesman
