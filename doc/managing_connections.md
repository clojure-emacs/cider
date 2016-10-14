## Overview

You can connect to multiple nREPL servers using <kbd>M-x</kbd> `cider-jack-in`
(or `cider-connect`) multiple times.  To close the current nREPL connection, use
<kbd>C-c C-q</kbd> (`cider-quit`). You can close all connections with
<kbd>C-u C-c C-q</kbd>.

CIDER maintains a list of nREPL connections and a single 'default'
connection. When you execute CIDER commands in a Clojure editing buffer such as
to compile a namespace, these commands are executed against a specific
connection. This is controlled by the variable `cider-request-dispatch` - when
it's set to `'dynamic` (the default), CIDER will try to infer which connection
to use from the current project and currently visited file; when `'static`
dispatch is used all requests will always be routed to the default connection
(this was the default behavior in CIDER before 0.10).

There's a handy command called `cider-toggle-request-dispatch`. You can use it
to quickly switch between dynamic and static request dispatch. A common use-case
for it would be to force temporary all evaluation commands to be using a
particular (the default) connection.

You can display the current nREPL connection using <kbd>C-c M-d</kbd>
and rotate the default connection using <kbd>C-c M-r</kbd>. Another
option for setting the default connection is to execute the command
<kbd>M-x</kbd> `cider-make-connection-default` in the appropriate
REPL buffer.

## Connection browser

You can obtain a list of all active connections using <kbd>M-x</kbd>
`cider-connection-browser`. This buffer provides a few extra keybindings:

Command                              |Keyboard shortcut               | Description
-------------------------------------|--------------------------------|-------------------------------
`cider-connections-make-default`     |<kbd>d</kbd>                    | Make connection at point default.
`cider-connections-close-connection` |<kbd>k</kbd>                    | Close connection at point.
`cider-connection-browser`           |<kbd>g</kbd>                    | Refresh connection browser.
`cider-connections-goto-connection`  |<kbd>RET</kbd>                  | Visit connection buffer.
`cider-popup-buffer-quit-function`   |<kbd>q</kbd>                    | Close window.

## Switch to connection buffer

The REPL buffers double as connection buffers.

To switch to the relevant REPL buffer based on the Clojure namespace
in the current Clojure buffer, use: <kbd>C-c C-z</kbd>. You can then
use the same key combination to switch back to the Clojure buffer you
came from.

The single prefix <kbd>C-u C-c C-z</kbd>, will switch you to the
relevant REPL buffer and set the namespace in that buffer based on
namespace in the current Clojure buffer.

## Renaming connections

To change the designation used for CIDER buffers use <kbd>M-x</kbd>
`cider-change-buffers-designation`. This changes the CIDER REPL
buffer, nREPL connection buffer and nREPL server buffer. For example
using `cider-change-buffers-designation` with the string "foo" would
change `*cider-repl localhost*` to `*cider-repl foo*`.
