## Overview

You can connect to multiple nREPL servers using <kbd>M-x</kbd> `cider-jack-in`
(or `cider-connect`) multiple times.  To close the current nREPL connection, use
<kbd>C-c C-q</kbd> (`cider-quit`). You can close all connections with
<kbd>C-u C-c C-q</kbd>.

CIDER maintains a list of nREPL connections and a single 'default'
connection. When you execute CIDER commands in a Clojure editing buffer such as
to compile a namespace, these commands are executed against a specific
connection.

You can display info on current nREPL connection using <kbd>C-c M-d</kbd>.

## Connection browser

You can obtain a list of all active connections using <kbd>M-x</kbd>
`cider-connection-browser`. This buffer provides a few extra keybindings:

Command                              |Keyboard shortcut               | Description
-------------------------------------|--------------------------------|-------------------------------
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
