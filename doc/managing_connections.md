## Overview

You can connect to multiple nREPL servers using <kbd>M-x</kbd> `cider-jack-in`
(or `cider-connect`) multiple times.  To close the current nREPL connection, use
<kbd>C-c C-q</kbd> (`cider-quit`). You can close all connections with
<kbd>C-u C-c C-q</kbd>.

CIDER maintains a list of nREPL connections and a single 'default'
connection. When you execute CIDER commands in a Clojure editing buffer such as
to compile a namespace, these commands are executed against a specific
connection.

You can display the current nREPL connection using <kbd>C-c M-d</kbd>
and rotate the default connection using <kbd>C-c M-r</kbd>.

## Switch to connection buffer

The REPL buffers double as connection buffers.

To switch to the relevant REPL buffer based on the Clojure namespace
in the current Clojure buffer, use: <kbd>C-c C-z</kbd>. You can then
use the same key combination to switch back to the Clojure buffer you
came from.

The single prefix <kbd>C-u C-c C-z</kbd>, will switch you to the
relevant REPL buffer and set the namespace in that buffer based on
namespace in the current Clojure buffer.
