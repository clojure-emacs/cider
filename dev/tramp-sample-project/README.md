This project spins up a Clojure project within a Docker image.

The Docker image exposes a SSH server.

This way, for development purposes, we can SSH into it with TRAMP and exercise CIDER's TRAMP-related capabilities.

## Some ways to get started:

### `cider-jack-in` from a tramp buffer
* `M-:` `(async-shell-command "make run")` to run the Docker image
* `M-:` `(find-file "/sshx:root@localhost#8022:/usr/src/app/src/foo.clj")`
* `M-x` `cider-jack-in`
* Enter password: `cider`

###  Manually create a remote repl and connect to it
* In one terminal tab, run `make run` to run the Docker image
* Once it's ready, from another tab, run `make ssh` and start a repl manually from there
  * The password is `cider`
  * `cd /usr/src/app; lein repl :headless :host 0.0.0.0 :port 7888`

Now, from emacs you can `cider-connect` to localhost.

* `M-:`, `(dired "/sshx:root@localhost#8022:/usr/src/app")`
* `M-x cider-connect` (choose `localhost`, `7888`)

NOTE: Do not visit `foo.clj` directly - do it from dired instead.
