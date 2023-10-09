This project spins up a Clojure project within a Docker image.

The Docker image exposes a SSH server.

This way, for development purposes, we can SSH into it with TRAMP and exercise CIDER's TRAMP-related capabilities.

To get started:

* In one terminal tab, run `make run` to run the Docker image
* Once it's ready, from another tab, run `make ssh` and start a repl manually from there
  * The password is `cider`
  * `cd /usr/src/app; lein repl :headless :host 0.0.0.0 :port 7888`

Now, from emacs you can `cider-connect` to localhost.

* `M-:`, `(dired "/sshx:root@localhost#8022:/usr/src/app")`
* `M-x cider-connect` (choose `localhost`, `7888`)

NOTE: Do not visit `foo.clj` directly - do it from dired instead.
