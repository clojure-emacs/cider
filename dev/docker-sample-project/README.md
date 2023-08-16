This project spins up a Clojure project within a Docker image.

The Docker image exposes an nREPL server.

This way, for development purposes, we can exercise CIDER's Docker-related capabilities.

To get started:

* From a terminal tab, run `make run` to run the Docker image
  * Note that it has a volume mapping for `src`, so any local changes will be visible in the Docker image.
  * Also note that the root of this subproject has a .dir-locals.el setting up `cider-path-translations`.
* `M-x cider-connect-clj`, choose localhost, 7888
* `M-x cider-load-buffer` the foo.clj namespace.
* From now on, you can `M-.` (jump to definition) recursively, starting from `clj-http.client`.
