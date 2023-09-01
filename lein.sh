#!/usr/bin/env bash
# there's no -Ee -o pipefail, intentionally

# This wrapper script invokes lein with the provided args (which must include the enrich-classpath middleware),
# detecting success, and invoking in return the generated `java` command.
# It falls back to lein with the same args, except that the enrich middleware will be replaced with a no-op middleware.
# By having all this logic as a .sh script (vs. inline in Elisp), we can keep using the same process/async machinery,
# which is concise and results in a non-blocking UX.

lein="$1"
shift

output=$(2>&1 "$lein" "$@")
cmd=$(grep "\s-cp\s"<<< "$output")

if grep --silent "\s-cp\s"<<< "$cmd"; then
  eval "$cmd"
else
  # Print errors:
  mkdir -p ~/.emacs.d
  echo "$output" >> ~/.emacs.d/cider-error.log
  no_enrich=()
  for arg in "$@"; do
    if [ "$arg" == "cider.enrich-classpath.plugin-v2/middleware" ]; then
      no_enrich+=("cider.enrich-classpath.fallback/middleware")
    else
      no_enrich+=("$arg")
    fi
  done
  $lein "${no_enrich[@]}"
fi
