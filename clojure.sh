#!/usr/bin/env bash
set -Eeuo pipefail
# This wrapper script adds enrich-classpath's (https://github.com/clojure-emacs/enrich-classpath) functionalities to the `clojure` binary.
# It works by accepting the `clojure` binary as the first argument, and whatever arguments you'd pass to `clojure` as the rest.
# sample usage: clojure.sh clojure -Asome-alias <<< "(System/getProperty \"java.class.path\")"

clojure="$1"
# remove it from "$@"/"$*":
shift

file="deps.edn"

if [ ! -e $file ]; then
  echo "$file not found."
  $clojure "$@"
elif [[ "$*" == *Spath* ]]; then
  echo "-Spath was passed; skipping enrich-classpath."
  $clojure "$@"
elif [[ "$*" == *Scp* ]]; then
  echo "-Scp was passed; skipping enrich-classpath."
  $clojure "$@"
else

  here="$PWD"
  there=$(mktemp -d -t mytempdir.XXXXXX)

  # copy any relevant file to the temporary folder
  files_to_copy=( ".tool-versions" # asdf runtime versions configuration
                )
  for file_to_copy in "${files_to_copy[@]}"; do
    if [ -e "$file_to_copy" ]; then
      cp "$file_to_copy" "$there"
    fi
  done

  # don't let local deps.edn files interfere:
  cd "$there"

  # enrich-classpath will emit a command starting by "clojure", or print a stacktrace:
  output=$(2>&1 "$clojure" -Sforce -Srepro -J-XX:-OmitStackTraceInFastThrow -J-Dclojure.main.report=stderr -Sdeps '{:deps {mx.cider/tools.deps.enrich-classpath {:mvn/version "1.18.6"}}}' -M -m cider.enrich-classpath.clojure "$clojure" "$here" "true" "$@")
  cmd=$(tail -n1 <(echo "$output"))

  cd "$here"

  if grep --silent "^$clojure" <<< "$cmd"; then
    # eval is necessary because $cmd contains arguments that have been processed through pr-str.
    eval "$cmd"
  else
    # Print errors:
    echo "$output"
    $clojure "$@"
  fi

fi
