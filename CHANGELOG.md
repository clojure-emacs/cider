# Changelog

## master (unreleased)

### New features

- [#3352](https://github.com/clojure-emacs/cider/pull/3352) Add CIDER Log Mode, a major mode that allows you to capture, debug, inspect and view log events emitted by Java logging frameworks.
- [#3354](https://github.com/clojure-emacs/cider/issues/3354): Add new customization variable `cider-reuse-dead-repls` to control how dead REPL buffers are reused on new connections.

### Bugs fixed

- [#3341](https://github.com/clojure-emacs/cider/issues/3341): Escape clojure-cli args on MS-Windows on non powershell invocations.
- [#3353](https://github.com/clojure-emacs/cider/issues/3353): Fix regression which caused new connections to prompt for reusing dead REPLs.
- [#3355](https://github.com/clojure-emacs/cider/pull/3355): Fix `cider-mode` disabling itself after a disconnect when `cider-auto-mode` is set to nil.
- [#3362](https://github.com/clojure-emacs/cider/issues/3362): Fix `sesman-restart` regression issue.

### Changes

- Bump the injected `cider-nrepl` to 0.31.

## 1.7.0 (2023-03-23)

### New features

- [#3314](https://github.com/clojure-emacs/cider/issues/3314): Detect `nrepl+unix` sockets (say via `lein nrepl :headless :socket nrepl.sock`).
- [#3262](https://github.com/clojure-emacs/cider/issues/3262): Add navigation functionality to `n/p/f/b` keys inside the data inspector's buffer.
- [#3310](https://github.com/clojure-emacs/cider/issues/3310): Add ability to use custom coordinates in `cider-jack-in-dependencies`.
- [cider-nrepl#766](https://github.com/clojure-emacs/cider-nrepl/issues/766): Complete local bindings for ClojureScript files.
- [#3179](https://github.com/clojure-emacs/cider/issues/3179): Introduce `cider-jack-in-universal` to support jacking-in without a project from a set of pre-configured Clojure project tools.

### Changes

- Allow using `npx nbb` as `cider-nbb-command`.
- [#3281](https://github.com/clojure-emacs/cider/pull/3281): Replace newline chars with actual newlines in `*cider-test-report*` buffer, for prettier error messages.
- Bump the injected `cider-nrepl` to 0.30.
- [#3219](https://github.com/clojure-emacs/cider/issues/3219): Disable by default forcing the display of output when the REPL prompt is at the first line of the of the REPL window. This behavior is desirable, but very slow and rarely needed. It can be re-enabled by setting `cider-repl-display-output-before-window-boundaries` to `t`.
- [#3335](https://github.com/clojure-emacs/cider/issues/3335): Disable the Paredit binding of RET in cider-repl-mode buffers, which can cause unexpected behaviour by appearing to hang instead of evaluating forms.
- [#3307](https://github.com/clojure-emacs/cider/issues/3307): Make eldoc highlighting on emacs special forms better match the location of the point when latest `cider-nrepl` is used.

## 1.6.0 (2022-12-21)

### New features

- [#3278](https://github.com/clojure-emacs/cider/pull/3278): Introduce integration tests, which also fix a long standing issue with orphaned process on MS-Windows by contracting `taskkill`, if available, to properly kill the nREPL server process tree.
- [#3061](https://github.com/clojure-emacs/cider/issues/3061): Add support for `nbb`.
- [#3249](https://github.com/clojure-emacs/cider/pull/3249): Add support for Clojure Spec 2.
- [#3247](https://github.com/clojure-emacs/cider/pull/3247): Add the `cider-stacktrace-analyze-at-point` and `cider-stacktrace-analyze-in-region` commands to view printed exceptions in the stacktrace inspector.

### Changes

- Bump the injected nREPL version to 1.0.
- [#3061](https://github.com/clojure-emacs/cider/issues/3061): Allow to use `cider-connect-clj` for self-hosted cljs repls (e.g. `nbb`).
- [#3291](https://github.com/clojure-emacs/cider/pull/3291): **Remove** the `'cljs-pending` `repl-type`. It is replaced by `cider-repl-cljs-upgrade-pending`.
- [#3261](https://github.com/clojure-emacs/cider/issues/3261): If user is connecting to nREPL from a TRAMP buffer, use its connection parameters (port, username) for establishing SSH tunnel.

### Bugs fixed

- Remove needless quotes from the choices of `cider-jack-in-auto-inject-clojure`.
- [#2561](https://github.com/clojure-emacs/cider/issues/2561): Disable undo in `*cider-test-report*` buffers.
- [#3251](https://github.com/clojure-emacs/cider/pull/3251): Disable undo in `*cider-stacktrace*` buffers.
- Consecutive overlays will not be spuriously deleted.
- [#3260](https://github.com/clojure-emacs/cider/pull/3260): Scroll REPL buffer in other frame.
- [#3293](https://github.com/clojure-emacs/cider/issues/3293): Can't jack in to more than one `bb` projects.

## 1.5.0 (2022-08-24)

### New features

- [#3226](https://github.com/clojure-emacs/cider/pull/3226): Populate completions metadata, making it possible to change the style of completion via `completion-category-override` or `completion-category-defaults`.
- [#2946](https://github.com/clojure-emacs/cider/issues/2946): Add custom var `cider-merge-sessions` to allow combining sessions in two different ways: Setting `cider-merge-sessions` to `'host` will merge all sessions associated with the same host within a project. Setting it to `'project` will combine all sessions of a project irrespective of their host.
- Support Gradle jack-in via the Gradle wrapper (`gradlew`), instead of just a globally installed `gradle` on the `PATH`.
- Gradle projects can now inject dependencies and middleware as with other build tools (dependency injection requires [Clojurephant](https://github.com/clojurephant/clojurephant) 0.7.0 or higher).
- [#3239](https://github.com/clojure-emacs/cider/issues/3239): Added commands to evaluate and tap last sexp (`cider-tap-last-sexp`) and sexp at point (`cider-tap-sexp-at-point`).

## Changes

- Upgrade clojure-mode to [5.15.1](https://github.com/clojure-emacs/clojure-mode/blob/v5.15.1/CHANGELOG.md).
- Upgrade injected `cider-nrepl` to [0.28.5](https://github.com/clojure-emacs/cider-nrepl/releases/tag/v0.28.5).
- [#3200](https://github.com/clojure-emacs/cider/issues/3200): Improve cider-browse-ns interface to allow selective hiding of var types as well as grouping options.  Include private vars in result list.
- Changed default `cider-gradle-command` to `./gradlew` to use the Gradle wrapper.
- Changed default `cider-gradle-global-options` to `""` (empty, formerly `--no-daemon`).
- [#3234](https://github.com/clojure-emacs/cider/pull/3234): Autocomplete multiple available ports on nREPL connect.

### Bugs fixed

- [#3235](https://github.com/clojure-emacs/cider/issues/3235): Check `name` is a TRAMP file in `cider--client-tramp-filename` via `tramp-tramp-file-p`.

## 1.4.1 (2022-05-25)

## Changes

* Upgrade cider-nrepl to [0.28.4](https://github.com/clojure-emacs/cider-nrepl/releases/tag/v0.28.4).

### Bugs fixed

* [#3195](https://github.com/clojure-emacs/cider/issues/3195): Revert the change that resulted in `(error "Cyclic keymap inheritance")` on `cider-test-run-test`.
* [#3182](https://github.com/clojure-emacs/cider/issues/3182): Don't try to invoke
JVM-specific code outside of JVM Clojure.
* [#3202](https://github.com/clojure-emacs/cider/pull/3202): Fix `cider-eval-ns-form`
  * Do not always perform `undef-all`. Undef only with `C-u` prefix.
  * Fix extraction of namespace name.

## 1.4.0 (2022-05-02)

## New features

* [#3188](https://github.com/clojure-emacs/cider/pull/3188): Add support for `undef-all` op, for removing stale vars and conflicting aliases.
  * Add new command `cider-undef-all`.
  * Existing commands `cider-load-buffer`, `cider-load-file`, and `cider-eval-ns-form` can be called with `C-u` prefix to execute `undef-all` before reloading the ns.
* [#3185](https://github.com/clojure-emacs/cider/pull/3185): Add feature to `cider-eval-in-context` for automatically extracting parent let bindings when called with `C-u` prefix argument.
* Add new interactive command `cider-inspire-me`. It does what you'd expect.
* [#3162](https://github.com/clojure-emacs/cider/pull/3162): Save eval results into kill ring and registers.
  * Add new customization variable `cider-eval-register` to automatically store the last interactive eval result into the specified register.
  * Add interactive command `cider-kill-last-result` to manually save the last eval result into kill ring.

### Changes

* [#3177](https://github.com/clojure-emacs/cider/pull/3177): Apply ANSI colorization to test assertion output.
* Use clojure-mode [5.14.0](https://github.com/clojure-emacs/clojure-mode/blob/v5.14.0/CHANGELOG.md#5140-2022-03-07).

### Bugs fixed

* [#3170](https://github.com/clojure-emacs/cider/issues/3170): Skip ensure repl available checks on xref functions. (this improves the interop with `clojure-lsp`)
* [#3173](https://github.com/clojure-emacs/cider/issues/3173): Locally remove `cider-complete-at-point` from `completion-at-point-functions` instead of killing it as a local variable.
* [#3172](https://github.com/clojure-emacs/cider/issues/3172): Restore the long-lost (but critical) inspirational message on connect.
* [#3186](https://github.com/clojure-emacs/cider/pull/3186): An assortment of small fixes.

## 1.3.0 (2022-03-07)

### New features

* [#3148](https://github.com/clojure-emacs/cider/pull/3148): Display error messages in multiline comment eval results, and in result overlays when `cider-show-error-buffer` is set to `nil`.
* [#3149](https://github.com/clojure-emacs/cider/pull/3149): Add option `'change` to `cider-eval-result-duration`, allowing multiple eval result overlays to persist until the next change to the buffer.

### Changes

* [#3127](https://github.com/clojure-emacs/cider/pull/3040): Strip all exec-opts flags (`-A` `-M` `-T` `-X`) if they exist in `cider-clojure-cli-aliases`. Also addresses a duplicate `:` in the generated `clj` command.
* `cider-jack-in-lein-plugins` no longer affects non-Leiningen projects.
  * Third-party packages should rely on `cider-jack-in-dependencies` instead.
* Upgrade cider-nrepl to [0.28.3](https://github.com/clojure-emacs/cider-nrepl/blob/v0.28.3/CHANGELOG.md#0283-2022-02-22).
* Remove `cider-jdk-src-paths` defcustom since enrich-classpath makes it redundant.
* Remove `cider-resolve-java-class` function since enrich-classpath makes it redundant.

### Bugs fixed

* Upgrade [enrich-classpath](https://github.com/clojure-emacs/enrich-classpath), which fixes various edge cases.
  * Remember: at the moment the enrich-classpath is disabled by default. If you wish to try it out, you can customize `cider-enrich-classpath` to `t`.
  * Also remember: for it to work, on Linux, you'll also have to do something like `sudo apt install openjdk-11-source` (depending on your package manager and JDK of choice).
* [#3145](https://github.com/clojure-emacs/cider/pull/3145): Allow fallback to other `xref` backends if cider-nrepl is not loaded.
* [#3148](https://github.com/clojure-emacs/cider/pull/3148): Fix eval result overlays at point inheriting the faces of following text.
* [#3133](https://github.com/clojure-emacs/cider/issues/3133): Respect `cider-injected-middleware-version`.
* [#3163](https://github.com/clojure-emacs/cider/pull/3163): `cider-clojuredocs`: prevent redundant prompt for a symbol.

## 1.2.0 (2021-12-22)

### New features

* Integrate [enrich-classpath](https://github.com/clojure-emacs/enrich-classpath) by default for Leiningen projects.
  * This enables functionality related to Java sources, javadocs or parsing thereof.
  * This can slightly slow down jack-in for the _first_ time for a given project; later on the related work will be cached.
  * The feature is experimental at this point and needs to be enabled with `(setq cider-enrich-classpath t)`.
* [#2831](https://github.com/clojure-emacs/cider/issues/2831): Add `xref` integration, configured with customizable variables `cider-use-xref` and `cider-xref-fn-depth`.
* [#3017](https://github.com/clojure-emacs/cider/issues/3017): Annotate company completion kinds.
* [#3040](https://github.com/clojure-emacs/cider/pull/3040): Support invoking `cider-clojuredocs` within the `*clojuredocs*` buffer.
* Make it possible to specify the version of `cider-nrepl` to use with `cider-jack-in`. See `cider-injected-middleware-version`.
* Make it possible to specify the version of nREPL to use with `cider-jack-in`. See `cider-injected-nrepl-version`.
* Upgrade `cider-nrepl`, `Orchard` and `clj-suitable` for pulling their latest bugfixes.
* Add support for babashka projects to `cider-jack-in`.
* Introduce `cider-jack-in-lein-middlewares` defcustom.
* [#3093](https://github.com/clojure-emacs/cider/pull/3093): Make `see-also`s clickable in ClojureDocs buffers.
* [#3044](https://github.com/clojure-emacs/cider/pull/3044): Dynamically upgrade nREPL connection. See `cider-upgrade-nrepl-connection`.

### Bugs fixed

* [#3022](https://github.com/clojure-emacs/cider/issues/3022): Handle empty stacktraces, pointing users to docs about the `OmitStackTraceInFastThrow` JVM optimization.
* [#3020](https://github.com/clojure-emacs/cider/issues/3020): Fix session linking on Windows, e.g. when jumping into a library on the classpath.
* [#3031](https://github.com/clojure-emacs/cider/pull/3031): Fix `cider-eval-defun-up-to-point` failing to match delimiters correctly in some cases, resulting in reader exceptions.
* [#3039](https://github.com/clojure-emacs/cider/pull/3039): Allow starting the sideloader for the tooling session.
* [#3041](https://github.com/clojure-emacs/cider/pull/3041): Sideloader: handle binary files, support multiple directories.
* [#3047](https://github.com/clojure-emacs/cider/pull/3047): Fix info/lookup fallback: response has an extra level.
* [#2746](https://github.com/clojure-emacs/cider/issues/2746): Handle gracefully Clojure versions with non-standard qualifiers (e.g. `1.11.0-master-SNAPSHOT`).
* [#3069](https://github.com/clojure-emacs/cider/pull/3069): Fix cursor color changing when it shouldn't in `evil-mode`.
* [#3071](https://github.com/clojure-emacs/cider/issues/3071): Use `xref` instead of `etags` to push point to marker stack.
* [#3074](https://github.com/clojure-emacs/cider/issues/3074): Recognize `pwsh` as a `powershell` executable.

### Changes

* Drop support for Emacs 25 (this tracks upstream deps like `parseedn` that no longer support Emacs 25 and is line with our compatibility policy for RHEL and Debian).

## 1.1.1 (2021-05-24)

### Bugs fixed

* [#3014](https://github.com/clojure-emacs/cider/pull/3014): Update Krell repl initialization code to follow latest guidelines as found in Krell wiki.
* [#3012](https://github.com/clojure-emacs/cider/issues/3012): Allow connecting sibling repls from any buffer.
* [#3010](https://github.com/clojure-emacs/cider/issues/3010): Remove `::` auto-resolved keyword expansion logic from `cider-symbol-at-point`, moving it to `cider-browse-spec`.

## 1.1.0 (2021-04-22)

### New features

* [#2930](https://github.com/clojure-emacs/cider/issues/2930): Add new customization variable `cider-test-default-include-selectors` and `cider-test-default-exclude-selectors` for specifying default test selectors when running commands such as `cider-test-run-ns-tests`.
* [#2907](https://github.com/clojure-emacs/cider/issues/2907): Add new customization variable `cider-format-code-options` to specify options used by `cljfmt` to format code when running commands `cider-format-buffer`, `cider-format-region`  and `cider-format-defun`.
* [#3002](https://github.com/clojure-emacs/cider/pull/3002): [Inspector] Make collection member truncation limits configurable.

### Bugs fixed

* [#2871](https://github.com/clojure-emacs/cider/issues/2871): Restore the dynamic code completion (the actual fixes are in `clj-suitable` and `cider-nrepl`).
* [#2993](https://github.com/clojure-emacs/cider/issues/2993): Fix bug where calling `cider-repl-set-ns` for a cljs ns when `cider-repl-require-ns-on-set` is `t` would fail.
* [#2983](https://github.com/clojure-emacs/cider/issues/2983): Update signal description in nrepl server sentinel as a workaround for Emacs bug #46284 affecting v27.1 on Windows.
* [#2941](https://github.com/clojure-emacs/cider/issues/2941): Use main args in alias for clojure cli.
* [#2953](https://github.com/clojure-emacs/cider/issues/2953): Don't font-lock function/macro vars as generic vars.
* [#2964](https://github.com/clojure-emacs/cider/issues/2964): Fix issue with `cider-company-enable-fuzzy-completion` and Helm.
* [#2937](https://github.com/clojure-emacs/cider/issues/2937): Green fringe produced for extra line in rich comment block.
* [#2996](https://github.com/clojure-emacs/cider/issues/2937): Fix debugger incorrectly locating `#_` ignored forms.
* Fix a compatibility issue with Java 15 and fetching fresh ClojureDocs data. (fixed in `cider-nrepl` 0.25.6)
* [#3004](https://github.com/clojure-emacs/cider/pull/3004): Use appropriate coding system when unzipping jars.
* [#2934](https://github.com/clojure-emacs/cider/issues/2934): Enable `eldoc-mode` in existing clojure buffers.

### Changes

* Removed `cider-clojure-cli-parameters` due to clojure-cli jack-in changes.
* Changed the behaviour of `cider-last-sexp` so it returns only the sexp, excluding all whitespace and/or the first newline after.

## 1.0.0 (2020-28-12)

### New features

* [#2909](https://github.com/clojure-emacs/cider/issues/2909): Add new customization variable `cider-inspector-auto-select-buffer` to control the auto selection of the inspector buffer.
* [#2940](https://github.com/clojure-emacs/cider/pull/2940): Add a new customization variable cider-shadow-watched-builds to allow watching several shadow-cljs builds at the same time.

### Bugs fixed

* Fix broken links to the docs in REPL warnings (the REPL links included the full CIDER version, but the docs URLs are without the patch version).
* [#2916](https://github.com/clojure-emacs/cider/issues/2916): Fix ordering of dependencies, global-opts and params for Clojure CLI projects when calling `cider-jack-in`.
* [#2929](https://github.com/clojure-emacs/cider/issues/2929): Fix handling of reader tags or metadata when calling `cider-eval-last-sexp-and-replace`.

### Changes

* Bump the injected nREPL version to 0.8.3.
* Bump the injected `cider-nrepl` version to 0.25.5.
* Bump the injected Piggieback version to 0.5.2. See [this issue](https://github.com/nrepl/piggieback/issues/118) for details.
* [#2897](https://github.com/clojure-emacs/cider/pull/2897): Translate paths from CIDER to nREPL and vice-versa.
* Set `cider-prompt-for-symbol` to `nil` by default.

## 0.26.1 (2020-08-14)

### Bugs fixed

* [#2886](https://github.com/clojure-emacs/cider/pull/2886): Don't check for `node`'s presence before starting a browser REPL.
* [#2889](https://github.com/clojure-emacs/cider/pull/2889): Fix a typo in `cider-info-form`.

### Changes

* Bump the injected piggieback version to 0.5.1.

## 0.26.0 (2020-08-03)

### New features

* Add first class support for Babashka (more warnings when you connect to `babashka.nrepl`).
* Add support for nREPL 0.8's `lookup` op.
* Add support for nREPL 0.7's sideloading functionality (experimental).
* Add support for nREPL 0.8's `ls-middleware` op.
* [#2861](https://github.com/clojure-emacs/cider/pull/2861): Add support for the Krell REPL.
* [#2881](https://github.com/clojure-emacs/cider/pull/2881): Add command to evaluate list around point (`cider-eval-list-at-point`).

### Changes

* [#2527](https://github.com/clojure-emacs/cider/issues/2527): Enable auto-clear of REPL buffer by setting a limit to the max buffer size.
* [#2852](https://github.com/clojure-emacs/cider/issues/2852): Convert 1-based column numbers in response map to Emacs' 0-based system.
* Differentiate between more types in `cider-eldoc`. They used to be just `var` and `fn` and now we have additional handling for
macros, special forms and methods.
* No longer fetches ClojureDocs data on first run (it's now bundled with `cider-nrepl`).
* No longer updates the ClojureDocs data automatically on startup (it has to be updated explicitly using `M-x cider-clojuredocs-refresh-cache`).
* Use nREPL 0.8 by default (when doing `cider-jack-in`).

### Bugs fixed

* Handle properly missing file metadata in `cider-doc` buffers, when you eval fallback to obtain var metadata.
* Show eldoc for `.` and `..`.
* [#2860](https://github.com/clojure-emacs/cider/issues/2860): Don't send blank strings in `eldoc` requests.
* [#2718](https://github.com/clojure-emacs/cider/issues/2718): When calling `cider-pprint-eval-last-sexp-to-comment`, avoid printing empty comment if eval throws error.
* [#2796](https://github.com/clojure-emacs/cider/issues/2796): Closing CIDER connection will disable the debug minor mode on clojure buffers.

## 0.25.0 (2020-06-04)

### New features

* [#2482](https://github.com/clojure-emacs/cider/pull/2842): Improvements to CIDER Inspector.
  * New defcustom `cider-inspector-skip-uninteresting` to control whether to skip over nils, numbers and keywords when navigating between values in the inspector buffer.
  * New defcustom `cider-auto-inspect-after-eval` to control whether a visible inspector buffer is updated with the last evaluated result.
* [#2833](https://github.com/clojure-emacs/cider/pull/2833): Save command history for jack-in with universal arg.
* [#2828](https://github.com/clojure-emacs/cider/pull/2828): Bind "L" to toggle display of locals during a debug session.
* [#2800](https://github.com/clojure-emacs/cider/pull/2800): Add support for force-out debugger command.
* Add support for nREPL 0.8 `completions` op. It's used if `cider-nrepl` is not available.
* Add `browser` to the list of supported ClojureScript REPL types.
* Add an interactive command to toggle Clojure font-locking in the REPL (`cider-repl-toggle-clojure-font-lock`).
* Add a defcustom controlling nREPL's print buffer size (`cider-print-buffer-size`). It's set to 4K by default, nREPL own default is 1k.

### Changes

* [#2826](https://github.com/clojure-emacs/cider/pull/2826): Add support for symbols with quotes and resolving of ns-aliased keywords in `cider-symbol-at-point`.
* [#2617](https://github.com/clojure-emacs/cider/pull/2617): Add menu bar entry for `Insert last sexp in REPL`.
* Removed support for the Nashorn ClojureScript REPL. (it was removed upstream in ClojureScript)
* [#2825](https://github.com/clojure-emacs/cider/issues/2825): Disable support for displaying images in the REPL. (set `cider-repl-use-content-types` to re-enable it)
* [#2850](https://github.com/clojure-emacs/cider/issues/2850): Ensure you're in the middle of a window after commands like `cider-find-var`.

### Bugs fixed

* [#2839](https://github.com/clojure-emacs/cider/pull/2839): Fix symbol-at-point on var-quoted symbols.
* [#2807](https://github.com/clojure-emacs/cider/pull/2807): Fix require-repl-utils for shadow-cljs repls.
* [#1971](https://github.com/clojure-emacs/cider/issues/1971), [#2628](https://github.com/clojure-emacs/cider/issues/2628): Don't try to font-lock multi-chunk results in the REPL.
* [#2816](https://github.com/clojure-emacs/cider/issues/2816): Update eldoc to work with Emacs 28.1.

## 0.24.0 (2020-02-15)

### New features

* [#2744](https://github.com/clojure-emacs/cider/pull/2744): Add startup commands to REPL banner.
* [#2499](https://github.com/clojure-emacs/cider/issues/2499): Add `cider-jump-to-pop-to-buffer-actions`.
* [#2738](https://github.com/clojure-emacs/cider/pull/2738): Add ability to lookup a function symbol when cursor is at the opening paren.
* [#2735](https://github.com/clojure-emacs/cider/pull/2735): New debugger command `P` to inspect an arbitrary expression, it was previously bound to `p` which now inspects the current value.
* [#2729](https://github.com/clojure-emacs/cider/pull/2729): New cider inspector command `cider-inspector-def-current-val` lets you define a var with the current inspector value.

### Changes

* [#2781](https://github.com/clojure-emacs/cider/pull/2781): Extend `cider-doc-xref-regexp` to recognize `[[var]]` syntax  and fully qualified symbols as xref links in cider-doc buffers.
* [#2731](https://github.com/clojure-emacs/cider/pull/2731): Make the in-buffer debugging menu customizable via `cider-debug-prompt-commands`.

### Bugs fixed

* [#2787](https://github.com/clojure-emacs/cider/issues/2787): Fix nrepl process naming collision when using `nrepl-hide-special-buffers`.
* [#2739](https://github.com/clojure-emacs/cider/pull/2739): Start built-in shadow-cljs build profiles correctly (node-repl, browser-repl).
* [#2730](https://github.com/clojure-emacs/cider/pull/2730): Require REPL utilities into current namespace not just `user` ns.
* [#2614](https://github.com/clojure-emacs/cider/issues/2614): Fix error highlighting in source buffers for Clojure 1.10.
* [#2733](https://github.com/clojure-emacs/cider/issues/2733): Restore compatibility with Emacs 25.3.

## 0.23.0 (2019-10-08)

### New features

* New configuration variable `cider-result-overlay-position` determining where debugger and inline eval result overlays should be displayed. Current options are 'at-eol and 'at-point.
* [#2606](https://github.com/clojure-emacs/cider/pull/2606): Defcustom `cider-path-translations` for translating paths from nREPL messages (useful where a file appears to be somewhere, but it's actually somewhere else).
* [#2698](https://github.com/clojure-emacs/cider/pull/2689): Infer figwheel builds automatically.
* New command `cider-clojuredocs-refresh-cache`.

### Changes

* [#2711](https://github.com/clojure-emacs/cider/pull/2711): `cider-selector` has more robust handling for edge cases.
* [#2572](https://github.com/clojure-emacs/cider/issues/2572): Make it possible to a start a one off ClojureScript REPL without defining a new REPL type.
* Dynamic cljs completions (via suitable) can be disable by setting `cider-enhanced-cljs-completion-p` to nil.

### Bugs fixed

* [#2715](https://github.com/clojure-emacs/cider/issues/2715): Fix the `shadow-cljs` presence check.
* [#2705](https://github.com/clojure-emacs/cider/issues/2705): Middleware version check looks at only at the minor version for comparison (when the major version is 0) and ensures a matching major and a minor >= required otherwise.
* Fixed some bugs related to the new suitable-powered ClojureScript code completion (this was fixed by upgrading the `suitable` used by `cider-nrepl`).
* Remove a misplaced error message when doing `clojuredocs-lookup`.
* [#2721](https://github.com/clojure-emacs/cider/issues/2721): Handle properly symbols ending in `.` (e.g. `SomeRecord.`).

## 0.22.0 (2019-09-01)

### New features

* [#2656](https://github.com/clojure-emacs/cider/issues/2656): Base64 encode clojure command and arguments on jack-in when `cider-clojure-cli-command` is `"powershell"` to avoid escaping issues. If no `clojure` command is found on Windows `cider-clojure-cli-command` defaults to `"powershell"`.
* Allow editing of jack in command with prefix or when `cider-edit-jack-in-command` is truthy.
* New defcustom `cider-repl-require-ns-on-set`: Set it to make cider require the namespace before setting it, when calling `cider-repl-set-ns`.
* [#2611](https://github.com/clojure-emacs/cider/issues/2611): Add `eval`-based classpath lookup fallback. It's used when cider-nrepl is not present.
* [#2611](https://github.com/clojure-emacs/cider/issues/2611): Add `eval`-based var info lookup fallback. It's used when cider-nrepl is not present.
* [#1840](https://github.com/clojure-emacs/cider/issues/1840): Add a command to find runtime function references (`cider-xref-fn-refs`).
* Add a command to find runtime function dependencies (`cider-xref-fn-deps`).
* Add a menu to the inspector.
* Add completion of shadow-cljs build names in the minibuffer when connecting or jacking in.

### Changes

* `cider-use-tooltips` now also controls whether `help-echo` is used.
* `cider-print-options` is now supported by the `pr` option for `cider-print-fn`. The options will now be also used by interactive eval commands that do not use pretty-printing.
* `spec-list` and `spec-form` requests send the current namespace for alias resolution.
* `C-c , C-g` and `C-c C-t C-g` cancel the key chord instead of rerunning the last test. The respective command has been moved to `C-c , C-a`, `C-c , a`, `C-c C-t C-a` and `C-c C-t a`.
* [#2643](https://github.com/clojure-emacs/cider/issues/2643): **(Breaking)** Stop using the `cider.tasks/nrepl-server` custom task for `cider-jack-in` with Boot.
* [#2647](https://github.com/clojure-emacs/cider/issues/2647): `cider-repl-require-repl-utils` now loads cljs specific REPL utils in cljs buffers.
* [#2689](https://github.com/clojure-emacs/cider/issues/2689): `cider-load-buffer` now takes an optional `callback` that will override the default `cider-load-file-handler`.
* [#2689](https://github.com/clojure-emacs/cider/issues/2689): `cider-load-file-handler` now takes an optional `done-handler` lambda that is run once load is complete.

### Bug fixes

* [#2685](https://github.com/clojure-emacs/cider/pull/2658): Send `exclude-regexps` in apropos under correct key
* Stop cursor moving when initialising the CIDER REPL, when `cider-repl-pop-to-buffer-on-connect` is nil. This fixes a bug introduced by [commit e0aca78b](https://github.com/clojure-emacs/cider/commit/e0aca78ba56425e50ea895c5adc7c0331cee0b19).
* [#2577](https://github.com/clojure-emacs/cider/issues/2577): Ensure user friendly error messages if a REPL connection is expected but none was found in certain situations.
* [#2593](https://github.com/clojure-emacs/cider/issues/2593): The REPL's initial namespace is now set correctly if configured in another tool (e.g. Leiningen's `:init-ns`).
* [#2607](https://github.com/clojure-emacs/cider/pull/2607): Use markers for specifying insertion point for `cider-eval-*-to-comment`commands. This fixes a bug where editing the buffer during a pending evaluation resulted in comments appearing in unintended locations.
* [#2308](https://github.com/clojure-emacs/cider/issues/2308): Don't rely on the classpath in `cider-library-present-p`. Now it does a `require` instead to check if some library is present or not.
* [#2541](https://github.com/clojure-emacs/cider/issues/2541): Hook properly CIDER's code completion machinery.
* [#2659](https://github.com/clojure-emacs/cider/issues/2659): Handle `#shadow/env` reader tags in `cider--shadow-get-builds`.
* [#2676](https://github.com/clojure-emacs/cider/issues/2676): Widen before `cider--file-string`, to allow `cider-load-buffer` to work on narrowed buffers.
* Don't disable `cider-mode` until all CIDER sessions have been closed.

## 0.21.0 (2019-02-19)

### New features

* The `cider-test-run-*` and `cider-ns-refresh-*` commands are now interruptible by the `cider-interrupt` command.
* Many commands now stream printed results back to the client incrementally – meaning it's now possible to, for example, interrupt evaluations while their result is being rendered.
* New option: `cider-repl-init-code`. This is a list of strings containing Clojure code to evaluate when the REPL starts (with bindings for any `set!`-able vars in place). Replaces `cider-print-length` and `cider-print-level`, which are now obsolete.
* New option: `cider-print-quota`. This is a hard limit on the number of bytes that will be returned by any printing operation. This defaults to one megabyte and can be set to `nil` if no limit is desired.

### Changes

* Add new defcustom `cider-switch-to-repl-on-insert`: Set to prevent cursor from going to the REPL when inserting a form in the REPL with the insert-to-repl commands. Replaces obsoleted `cider-switch-to-repl-after-insert-p`
* **(Breaking)** Upgrade to nREPL 0.6.0. This is now the minimum required version.
* **(Breaking)** Upgrade to piggieback 0.4.0. This is now the minimum required version.
* **(Breaking)** Remove `cider.nrepl.middleware.pprint`. All functionality has been replaced by the built-in printing support in nREPL 0.6.
* Option `cider-repl-scroll-on-output` is now obsolete, and the default REPL behavior has changed to _not_ recenter the window. The built-in variable `scroll-conservatively` can be set to 101 (either globally or locally in the REPL buffer) to restore the old behavior. This change has a dramatic positive effect on REPL performance.
* `cider-pprint-fn` and `cider-pprint-options` are now obsolete, replaced by `cider-print-fn` and `cider-print-options`.
* `cider-debug-print-options`, `cider-stacktrace-print-options`, and `cider-repl-pretty-print-width` are now all obsolete, replaced by `cider-print-options`.
* [#2546](https://github.com/clojure-emacs/cider/pull/2546): New defcustom `cider-ns-save-files-on-refresh-modes` to control for which buffers `cider-ns-refresh` should save before refreshing.

### Bug fixes

* Fix values for `cider-preferred-build-tool` variable.
* Fix value and safe property for `cider-allow-jack-in-without-project` variable.
* `cider-ns-save-files-on-refresh` will now save any modified buffers visiting files on the classpath, rather than just in the current project.
* `cider-expected-ns` no longer requires an absolute path as its argument, and now internally handles paths canonically and consistently.
* Fixed a bug causing REPL output to be inserted after the prompt.
* Fixed a bug causing `cider-pprint-eval-last-sexp-to-comment` and `cider-pprint-eval-defun-to-comment` to not insert anything.
* `cider-find-var` now correctly uses a new window when passed a prefix of `-` or a double prefix argument.

## 0.20.0 (2019-01-14)

### New features

* Make it possible to pass an options map to the currently selected pprint function via `cider-pprint-options`.
* Add support for zprint.
* Make it possible to eval and pprint in the scratch buffer using `C-u C-j`.
* [#2532](https://github.com/clojure-emacs/cider/pull/2532): Add support for `CompilationException` dynamic source location discovery.

### Changes

* [#2496](https://github.com/clojure-emacs/cider/issues/2496): Replace CIDER's pprint implementation with nREPL 0.5's built-in pprint support.
* [#2558](https://github.com/clojure-emacs/cider/pull/2558): Load clj, cljc, & cljs (if cljs REPL available) files on `cider-load-all-files` (`C-c C-M-l`). Previously, this only loaded clj files.
* Enable pretty-printing in the REPL by default.

### Bug fixes

* [#2532](https://github.com/clojure-emacs/cider/pull/2532): Fix re-display hangs while dynamically recovering source locations under mouse pointer.
* [#2560](https://github.com/clojure-emacs/cider/pull/2560): Detect REPL type for completion, eldoc and info ops.

## 0.19.0 (2019-01-01)

### New features

* [#2430](https://github.com/clojure-emacs/cider/issues/2375): `cider-find-var` opens archive files inside [AVFS](http://avf.sourceforge.net) folders if AVFS is detected.
* [#2446](https://github.com/clojure-emacs/cider/issues/2446): Implement Sesman friendly sessions to allow for on-the-fly association with sessions from dependency projects and jars.
* [#2253](https://github.com/clojure-emacs/cider/issues/2253): Split `continue` debug command into "continue till next breakpoint" (`c`) and "continue non stop" (`C`) commands.

### Bug fixes

* [#2474](https://github.com/clojure-emacs/cider/issues/2474): Fix incorrect detection of output and out-of-order printing.
* [#2514](https://github.com/clojure-emacs/cider/issues/2514): Don't auto-jump to warnings when `cider-auto-jump-to-error` is set to 'errors-only.
* [#2453](https://github.com/clojure-emacs/cider/issues/2453): Make it possible to debug deftype methods by direct insertion of #dbg and #break readers into the deftype methods.
* [#1869](https://github.com/clojure-emacs/cider/issues/1869),[cider-nrepl#460](https://github.com/clojure-emacs/cider-nrepl/issues/460): Fix `continue` debugger command which was stopping entering debugger on repeated invocations.
* [#2444](https://github.com/clojure-emacs/cider/issues/2444): Reuse dead REPL buffers on new connections.
* [#2441](https://github.com/clojure-emacs/cider/issues/2441): Make it possible to use `C-c C-x` keys without loading cider first (autoload `cider-start-map`).
* [#2440](https://github.com/clojure-emacs/cider/issues/2440): Make `cider-check-cljs-repl-requirements` take effect again.
* [#2439](https://github.com/clojure-emacs/cider/issues/2439): Remove mentions of `cider-toggle-connection-buffer` from the docs.
* [#2435](https://github.com/clojure-emacs/cider/issues/2435): Remove killed REPLs from sessions in client sentinel.
* Fix jack-in from inside of remote buffers.
* [#2454](https://github.com/clojure-emacs/cider/pull/2454): Fix erratic inspector behavior when multiple REPLs are connected
* [#2467](https://github.com/clojure-emacs/cider/pull/2467): Make generic CIDER ops use any available nREPL connection.
* [#2105](https://github.com/clojure-emacs/cider/issues/2105): Fix no comment syntax defined message when loading buffer after running a failing test.
* [#2115](https://github.com/clojure-emacs/cider/issues/2515): Reset the current buffer after `display-buffer`.

### Changes

* [#2482](https://github.com/clojure-emacs/cider/issues/2482): Don't bind nREPL server started by `cider-jack-in` to `::` (use `localhost` instead).
* [#2484](https://github.com/clojure-emacs/cider/pull/2484): Fix issues where some functionality in REPL buffers (like eldoc) was broken.
* [#2484](https://github.com/clojure-emacs/cider/pull/2484): REPL types are now symbols instead of strings.
* [#1544](https://github.com/clojure-emacs/cider/issues/1544): Add a new defcustom `cider-infer-remote-nrepl-ports` to control whether we use tramp/ssh to infer remote ports.  Now defaulting to `nil` (previously it always tried to infer).

## 0.18.0 (2018-09-02)

### New features

* [#2375](https://github.com/clojure-emacs/cider/issues/2375): Move `cider-eval-toplevel-inside-comment-form` into clojure-mode as `clojure-toplevel-inside-comment-form` so `beginning-of-defun` is aware of comment forms.
* Add new `cider-session-name-template` variable for flexible customization of cider session and REPL buffer names.
* Bind `C-c M-r` to `cider-restart`.
* Add new `cider-start-map` keymap (`C-c C-x`) for jack-in and connection commands.
* Add new `cider-ns-map` keymap (`C-c M-n`) for namespace related functionality.
* Allow evaling top level forms in a comment form rather than the entire comment form with `cider-eval-toplevel-inside-comment-form`.
* Create keymap for inserting forms into the REPL at `C-c C-j`.
* Add new defcustom `cider-invert-insert-eval-p`: Set to cause insert-to-repl commands to eval the forms by default when inserted.
* Add new defcustom `cider-switch-to-repl-after-insert-p`: Set to prevent cursor from going to the REPL when inserting a form in the REPL with the insert-to-repl commands.
* Inject piggieback automatically on `cider-jack-in-clojurescript`.
* Introduce a new command named `cider` (`C-c M-x`) that acts as a simple wrapper around all commands for starting/connecting to REPLs.
* [#2305](https://github.com/clojure-emacs/cider/issues/2305): Make it possible to disable the REPL type auto-detection by customizing `cider-repl-auto-detect-type`.
* [#2373](https://github.com/clojure-emacs/cider/issues/2373): Make it possible to configure the welcome message displayed in scratch buffers via `cider-scratch-initial-message`.
* Add the ability to jump to the profiler buffer using `cider-selector`.
* [#1980](https://github.com/clojure-emacs/cider/issues/1980): Echo back missing namespace name on interactive eval (requires nREPL 0.4.3+).
* [#2397](https://github.com/clojure-emacs/cider/pull/2397): Add shadow-select ClojureScript REPL type.
* [#2314](https://github.com/clojure-emacs/cider/pull/2314): Add `cider-ns-reload` and `cider-ns-reload-all` interactive commands.

### Bugs fixed

* [#2317](https://github.com/clojure-emacs/cider/issues/2317): The stdin prompt can now be cancelled.
* [#2328](https://github.com/clojure-emacs/cider/issues/2328): Added `cider-eval-sexp-to-point`.
* [#2310](https://github.com/clojure-emacs/cider/issues/2310): `cider-format-edn-last-sexp` will format the last sexp.
* [#2294](https://github.com/clojure-emacs/cider/issues/2294): Fix setting default stacktrace filters.
* [#2286](https://github.com/clojure-emacs/cider/issues/2286): Fix eldoc issue with images in the REPL.
* [#2307](https://github.com/clojure-emacs/cider/pull/2307): Use a better error when a cljs REPL form cannot be found.
* Fix the broken test selector functionality.
* [#2291](https://github.com/clojure-emacs/cider/issues/2291): `cider-use-tooltips` custom variable works as expected.
* [#2424](https://github.com/clojure-emacs/cider/issues/2424): Fallback to `lein` as the default jack-in command when `clojure` is not present.

### Changes

* **(Breaking)** Move `cider-repl-set-ns`, previously on `C-c M-n`, on `C-c M-n (M-)n` in the `cider-ns-map`.
* **(Breaking)** Move `cider-ns-refresh`, previously on `C-c C-x`, on `C-c M-n (M-)r` in the `cider-ns-map`.
* **(Breaking)** Bump the minimum required Emacs version to 25.1.
* **(Breaking)** Drop support for Java 7 and Clojure(Script) 1.7.
* **(Breaking)** Use session name as part of CIDER buffers names (REPL, server, messages), and obsolete `nrepl-buffer-name-separator` and `nrepl-buffer-name-show-port`. See `cider-session-name-template` and `cider-format-connection-params` for how to customize CIDER buffer names.
* **(Breaking)** Use a custom task (`cider.tasks/nrepl-server`) for `cider-jack-in` with Boot (that's done to provide access to newer nREPL features to users of older versions of Boot).
* Rename `cider-eval-defun-to-point` to `cider-eval-defun-up-to-point`.
* Add support for printing to the current buffer to `cider-eval-defun-up-to-point`.
* Remove `cider-ping` command.
* Remove `cider-visit-error-buffer` in favour of using `cider-selector`.
* Rename `cider-refresh` to `cider-ns-refresh` (and all the related defcustoms).
* **(Breaking)** Rewrote connection management (see https://docs.cider.mx/cider/usage/managing_connections.html for details).
* **(Breaking)** `cider-jack-in-clojurescript` now creates only a ClojureScript REPL (use `cider-jack-in-clj&cljs` to create both REPLs).
* [#2357](https://github.com/clojure-emacs/cider/issues/2357): Support both keywords and strings as test selectors (previously it was only strings).
* [#2378](https://github.com/clojure-emacs/cider/pull/2378): Add autoloads target to Makefile.
* Map `cider-pprint-eval-last-sexp` to `C-c C-v (C-)f (C-)e` in the `cider-eval-commands-map`.
* Map `cider-pprint-eval-defun-at-point` to `C-c C-v (C-)f (C-)d` in the `cider-eval-commands-map`.
* Accept bare figwheel-main build names (e.g., `dev`). Previously, a keyword (e.g., `:dev`) was required.
* Stop releasing CIDER and cider-nrepl together. cider-nrepl now has its own release cycle and CIDER introduces `cider-required-middleware-version` to track it.

## 0.17.0 (2018-05-07)

### New features

* [#2248](https://github.com/clojure-emacs/cider/pull/2248): `cider-repl` can now display recognized images in the REPL buffer.
* [#2172](https://github.com/clojure-emacs/cider/pull/2172): Render diffs for expected / actual test results.
* [#2167](https://github.com/clojure-emacs/cider/pull/2167): Add new defcustom `cider-jdk-src-paths`. Configure it to connect stack trace links to Java source code.
* [#2161](https://github.com/clojure-emacs/cider/issues/2161): Add new interactive command `cider-eval-defun-to-point` which is bound to `C-c C-v (C-)z`. It evaluates the current top-level form up to the point.
* [#2113](https://github.com/clojure-emacs/cider/issues/2113): Add new interactive commands `cider-eval-last-sexp-in-context` (bound to `C-c C-v (C-)c`) and `cider-eval-sexp-at-point-in-context` (bound to `C-c C-v (C-)b`).
* Add new interactive command `cider-repl-set-type`.
* [#1976](https://github.com/clojure-emacs/cider/issues/1976): Add new interactive command `cider-connect-clojurescript`.
* Add a menu for `cider-browse-ns-mode`.
* [#2160](https://github.com/clojure-emacs/cider/issues/2160): Make it possible to configure the default `*print-level*` and `*print-length*` via defcustoms (`cider-repl-print-level` and `cider-repl-print-length`).
* New interactive command `cider-cheatsheet` allows you to browse the Clojure Cheatsheet with an Emacs interface.
* [#2191](https://github.com/clojure-emacs/cider/issues/2191): Add support for jacking-in just with the `clojure` command-line tool and `tools.deps`.
* Make it possible to start a Nashorn ClojureScript REPL.
* [#2235](https://github.com/clojure-emacs/cider/pull/2235): Make the REPL ignore blank input rather than evaluating.
* [#2241](https://github.com/clojure-emacs/cider/pull/2241): Make `cider-test-ediff` diff eval'ed values.
* Add support for shadow-cljs to `cider-jack-in`.
* [#2244](https://github.com/clojure-emacs/cider/issues/2244): Display the REPL type in the modeline.
* [#2238](https://github.com/clojure-emacs/cider/pull/2238): Allow specifying predicates for entries in `cider-jack-in-lein-plugins` and `cider-jack-in-nrepl-middlewares`.
* Add support for test selectors. If test all or all loaded is called with a prefix ask for filter test selectors in the minibuffer and only run those tests in the project which match the filters. Add variation of test namespace which asks for filter selectors the same way and only runs a subset of the namespace tests.
* Add a configuration variable allowing to control whether server output should be redirected to the REPL (`cider-redirect-server-output-to-repl`).

### Bugs Fixed

* [#1913](https://github.com/clojure-emacs/cider/issues/1913): Fix `cider-toggle-buffer-connection` to allow cycling of connection and restoring all connections in cljc buffers.
* [#2148](https://github.com/clojure-emacs/cider/issues/2148): Fix `jump to definition` working properly when remote `cider-jack-in` and `cider-connect`.
* Font-lock failed assertions even in tests that were evaluated interactively.
* [#2102](https://github.com/clojure-emacs/cider/issues/2102): Make `cider-format-buffer` handle mismatched parens gracefully.

### Changes

* [#2163](https://github.com/clojure-emacs/cider/issues/2163): Add `cider-browse-spec-regex`, and changed `cider-browse-spec-all` to use it.
* [#2029](https://github.com/clojure-emacs/cider/pull/2154): Make cider-doc use cider-browse-spec functionality to print the spec part of the doc buffer
* [#2151](https://github.com/clojure-emacs/cider/pull/2151): Improve formatting of spec in `cider-doc` buffer.
* Remove support for CLJX.
* Fix `cider-eval-region` masking `clojure-refactor-map` in `cider-repl-mode`.
* [#2171](https://github.com/clojure-emacs/cider/issues/2171): Update `See Also` mappings for Clojure 1.9.
* [#2202](https://github.com/clojure-emacs/cider/issues/2202): Make `cider-jack-in-clojurescript` prompt from the ClojureScript REPL type to use.
* [#2202](https://github.com/clojure-emacs/cider/issues/2202): Don't try to start a ClojureScript REPL before checking whether that's possible or not.
* [orchard#24](https://github.com/clojure-emacs/orchard/pull/24): Inspector now separately renders clickable keys and values when inspecting maps.
* [orchard#24](https://github.com/clojure-emacs/orchard/pull/24): Inspector now remembers the current page of each level of nesting when navigating big and nested collection.
* Require piggieback 0.3 or newer.
* Drops support for Rhino in favour of the modern Nashorn.

## 0.16.0 (2017-12-28)

### New Features

* [#2082](https://github.com/clojure-emacs/cider/pull/2082), [cider-nrepl#440](https://github.com/clojure-emacs/cider-nrepl/pull/440): Add specialized stacktraces for clojure.spec assertions.
* [#2111](https://github.com/clojure-emacs/cider/pull/2111): Add `cider-pprint-eval-last-sexp-to-comment` and `cider-pprint-eval-defun-to-comment`.
* Add a REPL shortcut for `cider-repl-require-repl-utils` (this makes it easy to require common functions like `doc`, `source`, etc. in REPL buffers).
* [#2112](https://github.com/clojure-emacs/cider/issues/2112): Add a new interactive command `cider-find-keyword` (bound to `C-c C-:`).
* [#2144](https://github.com/clojure-emacs/cider/issues/2144): Create a Docker image to mimic the Travis CI environment.

### Changes

* `cider-switch-to-last-clojure-buffer` switches to most recent relevant Clojure(Script) buffer instead of the last "remembered" buffer.
* [cider-nrepl#438](https://github.com/clojure-emacs/cider-nrepl/pull/438): Improve startup time by deferring loading CIDER's middleware until the first usage.
* [#2078](https://github.com/clojure-emacs/cider/pull/2078): Improve startup time by bundling together sync requests during startup.
* `cider-rotate-default-connection` will warn if you use it with only a single active connection.
* `cider-format-buffer` tries to preserve the point position.

### Bugs Fixed

* [#2084](https://github.com/clojure-emacs/cider/issues/2084): Select correct REPL type (clj or cljs) in `cider-switch-to-repl-buffer` conditional on the current buffer.
* [#2088](https://github.com/clojure-emacs/cider/issues/2088): Fix functions defined with `def` being font-locked as vars instead of functions.
* [#1651](https://github.com/clojure-emacs/cider/issues/1651), [cider-nrepl#445](https://github.com/clojure-emacs/cider-nrepl/pull/455): Fix `cider-expected-ns` returns `nil` on boot projects.
* [#2120](https://github.com/clojure-emacs/cider/issues/2120): Fix Travis CI build errors for Emacs versions >25.2.
* [#2117](https://github.com/clojure-emacs/cider/pull/2117): Ensure `cider-repl-result-prefix` is only inserted before the first result chunk.
* [#2123](https://github.com/clojure-emacs/cider/issues/2123): Process properly the Java version in Java 9.

## 0.15.1 (2017-09-13)

### New Features

* [#2083](https://github.com/clojure-emacs/cider/pull/2083): New utility function `cider-add-face`.
* [#2083](https://github.com/clojure-emacs/cider/pull/2083): New utility function `cider-run-chained-hook`.
* [#2083](https://github.com/clojure-emacs/cider/pull/2083): New `cider-repl-preoutput-hook` that allows custom output processing.
* [#2083](https://github.com/clojure-emacs/cider/pull/2083): Highlight clojure.spec keywords in REPL (`cider-repl-highlight-spec-keywords` pre-output processor).

### Changes

* [#2045](https://github.com/clojure-emacs/cider/issues/2045) `*cider-scratch*` buffers are no longer automatically killed on connection quit.
* [#2083](https://github.com/clojure-emacs/cider/pull/2083): Jump to other window when clicking on location references in REPL.
* [#2083](https://github.com/clojure-emacs/cider/pull/2083): Improve project namespace highlighting in REPLs.
* [#2083](https://github.com/clojure-emacs/cider/pull/2083): Find locations in more cases when clicking on references in REPL.

### Bugs Fixed

* [#2004](https://github.com/clojure-emacs/cider/issues/2004), [#2039](https://github.com/clojure-emacs/cider/issues/2039), [cider-nrepl#420](https://github.com/clojure-emacs/cider-nrepl/issues/420): Fix namespace issues in instrumentation and debugging commands.
* Project-Only stacktrace filter: hide all other tags when viewing project-only stacktrace.
* Fix interactive evaluation in cljc buffers with only one connection.
* [#2058](https://github.com/clojure-emacs/cider/pull/2058): Don't cache ns-forms in buffers with no such forms.
* [#2057](https://github.com/clojure-emacs/cider/pull/2057): Use `cider--font-lock-ensure` for compatibility with Emacs 24.5.
* [cider-nrepl#436](https://github.com/clojure-emacs/cider-nrepl/pull/436): Ensure that `*print-right-margin*` is not ignored by cider-nrepl middleware.
* [cider-nrepl#435](https://github.com/clojure-emacs/cider-nrepl/pull/435): Allow debugging of forms with `#?(:cljs ... :clj ..)` conditionals.
* [cider-nrepl#432](https://github.com/clojure-emacs/cider-nrepl/pull/432): Ensure `pprint` is after `load-file`.

## 0.15.0 (2017-07-20)

### New Features

* [#2050](https://github.com/clojure-emacs/cider/pull/2050): Use `view-mode` for `cider-grimoire` buffers
* Make stacktraces and other location references in REPL clickable.
* Highlight root namespace in REPL stacktraces.
* Filter stacktrace to just frames from your project.
* [#1918](https://github.com/clojure-emacs/cider/issues/1918): Add new commands `cider-browse-spec` and `cider-browse-spec-all` which start a spec browser.
* [#2015](https://github.com/clojure-emacs/cider/pull/2015): Show symbols as special forms *and* macros in `cider-doc`
* [#2012](https://github.com/clojure-emacs/cider/pull/2012): Support special forms in `cider-apropos` and `cider-grimoire-lookup`.
* [#2007](https://github.com/clojure-emacs/cider/pull/2007): Fontify code blocks from `cider-grimoire` if possible.
* Add support for notifications from the NREPL server.
* [#1990](https://github.com/clojure-emacs/cider/issues/1990): Add new customation variable `cider-save-files-on-cider-refresh` to allow auto-saving buffers when `cider-refresh` is called.
* Add new function `cider-load-all-files`, along with menu bar update.
* Add new customization variable `cider-special-mode-truncate-lines`.
* Add an option `cider-inspector-fill-frame` to control whether the cider inspector window fills its frame.
* [#1893](https://github.com/clojure-emacs/cider/issues/1893): Add negative prefix argument to `cider-refresh` to inhibit invoking of cider-refresh-functions
* [#1776](https://github.com/clojure-emacs/cider/issues/1776): Add new customization variable `cider-test-defining-forms` allowing new test defining forms to be recognized.
* [#1860](https://github.com/clojure-emacs/cider/issues/1860): Add `cider-repl-history` to browse the REPL input history and insert elements from it into the REPL buffer.
* Add new customization variable `cider-font-lock-reader-conditionals` which toggles syntax highlighting of reader conditional expressions based on the buffer connection.
* Add new face `cider-reader-conditional-face` which is used to mark unused reader conditional expressions.
* [#1544](https://github.com/clojure-emacs/cider/issues/1544): Add a new defcustom `nrepl-use-ssh-fallback-for-remote-hosts` to control the behavior of `nrepl-connect` (and in turn that of `cider-connect`) for remote hosts.
* [#1910](https://github.com/clojure-emacs/cider/issues/1910): Add custom company-mode completion style to show fuzzy completions from Compliment.
* Introduce `cider-*-global-options` for customizing options that are not related to tasks.
* [#1731](https://github.com/clojure-emacs/cider/issues/1731): Change code in order to use the new `cider.tasks/add-middleware` boot tasks.
* [#1943](https://github.com/clojure-emacs/cider/pull/1943): Add interactive function to flush Compliment caches.
* [#1726](https://github.com/clojure-emacs/cider/issues/1726): Order keys in printed nrepl message objects.
* [#1832](https://github.com/clojure-emacs/cider/issues/1832): Add new customization variable `cider-eldoc-display-context-dependent-info` to control showing eldoc info for datomic query input parameters.
* Make it possible to disable auto-evaluation of changed ns forms via the defcustom `cider-auto-track-ns-form-changes`.
* [#1991](https://github.com/clojure-emacs/cider/issues/1832): Make it possible to disable the prompt to open a ClojureScript in a browser on connect via `cider-offer-to-open-cljs-app-in-browser`.
* [#1995](https://github.com/clojure-emacs/cider/pull/1995): Add new customization variable `cider-doc-auto-select-buffer` to control cider-doc popup buffer auto selection.
* Ensure that `cider-current-connection` picks the most recently used connection in ambiguous cases.
* Ensure that `cider-switch-to-repl-buffer` picks the most recent REPL buffer if multiple connections are available.
* Add new function `cider-project-connections-types`.

### Changes

* Handle ANSI REPL evaluation created by Puget.
* Drop support for Emacs 24.3.
* Don't try to use ssh automatically when connecting to remote hosts and a direct connection fails. See `nrepl-use-ssh-fallback-for-remote-hosts`.
* [#1945](https://github.com/clojure-emacs/cider/pull/1945): Start nREPL servers bound to `::` by default using `cider-jack-in`.
* Renamed `cider-prompt-save-file-on-load` to `cider-save-file-on-load` and adjust its supported values accordingly (the default now is `'prompt` and `'always-save` is now simply `t`).
* [#2014](https://github.com/clojure-emacs/cider/pull/2014): Unify the format for `forms-str` and `arglists-str`.
* [#2027](https://github.com/clojure-emacs/cider/pull/2027): Mark many custom variables relating to `cider-jack-in` as safe.
* [#2023](https://github.com/clojure-emacs/cider/issues/2023): Make popup-buffer sexp indentation optional.

### Bugs Fixed

* [#2040](https://github.com/clojure-emacs/cider/issues/2040): Fix fontification of conditional expressions in cljc files.
* [#2018](https://github.com/clojure-emacs/cider/issues/2018): Don't delete wrong overlays during code evaluation.
* [#1699](https://github.com/clojure-emacs/cider/issues/1699): Fix "Method code too large!" error that occurred during instrumentation for debugging.
* [#1987](https://github.com/clojure-emacs/cider/issues/1987): Fix: Update faces when disabling a theme
* [#1962](https://github.com/clojure-emacs/cider/issues/1962): Fix performance in fringe overlay placement.
* [#1947](https://github.com/clojure-emacs/cider/issues/1947): Fix error on `cider-jack-in` when `enlighten-mode` is enabled.
* [#1588](https://github.com/clojure-emacs/cider/issues/1588): Redirect `*err*`, `java.lang.System/out`, and `java.lang.System/err` to REPL buffer on all attached sessions.
* [#1707](https://github.com/clojure-emacs/cider/issues/1707): Allow to customize line truncating in CIDER's special buffers.
* [#1876](https://github.com/clojure-emacs/cider/issues/1876): Set pretty-printing width with `cider-repl-pretty-print-width`. If this variable is not set, fall back to `fill-column`.
* [#1875](https://github.com/clojure-emacs/cider/issues/1875): Ensure that loading and evaluation in cljc buffers is performed in both clj and cljs repls.
* [#1897](https://github.com/clojure-emacs/cider/issues/1897): Bind TAB in stacktrace buffers in the terminal.
* [#1895](https://github.com/clojure-emacs/cider/issues/1895): Connect to the same host:port after `cider-restart` if the connection was established with `cider-connect`.
* [#1881](https://github.com/clojure-emacs/cider/issues/1881): Add `cider-cljs-boot-repl` and `cider-cljs-gradle-repl` defcustom and hook `boot-cljs-repl`.
* [#1997](https://github.com/clojure-emacs/cider/pull/1997): Fix a nil error when loading a code buffer and the error buffer is visible.
* [#390](https://github.com/clojure-emacs/cider/issues/390): Workaround for orphaned java process on windows machine after quitting the REPL.

## 0.14.0 (2016-10-13)

### New Features

* [#1825](https://github.com/clojure-emacs/cider/issues/1825): Display test input generated by `test.check`.
* [#1769](https://github.com/clojure-emacs/cider/issues/1769): Display function spec in the doc buffers.
* Add a new interactive command `cider-toggle-request-dispatch`. It allows you to quickly toggle between dynamic and static
request dispatch.
* Add a new interactive command `nrepl-toggle-message-logging`. It allows you to quickly toggle nREPL message logging on and off
within the scope of your current Emacs session.
* [#1851](https://github.com/clojure-emacs/cider/issues/1851): Add a command to rerun the last test ran via `cider-test-run-test`. The new command is named `cider-test-rerun-test` and is about to `C-c C-t (C-)g`.
* [#1748](https://github.com/clojure-emacs/cider/issues/1748): Add new interactive command `cider-pprint-eval-last-sexp-to-repl`.
* [#1789](https://github.com/clojure-emacs/cider/issues/1789): Make it easy to change the connection of the cider-scratch buffer from the mode's menu.
* New interactive command `cider-toggle-buffer-connection`.
* [#1861](https://github.com/clojure-emacs/cider/issues/1861): New interactive commands in message log buffer `nrepl-log-expand-button` and `nrepl-log-expand-all-buttons`.
* [#1872](https://github.com/clojure-emacs/cider/issues/1872): Add new value `display-only` for option `cider-repl-pop-to-buffer-on-connect` that allows for showing the REPL buffer without focusing it.

### Changes

* [#1758](https://github.com/clojure-emacs/cider/issues/1758): Disable nREPL message logging by default due to its negative impact on performance.
* Warn when running `cider-jack-in` without a Clojure project. This behavior is controllable via `cider-allow-jack-in-without-project`.

### Bugs Fixed

* [#1677](https://github.com/clojure-emacs/cider/issues/1677): Interpret `\r` as a newline.
* [#1819](https://github.com/clojure-emacs/cider/issues/1819): Handle properly missing commands on `cider-jack-in`.
* Add option to define exclusions for injected dependencies. Fixes [#1824](https://github.com/clojure-emacs/cider/issues/1824): Can no longer jack-in to an inherited clojure version.
* [#1820](https://github.com/clojure-emacs/cider/issues/1820): Don't try to display eldoc in EDN buffers.
* [#1823](https://github.com/clojure-emacs/cider/issues/1823): Fix column location metadata set by interactive evaluation.
* [#1859](https://github.com/clojure-emacs/cider/issues/1859): Make nREPL message log much faster. `nrepl-dict-max-message-size` custom variable was removed.
* [#1613](https://github.com/clojure-emacs/cider/issues/1859): Check whether a before/after refresh function is resolvable.

## 0.13.0 (2016-07-25)

### New Features

* Add an option `nrepl-prompt-to-kill-server-buffer-on-quit` to control whether killing nREPL server buffer and process requires a confirmation prompt.
* [#1672](https://github.com/clojure-emacs/cider/issues/1672): Allow setting a preferred build tool when multiple are found via `cider-preferred-build-tool`.
* Ensure Clojure version meets minimum supported by CIDER (1.7.0).
* Fringe indicators highlight which sexps have been loaded. Disable it with `cider-use-fringe-indicators`.
* New command: `cider-inspect-last-result`.
* `cider-cljs-lein-repl` now also supports figwheel.
* Option `cider-jack-in-auto-inject-clojure` enables the user to specify a
  version of Clojure for CIDER. This allows the user to override the version
  used in a project, particular if it is lower than minimum required for CIDER.
* Allow the ns displayed by eldoc to be tailored via `cider-eldoc-ns-function`.
* After connecting a ClojureScript REPL, CIDER will try to figure out if it's being served on a port and will offer to open it in a browser.
* [#1720](https://github.com/clojure-emacs/cider/issues/1720): Add a command `cider-eval-sexp-at-point` to evaluate the form around point (bound to `C-c C-v v`).
* [#1564](https://github.com/clojure-emacs/cider/issues/1564): CIDER's internal namespaces and vars are filtered from the ns-browser and apropos functions.
* [#1725](https://github.com/clojure-emacs/cider/issues/1725): Display class names in eldoc for interop forms.
* [#1572](https://github.com/clojure-emacs/cider/issues/1572): Add support for variables in eldoc.
* [#1736](https://github.com/clojure-emacs/cider/issues/1736): Show "See Also" links for functions/variables in documentation buffers.
* [#1767](https://github.com/clojure-emacs/cider/issues/1767): Add a command `cider-read-and-eval-defun-at-point` to insert the defun at point into the minibuffer for evaluation (bound to `C-c C-v .`).
* [#1646](https://github.com/clojure-emacs/cider/issues/1646): Add an option `cider-apropos-actions` to control the list of actions to be applied on the symbol found by an apropos search.
* [#1783](https://github.com/clojure-emacs/cider/issues/1783): Put eval commands onto single map bound to `C-c C-v`.
* [#1804](https://github.com/clojure-emacs/cider/issues/1804): Remember cursor position between `cider-inspector-*` operations.

### Changes

* Simpler keybindings in macroexpand buffer. Expand one step with `m` and all expansions with `a`. Previously was `C-c C-m` and `C-c M-m`.
* Signal an error sooner if the user misconfigured `cider-known-endpoints`.
* `cider-inspect-read-and-inspect` is obsolete. Use `cider-inspect-expression` instead.
* Extremely long overlays are truncated and `cider-inspect-last-result` is recommended.
* Signal `user-error` instead of `error` on jack-in if a project type is not supported.
* Users with `boot.sh` instead of `boot` should customize `cider-boot-command` instead of relying on automatic detection.
* [#1737](https://github.com/clojure-emacs/cider/issues/1737): Show value of locals in debugger tooltip.
* Rebind `cider-eval-last-sexp-and-replace` to `C-c C-v w`.
* Rebind `cider-eval-region` to `C-c C-v r`.
* Rebind `cider-eval-ns-form` to `C-c C-v n`.
* [#1577](https://github.com/clojure-emacs/cider/issues/1577): Show first line of docstring in ns browser.
* `cider-repl-closing-return` (`C-<Return>`) now also completes brackets (`[]`) and curly braces (`{}`) in an expression.

### Bugs fixed

* [#1755](https://github.com/clojure-emacs/cider/issues/1755): Impossible completion for multiple zombie REPL buffers.
* [#1712](https://github.com/clojure-emacs/cider/issues/1712): Bad compilation issue caused when installed along with `nim-mode`.
* Fix arglist display for `def` in the doc buffer.
* Use `cider-apropos-select` instead of `cider-apropos` in `cider-apropos-documentation-select`.
* [#1561](https://github.com/clojure-emacs/cider/issues/1561): Use an appropriate font-lock-face for variables, macros and functions in
the ns-browser.
* [#1708](https://github.com/clojure-emacs/cider/issues/1708): Fix `cider-popup-buffer-display` when another frame is used for the error buffer.
* [#1733](https://github.com/clojure-emacs/cider/pull/1733): Better error handling when no boot command is found in `exec-path`.
* Fix orphaned nrepl-messages buffer after `cider-quit`.
* [#1782](https://github.com/clojure-emacs/cider/issues/1782): Disable mouse-over tooltips when `help-at-pt-display-when-idle` is non-nil.
* [#1811](https://github.com/clojure-emacs/cider/issues/1811): Handle properly jack-in commands with spaces in them.

## 0.12.0 (2016-04-16)

### New Features

* Option `cider-use-tooltips` controls the display of mouse-over tooltips.
* `f` key reruns failed tests on the test-report buffer.
* `g` key reruns test at point on the test-report buffer.
* Debugger now supports step-in.
* Improve CIDER's menu-bar menu:
  - Thoroughly reorganize it and split it into 3 separate menus;
  - Add custom-written `:help` strings to some items, and automatically add help strings to the rest;
  - Add a few commands;
  - Grey-out commands that rely on connections while there is no connection.
* Var docstrings are automatically displayed in mouse-over tooltips.
* [#1636](https://github.com/clojure-emacs/cider/pull/1636): New minor-mode `cider-auto-test-mode` for test-driven-development. When activated, tests are rerun after every load-file.
* Javadoc commands take into account the variable `clojure.java.javadoc/*remote-javadocs*`.
* Javadoc also works on classes of the AmazonAWS Java SDK.
* Apropos commands now accept lists of space-separated words as arguments, in addition to regular expressions (similar to Emacs's own apropos commands).
* [#1541](https://github.com/clojure-emacs/cider/issues/1541): New commands `cider-apropos-select` (bound to `C-c C-d C-s`) and `cider-apropos-documentation-select` (bound to `C-c C-d c-e`).
* New function `cider-expected-ns` is like `clojure-expected-ns`, but uses classpath for better results.  See [clojure-mode#372](https://github.com/clojure-emacs/clojure-mode/issues/372).
* A double prefix argument (`C-u C-u`) for `cider-eval-defun-at-point` debugs the sexp at point instead of the entire defun, and offers to create a conditional breakpoint.
* New command `cider-load-all-project-ns` allows you to load all project namespaces.
* Display eldoc for keywords used to get map keys.
* Display eldoc for `Classname.`.
* Display namespace in eldoc.
* [cider-nrepl#313](https://github.com/clojure-emacs/cider-nrepl/issues/313): Selectively suppress user-specified categories of middleware errors from foregrounding stacktrace buffers via the `cider-stacktrace-suppressed-errors` variable.

### Changes

* Doc buffer splits arglists into several lines.
* Changed the face of the words “Macro” and “Special form” in the doc buffer to be easier to see.
* Display multi-line eval overlays at the start of the following line. It looked weird that these overlays started on the middle of a line, but then folded onto the start of following lines.
* [#1627](https://github.com/clojure-emacs/cider/issues/1627): Align the terminology used by `cider-test` with the one used by lein and boot (use the terms `assertion` and `test`).
* Remove the warning about missing nREPl ops.
* [#1420](https://github.com/clojure-emacs/cider/issues/1420): Show stacktrace buffers for sync requests errors.

### Bugs fixed

* [cider-nrepl#329](https://github.com/clojure-emacs/cider-nrepl/pull/329): Fix error instrumenting functions that call clojure.tools.logging.
* [#1643](https://github.cim/clojure-emacs/cider/issues/1643): Running tests no longer deletes unrelated overlays.
* [#1632](https://github.com/clojure-emacs/cider/pull/1632): Redefining a function correctly updates eldoc.
* [#1630](https://github.com/clojure-emacs/cider/pull/1630): The debugger no longer gets confused inside `@` redefs.
* [#1599](https://github.com/clojure-emacs/cider/pull/1599): Don't error when test makes 0 assertions.
* [#1563](https://github.com/clojure-emacs/cider/issues/1563): Handle invalid regular expressions in apropos.
* [#1625](https://github.com/clojure-emacs/cider/issues/1625): Display a more meaningful message when running
an individual test using `C-c C-t t`.
* Fix buffer closing in `cider-close-ancillary-buffers`.
* Dynamic font-locking is also refreshed when a file's namespace depends on a namespace that was changed, so the traced-face should be immediately updated even on functions from another namespace.
* [#1656](https://github.com/clojure-emacs/cider/issues/1656): Apply ansi colors to output when doing eval and print.

## 0.11.0 (2016-03-03)

### New features

* [#1545](https://github.com/clojure-emacs/cider/pull/1545): New feature: Enlighten. See the new Readme section for more information.
* [#1169](https://github.com/clojure-emacs/cider/pull/1169): New command `cider-eval-defun-to-comment`.
* Change default value of `cider-overlays-use-font-lock` to `t`. Unlike before, a value of `t`, causes `cider-result-overlay-face` is to be prepended to the font-lock faces (instead of just not being used).
* `cider-result-overlay-face` default value changed to a background and a box, so it can be prepended to other faces without overriding the foreground.
* [#1518](https://github.com/clojure-emacs/cider/pull/1518): Add `cider-dynamic-indentation` defcustom, to disable dynamic indent functionality.
* Font-lock traced vars.
* New defcustom, `cider-pprint-fn`, allows you to set the function to use when pretty-printing evaluation results.
* [#1432](https://github.com/clojure-emacs/cider/issues/1432): Show explicit error messages when invoking commands with no ClojureScript support.
* [#1463](https://github.com/clojure-emacs/cider/issues/1463): Assume that `cider-connect` is invoked from within a project,
and try to associate the created connection with this project automatically.
* Typing `s` in a debug session shows the current stack.
* Typing `h` (as in *h*ere) skips all sexps until the current point position.
* [#1507](https://github.com/clojure-emacs/cider/issues/1507): Add the ability to control the REPL's scroll on output functionality via `cider-repl-scroll-on-output`.
* [#1543](https://github.com/clojure-emacs/cider/issues/1543): Add some getting started instructions to the welcome banner.
* New command `cider-drink-a-sip`. Use in case you're thirsty for knowledge.
* Make the connection message configurable via `cider-connection-message-fn`. This means now you can have any function (e.g. `cider-random-tip`) provide the second part of the message.
* New command `cider-repl-clear-banners`.
* New command `cider-repl-clear-help-banner`.

### Changes

* [#1531](https://github.com/clojure-emacs/cider/issues/1531) `cider-jack-in` now injects its own dependencies using CLI. Both leiningen and boot are supported. Set `cider-inject-dependencies-at-jack-in` to nil to opt out. Extension point for other tools to inject their own dependencies is `cider-add-repl-dependencies`.
* `cider-inspect` now operates by default on the last sexp. Its behavior can be altered via prefix arguments.
* Requires Clojure(Script) 1.7 or newer.
* Requires Java 7 or newer.
* Improve stacktrace presentation of compiler errors (readability, DWIM point positioning).
* [#1458](https://github.com/clojure-emacs/cider/issues/1458): Separate nREPL messages by connections instead of by sessions.
* [#1226](https://github.com/clojure-emacs/cider/issues/1226): Enable running of all loaded and all project tests.
* Give test commands their own keybinding prefix (`C-c C-t`). Use both single-key and
  `Control` + letter mnemonics for these commands (as for the documentation
  commands).
* `cider-test` commands now have keybindings in `cider-repl-mode`. The keybindings are exactly the same as those in `cider-mode`.
* Changed the binding of `cider-apropos-documentation` to `C-c C-d f` and `C-c C-d C-f` (it was `C-c C-d A`).
* [#1584](https://github.com/clojure-emacs/cider/issues/1584): Don't enable `eldoc-mode` automatically in `cider-repl-mode`.
* [#1585](https://github.com/clojure-emacs/cider/issues/1585): Show the eval command in the debugger's prompt.

### Bugs fixed

* [#1578](https://github.com/clojure-emacs/cider/issues/1578): nrepl-server-filter called with dead process buffer in Windows.
* [#1441](https://github.com/clojure-emacs/cider/issues/1441): Don't popup a buffer that's already displayed.
* [#1557](https://github.com/clojure-emacs/cider/issues/1557): When a sibling REPL is started by hasn't yet turned into a cljs REPL, it won't hijack clj requests.
* [#1562](https://github.com/clojure-emacs/cider/issues/1562): Actually disable cider-mode when it gets disabled.
* [#1540](https://github.com/clojure-emacs/cider/issues/1540): Fix cider-complete-at-point.
* [cider-nrepl#294](https://github.com/clojure-emacs/cider-nrepl/issues/294): Handle errors in the `complete-doc` nREPL op.
* [#1493](https://github.com/clojure-emacs/cider/issues/1493): Support special forms in eldoc.
* [#1529](https://github.com/clojure-emacs/cider/issues/1529): Close nREPL message buffer when you quit its matching connection.
* [#707](https://github.com/clojure-emacs/cider/issues/707): Better support clojure.test/with-test.
* Fix namespace navigation in the namespace browser.
* [#1565](https://github.com/clojure-emacs/cider/issues/1565): Fix font-locking in apropos buffers.
* [#1570](https://github.com/clojure-emacs/cider/issues/1570): Handle properly rest params in eldoc.

## 0.10.2 (2016-01-27)

### Changes

* `cider-current-connection` actually, really considers major mode before `cider-repl-type`.

### Bugs fixed

* [#1521](https://github.com/clojure-emacs/cider/pull/1521): Don't assume the REPL buffer is in the current frame in `cider-repl--show-maximum-output`.

## 0.10.1 (2016-01-05)

### Changes

* Suppress eldoc when the current sexp seems to be too large.
* [#1500](https://github.com/clojure-emacs/cider/pull/1500): Improve the performance of REPL buffers by using text properties instead of overlays for ANSI coloring.
* `cider-current-connection` considers major mode before `cider-repl-type`.

### Bugs fixed

* [#1450](https://github.com/clojure-emacs/cider/pull/1450): Fix an error in `cider-restart` caused by a reference to a killed buffer.
* [#1456](https://github.com/clojure-emacs/cider/issues/1456): Don't font-lock buffer if font-lock-mode is OFF.
* [#1459](https://github.com/clojure-emacs/cider/issues/1459): Add support for dynamic dispatch in scratch buffers.
* [#1466](https://github.com/clojure-emacs/cider/issues/1466): Correctly font-lock pretty-printed results in the REPL.
* [#1475](https://github.com/clojure-emacs/cider/pull/1475): Fix `args-out-of-range` error in `cider--get-symbol-indent`.
* [#1479](https://github.com/clojure-emacs/cider/pull/1479): Make paredit and `cider-repl-mode` play nice.
* [#1452](https://github.com/clojure-emacs/cider/issues/1452): Fix wrong ANSI coloring in the REPL buffer.
* [#1486](https://github.com/clojure-emacs/cider/issues/1486): Complete a partial fix in stacktrace font-locking.
* [#1482](https://github.com/clojure-emacs/cider/issues/1482): Clear nREPL sessions when a connection is closed.
* [#1435](https://github.com/clojure-emacs/cider/issues/1435): Improve error display in cider-test.
* [#1379](https://github.com/clojure-emacs/cider/issues/1379): Fix test highlighting at start of line.
* [#1490](https://github.com/clojure-emacs/cider/issues/1490): Don't display the inspector buffer when evaluation fails.

## 0.10.0 (2015-12-03)

### New features

* [#1406](https://github.com/clojure-emacs/cider/issues/1406): When running tests, report test ns in minibuffer messages.
* [#1402](https://github.com/clojure-emacs/cider/pull/1402): When tests pass after previously failing, update the test-report buffer to show success.
* [#1373](https://github.com/clojure-emacs/cider/issues/1373): Add gradle support for `cider-jack-in`.
* Indentation of macros (and functions) [can be specified](https://docs.cider.mx/cider/config/indentation.html#_dynamic_indentation) in the var's metadata, via [indent specs](https://docs.cider.mx/cider/indent_spec.html).
* [Abbreviated printing](https://github.com/clojure-emacs/cider-nrepl/pull/268) for functions multimethods. Instead of seeing `#object[clojure.core$_PLUS_ 0x4e648e99 "clojure.core$_PLUS_@4e648e99"]` you'll see `#function[clojure.core/+]`.
* [#1376](https://github.com/clojure-emacs/cider/pull/1376): Anything printed to `*out*` outside an eval scope is also forwarded to all nREPL sessions connected from CIDER. Normally it would only be sent to the server's `out`.
* [#1371](https://github.com/clojure-emacs/cider/issues/1371): Font-lock deprecated vars with a background color.
* [#1232](https://github.com/clojure-emacs/cider/pull/1232): Add `cider-load-buffer-and-switch-to-repl-buffer`.
* [#1325](https://github.com/clojure-emacs/cider/issues/1325): Jump to error location when clicking on the error message in the stack-trace pop-up.
* [#1301](https://github.com/clojure-emacs/cider/issues/1301): CIDER can do dynamic font-locking of defined variables, functions, and macros. This is controlled by the `cider-font-lock-dynamically` custom option.
* [#1271](https://github.com/clojure-emacs/cider/issues/1271): New possible value (`always-save`) for `cider-prompt-save-file-on-load`.
* [#1197](https://github.com/clojure-emacs/cider/issues/1197): Display some indication that we're waiting for a result for long-running evaluations.
* [#1127](https://github.com/clojure-emacs/cider/issues/1127): Make it possible to associate a buffer with a connection (via `cider-assoc-buffer-with-connection`).
* [#1217](https://github.com/clojure-emacs/cider/issues/1217): Add new command `cider-assoc-project-with-connection` to associate a project directory with a connection.
* [#1248](https://github.com/clojure-emacs/cider/pull/1248): Add <kbd>TAB</kbd> and <kbd>RET</kbd> keys to the test-report buffer.
* [#1245](https://github.com/clojure-emacs/cider/pull/1245): New variable, `cider-overlays-use-font-lock` controls whether results overlay should be font-locked or just use a single face.
* [#1235](https://github.com/clojure-emacs/cider/pull/1235): Add support for syntax-quoted forms to the debugger.
* [#1212](https://github.com/clojure-emacs/cider/pull/1212): Add pagination of long collections to inspector.
* [#1237](https://github.com/clojure-emacs/cider/pull/1237): Add two functions for use with `cider-repl-prompt-function`, `cider-repl-prompt-lastname` and `repl-prompt-abbreviated`.
* [#1201](https://github.com/clojure-emacs/cider/pull/1201): Integrate overlays with interactive evaluation. `cider-use-overlays` can be used to turn this on or off.
* [#1195](https://github.com/clojure-emacs/cider/pull/1195): CIDER can [create cljs REPLs](https://github.com/clojure-emacs/cider#clojurescript-usage).
* [#1191](https://github.com/clojure-emacs/cider/pull/1191): New custom variables `cider-debug-print-level` and `cider-debug-print-length`.
* [#1188](https://github.com/clojure-emacs/cider/pull/1188): New debugging tool-bar.
* [#1187](https://github.com/clojure-emacs/cider/pull/1187): The list of keys displayed by the debugger can be configured with `cider-debug-prompt`.
* [#1187](https://github.com/clojure-emacs/cider/pull/1187): While debugging, there is a menu on the menu-bar listing available commands.
* [#1184](https://github.com/clojure-emacs/cider/pull/1184): When the user kills the REPL buffer, CIDER will offer to kill the nrepl buffer and process too. Also, when the client (repl) process dies, the server (nrepl) process is killed too.
* [#1182](https://github.com/clojure-emacs/cider/pull/1182): New command `cider-browse-instrumented-defs`, displays a buffer listing all definitions currently instrumented by the debugger.
* [#1182](https://github.com/clojure-emacs/cider/pull/1182): Definitions currently instrumented by the debugger are marked with a red box in the source buffer.
* [#1174](https://github.com/clojure-emacs/cider/pull/1174): New command `cider-run`, runs the project's `-main` function.
* [#1176](https://github.com/clojure-emacs/cider/pull/1176): While debugging, cider's usual eval commands will evaluate code in the current lexical context. Additionally, the <kbd>l</kbd> key now inspects local variables.
* [#1149](https://github.com/clojure-emacs/cider/pull/1149): [Two new ways](https://github.com/clojure-emacs/cider#debugging) to debug code, the `#break` and `#dbg` reader macros.
* [#1219](https://github.com/clojure-emacs/cider/pull/1219): The output of `cider-refresh` is now sent to a dedicated `*cider-refresh-log*` buffer.
* [#1219](https://github.com/clojure-emacs/cider/pull/1219): New custom variables `cider-refresh-before-fn` and `cider-refresh-after-fn`.
* [#1220](https://github.com/clojure-emacs/cider/issues/1220): Treat keywords as symbols in lookup commands like `cider-find-var`.
* [#1241](https://github.com/clojure-emacs/cider/pull/1241): Passing a double prefix argument to `cider-refresh` will now clear the state of the namespace tracker used by the refresh middleware. This is useful for recovering from errors that a normal reload would not otherwise recover from, but may cause stale code in any deleted files to not be completely unloaded.
* New defcustom `cider-result-use-clojure-font-lock` allows you disable the use of Clojure font-locking for interactive results.
* [#1239](https://github.com/clojure-emacs/cider/issues/1239): New defcustom `cider-refresh-show-log-buffer`, controls the behavior of the `*cider-refresh-log*` buffer when calling `cider-refresh`. When set to nil (the default), the log buffer will still be written to, but not displayed automatically. Instead, the most relevant information will be displayed in the echo area. When set to non-nil, the log buffer will be displayed every time `cider-refresh` is called.
* [#1328](https://github.com/clojure-emacs/cider/issues/1328): Auto-scroll the `*nrepl-server*` buffer on new output.
* [#1300](https://github.com/clojure-emacs/cider/issues/1300): Add the ability to replicate an existing connection with `cider-replicate-connection`.
* [#1330](https://github.com/clojure-emacs/cider/issues/1330): Leverage nREPL 0.2.11's source-tracking feature.
* [#1392](https://github.com/clojure-emacs/cider/issues/1392): Track definitions made in the REPL.
* [#1337](https://github.com/clojure-emacs/cider/issues/1337): Added a command to switch between the Clojure and ClojureScript REPLs in the same project (bound to <kbd>C-c M-o</kbd> in `cider-repl-mode`).

### Changes

* [#1299](https://github.com/clojure-emacs/cider/issues/1299) <kbd>C-c C-k</kbd> and <kbd> C-c C-l</kbd> now dispatch to both the Clojure and ClojureScript REPL (in the same project) when called from a `.cljc` or `.cljx` file.
* [#1397](https://github.com/clojure-emacs/cider/issues/1297) <kbd>C-c M-n</kbd> now changes the ns of both the Clojure and ClojureScript REPL (in the same project) when called from a cljc or cljx file.
* [#1348](https://github.com/clojure-emacs/cider/issues/1348): Drop the dash dependency.
* The usage of the default connection has been reduced significantly. Now evaluations & related commands will be routed via the connection matching the current project automatically unless there's some ambiguity when determining the connection (like multiple or no matching connections). Simply put you'll no longer have to mess around much with connecting-setting commands (e.g. `nrepl-connection-browser`, `cider-rotate-default-connection`).
* [#732](https://github.com/clojure-emacs/cider/issues/732): `cider-quit` and `cider-restart` now operate on the current connection only. With a prefix argument they operate on all connections.
* `nrepl-log-messages` is now set to `t` by default.
* Renamed `cider-repl-output-face` to `cider-repl-stdout-face` and `cider-repl-err-output-face` to `cider-repl-stderr-face`.
* Clearing the REPL buffer is now bound to `C-u C-C C-o`.
* [#1422](https://github.com/clojure-emacs/cider/issues/1422): Don't display mismatching parens error on incomplete expressions in REPL buffers.
* [#1412](https://github.com/clojure-emacs/cider/issues/1412): nREPL messages for separate sessions are tracked in separate buffers.
* Removed `cider-switch-to-repl-command`.
* Renamed `cider-default-repl-command` to `cider-jack-in-default`.

### Bugs fixed

* [#1384](https://github.com/clojure-emacs/cider/pull/1384): Match windows file names in `cider-compilation-regexp`.
* [#1252](https://github.com/clojure-emacs/cider/issues/1252) `cider-repl-clear-buffer` stops working in certain circumstances.
* [#1164](https://github.com/clojure-emacs/cider/pull/1164): Fix an error in `cider-browse-ns--doc-at-point`.
* [#1189](https://github.com/clojure-emacs/cider/issues/1189): Don't show result from automatic ns form evaluation.
* [#1079](https://github.com/clojure-emacs/cider/issues/1079): Don't try to font-lock very long results. The maximum font-lockable result length is controlled by `cider-font-lock-max-length`.

## 0.9.1 (2015-06-24)

### New features

* [#1155](https://github.com/clojure-emacs/cider/pull/1155): The debugger displays overlays highlighting the current sexp and its return value.

### Bugs fixed

* [#1142](https://github.com/clojure-emacs/cider/issues/1142): Don't retrieve nrepl ports when `cider-known-endpoints` entry already contains the port.
* [#1153](https://github.com/clojure-emacs/cider/pull/1153): Fix behavior of `cider-switch-to-current-repl-buffer`.
* [#1139](https://github.com/clojure-emacs/cider/issues/1139): Fix evaluation of ns forms and of forms with unevaluated namespaces.
* Replace `assert` with `cl-assert` (we don't use anything from `cl` now).
* [#1135](https://github.com/clojure-emacs/cider/pull/1135): Fix a corner case with locals display in the debugger.
* [#1129](https://github.com/clojure-emacs/cider/issues/1129): Fix occasional `(wrong-type-argument stringp nil)` on clojure-android.
* [#1122](https://github.com/clojure-emacs/cider/issues/1122): Run client initialization in new client buffer.
* [#1143](https://github.com/clojure-emacs/cider/issues/1143): Handle tests without location metadata.

## 0.9.0 (2015-06-16)

### New features

* [#1109](https://github.com/clojure-emacs/cider/issues/1109): New defcustom `cider-auto-mode`.
On by default, when `nil` don't automatically enable `cider-mode` in all Clojure buffers.
* [#1061](https://github.com/clojure-emacs/cider/issues/1061): New command `cider-find-ns`, bound to <kbd>C-c C-.</kbd>, which prompts for an ns and jumps to the corresponding source file.
* [#1019](https://github.com/clojure-emacs/cider/pull/1019): New file, cider-debug.el.
  Provides a new command, `cider-debug-defun-at-point`, bound to <kbd>C-u C-M-x</kbd>.
  Interactively debug top-level clojure forms.
* New defcustom, `cider-auto-select-test-report-buffer` (boolean).
  Controls whether the test report buffer is selected after running a test. Defaults to true.
* Trigger Grimoire doc lookup from doc buffers by pressing <kbd>g</kbd> (in Emacs) and <kbd>G</kbd> (in browser).
* [#903](https://github.com/clojure-emacs/cider/pull/903): Isolate
  `nrepl-client` connection logic from CIDER. New hooks `cider-connected-hook`
  and `cider-disconnected-hook`.
* [#920](https://github.com/clojure-emacs/cider/issues/920): Support `cider-jack-in` for boot-based projects.
* [#949](https://github.com/clojure-emacs/cider/issues/949): New custom var: `cider-default-repl-command`.
* New code formatting commands - `cider-format-buffer`, `cider-format-region` and `cider-format-defun`.
* New data formatting commands - `cider-format-edn-buffer` and `cider-format-edn-region`.
* New insert region in REPL command - `cider-insert-region-in-repl`.
* Pretty printing functionality moved to middleware, adding support for ClojureScript.
  - New command to eval and pprint result: `cider-interactive-pprint-eval`.
  - `cider-format-pprint-eval` has been removed.
* Warn when used with incompatible nREPL server.
* Allow the prompt to be tailored by adding `cider-repl-prompt-function` and `cider-repl-default-prompt`.
* Support for middleware-annotated completion candidates.
  - `cider-annotate-completion-function` controls how the annotations are formatted.
  - `cider-completion-annotations-alist` controls the abbreviations used in annotations.
  - `cider-completion-annotations-include-ns` controls when to include the candidate namespace in annotations.
* Inspector middleware now relies on `eval` middleware, adding support for ClojureScript.
* Better printing of large amounts of exception cause data in the error buffer.
  - New defcustom, `cider-stacktrace-print-length` (boolean).
* [#958](https://github.com/clojure-emacs/cider/pull/958): Reuse existing repl
  buffers with dead processes. Users are now informed about existing zombie repl
  buffers and are offered the choice to reuse those for new connections.
* New defcustom, `cider-prompt-for-symbol`. Controls whether to prompt for
  symbol when interactive commands require one. Defaults to t, which always
  prompts. Currently applies to all documentation and source lookup commands.
* [#1032](https://github.com/clojure-emacs/cider/issues/1032): New functions, `cider-find-dwim` and
  `cider-find-dwim-other-window`. These functions combine the functionality of `cider-jump-to-var` and
  `cider-jump-to-resource`. Which are now renamed to `cider-find-var` and `cider-find-resource` respectively.
* [#1014](https://github.com/clojure-emacs/cider/issues/1014): A prefix of <kbd>-</kbd> causes `cider-find-var` and
  `cider-find-resource` to show results in other window. Additionally, a double prefix argument <kbd>C-u C-u</kbd>
  inverts the meaning of `cider-prompt-for-symbol` and shows the results in other window.
* [#1062](https://github.com/clojure-emacs/cider/issues/1062): Added completion candidates to `cider-find-resource`.
* Middleware support for Piggieback 0.2.x.
* In the namespace browser, `d` and `s` are now bound to show the documentation
  or the source respectively for the symbol at point.
* [#1090](https://github.com/clojure-emacs/cider/issues/1090): New defcustom,
  `cider-macroexpansion-print-metadata` (boolean). Controls whether metadata of
  forms is included in macroexpansion results. Defaults to nil.

### Changes

* Display the current connection instead of the current namespace in `cider-mode`'s modeline.
* [#1078](https://github.com/clojure-emacs/cider/issues/1078): Removed
  `cider-load-fn-into-repl-buffer`, previously bound to `C-c M-f` in the REPL.
* [#1019](https://github.com/clojure-emacs/cider/pull/1019):
  <kbd>C-u C-M-x</kbd> no longer does `eval-defun`+print-result. Instead it debugs the form at point.
* [#854](https://github.com/clojure-emacs/cider/pull/854): Error navigation now
  favors line information reported by the stacktrace, being more detailed than
  the info reported by `info` middleware.
* [#854](https://github.com/clojure-emacs/cider/pull/854): Add `nrepl-dict` constructor.
* [#934](https://github.com/clojure-emacs/cider/issues/934): Remove
  `cider-turn-on-eldoc-mode` in favor of simply using `eldoc-mode`.
* [#953](https://github.com/clojure-emacs/cider/pull/953): Use `sshx` instead of `ssh` in `cider-select-endpoint`.
* [#956](https://github.com/clojure-emacs/cider/pull/956): Eval full ns form only when needed.
* Enable annotated completion candidates by default.
* [#1031](https://github.com/clojure-emacs/cider/pull/1031): Interactive functions prompt with
  symbol at point as a default value.
* Remapped `cider-grimoire` to <kbd>C-c C-d r</kbd> & <kbd>C-c C-d C-r</kbd>
to avoid conflicts with <kbd>C-g</kbd>.
* [#1088](https://github.com/clojure-emacs/cider/issues/1088): Kill the
source-tracking evaluation hack as it wasn't compatible with ClojureScript.
* Removed `clojure-enable-cider` and `clojure-disable-cider`.

### Bugs fixed

* [#921](https://github.com/clojure-emacs/cider/issues/921): Fixed
non-functioning `cider-test-jump` from test reports.
* [#962](https://github.com/clojure-emacs/cider/pull/962): On error don't auto-jump to tooling files.
* [#909](https://github.com/clojure-emacs/cider/issues/909): Fixed
`cider-repl-set-ns`'s behavior for ClojureScript.
* [#950](https://github.com/clojure-emacs/cider/issues/950): Eval `ns` form in the
`user` namespace when using `cider-interactive-eval`.
* [#954](https://github.com/clojure-emacs/cider/issues/954): Detect properly a project's root
when in buffer that's not visiting a file (e.g. a REPL buffer).
* [#977](https://github.com/clojure-emacs/cider/issues/977): `cider-format-region` now respects indentation of the region start position.
* [#979](https://github.com/clojure-emacs/cider/issues/979): Fixed the inspector buffer popping up needlessly.
* [#981](https://github.com/clojure-emacs/cider/issues/981): Updated `cider-find-file` to use `find-buffer-visiting` instead of `get-file-buffer`.
* [#1004](https://github.com/clojure-emacs/cider/issues/1004): `:repl-env` key is now filtered from exception causes, as it contains unprintably large strings of compiled javascript when using ClojureScript.
* Tunneled ssh connection now deals correctly with the ssh password request.
* [#1033](https://github.com/clojure-emacs/cider/issues/1033): Removed erroneous underlining from stacktrace frames and disabled frame filters in the error buffer.
* The error buffer no longer pops up when there is no error to display.
* `cider-find-resource` now correctly throws an error when no path is provided.
* [#946](https://github.com/clojure-emacs/cider/issues/946): `cider-stacktrace-mode` is now enabled before the error buffer is displayed.
* [#1077](https://github.com/clojure-emacs/cider/issues/1077): Respect `cider-repl-display-in-current-window` in `cider-switch-to-last-clojure-buffer`.

## 0.8.2 (2014-12-21)

### Bugs fixed

* [#867](https://github.com/clojure-emacs/cider/issues/867): Update Grimoire URL to fix (cider-grimoire-lookup) regression due to HTTP 301 (Moved Permanently).
* [#883](https://github.com/clojure-emacs/cider/issues/883): Encode properly the javadoc url.
* [#824](https://github.com/clojure-emacs/cider/issues/824): Fix REPL font-locking.
* [#888](https://github.com/clojure-emacs/cider/issues/888): Handle comments in `cider-repl-mode`.
* [#830](https://github.com/clojure-emacs/cider/issues/830): Stop using `load-file` for most interactive evaluation commands.
* [#885](https://github.com/clojure-emacs/cider/issues/885): Translate nREPL-delivered map keys to symbols before adding as text properties.
* Fix tab completion in `cider-read-from-minibuffer`.
* [#894](https://github.com/clojure-emacs/cider/issues/894): Make it possible to enter any symbol with `cider-read-symbol-name`.
* Report Clojure's version including its qualifier (e.g. `alpha4`) if present.
* Use the `field` text property to make move-beginning-of-line respect the REPL prompt instead of writing our own beginning-of-line commands.

## 0.8.1 (2014-11-20)

### Bugs fixed

* Fixed version mismatch warning on CIDER startup (the actual bug was in `cider-nrepl`).

## 0.8.0 (2014-11-20)

### New features

* `cider-auto-jump-to-error` accepts new option `'errors-only`
* `cider-connect` now asks for remote hosts defined in machine-wide `ssh`
  configuration files and automatically detects running instances of lein
  server, both on local and remote machines.
* New defcustom `cider-stacktrace-print-level`.  Controls the `*print-level*` used when
  pretty printing an exception cause's data.  Defaults to 50.
* New interactive command `cider-undef`.
* New interactive command `cider-clear-compilation-highlights`.
* First pass at a CIDER quick reference card.
* `completion-at-point` now annotates functions, macros and special forms, thus making it
simpler to gain understanding of what you're using (disabled by default).
* When invoked with a prefix argument `cider-quit` doesn't ask for confirmation.
* Enhance stacktrace to definition navigation to work for interactively defined vars.
* New vars: `cider-to-nrepl-filename-function` and `cider-from-nrepl-filename-function`
are used to translate filenames from/to the nREPL server (default Cygwin implementation provided).
* Java classpath browser (`M-x cider-classpath`).
* Clojure namespace browser (`M-x cider-browse-ns` and `M-x cider-browse-ns-all`).
* Added the ability to jump to a definition from a docview buffer.
* New interactive command `cider-close-nrepl-session`.
* New interactive command `cider-describe-nrepl-session`.
* New interactive command `cider-toggle-trace-ns` (mapped to <kbd>C-c M-t n</kbd>)
* New interactive command `cider-repl-require-repl-utils`.
* [#784](https://github.com/clojure-emacs/cider/issues/784): Make it possible to run tests in
the current ns with `C-u C-c ,`.

### Changes

* bencode decoder was rewritten:
  - nREPL dicts are now plists and accessor api is given by `nrepl-dict-p`,
    `nrepl-dict-get` and `nrepl-dict-put`.
  - nested stack is used for decoded messages to avoid re-parsing of incomplete messages
  - queues are used for incoming strings from the server and for the decoded responses
* REPL buffers are now connection buffers for REPL client connections.
* Server and client cranking were isolated into `nrepl-start-server-process` and
  `nrepl-start-client-process`.

* nrepl-client.el refactoring:

  - `nrepl-send-request-sync` was renamed into `nrepl-send-sync-request` to comply
  -  with the names of other 'sync' variables.

  - nREPL requests are now named with `nrepl-request:OP` where "OP" stands for
    the type of the request (eval, clone etc.). The following functions
    were renamed:

       `nrepl-send-string` -> `nrepl-request:eval`
       `nrepl-send-string-sync` -> `nrepl-sync-request:eval`
       `nrepl-send-interrupt` -> `nrepl-request:interrupt`
       `nrepl-send-stdin` -> `nrepl-request:stdin`
       `nrepl-describe-session` -> `nrepl-request:describe`
       `nrepl-create-client-session` -> `nrepl-request:clone`

* Renamed `cider-macroexpansion-suppress-namespaces` to `cider-macroexpansion-display-namespaces`.
* [#652](https://github.com/clojure-emacs/cider/issues/652): Suppress eldoc when
an error message is displayed in the minibuffer.
* [#719](https://github.com/clojure-emacs/cider/issues/719) The customization
variable `cider-test-show-report-on-success` controls now, whether to show the
`*cider-test-report*` buffer on passing tests. The default is to not show the
buffer.
* Renamed `cider-toggle-trace` to `cider-toggle-trace-var` and remapped it to <kbd>C-c M-t v</kbd>.

### Bugs fixed

* [#705](https://github.com/clojure-emacs/cider/pull/705): Fixed macroexpansion
bug for `tidy` namespace display.
* Font-lock properly error messages in the REPL resulting from interactive evaluation.
* [#671](https://github.com/clojure-emacs/cider/issues/671): Remove problematic code that was
setting the REPL's initial ns based on lein's `:init-ns` option.
* [#695](https://github.com/clojure-emacs/cider/issues/695): Keep point at
original position when clearing or highlighting test results.
* [#744](https://github.com/clojure-emacs/cider/issues/744): Fix the ability to customize the
lein command invoked by `cider-jack-in`.
* [#752](https://github.com/clojure-emacs/cider/issues/752): Don't assume
`clojure.core/let` is always available as `let`.
* [#772](https://github.com/clojure-emacs/cider/issues/772): Don't try to read Clojure results as
Emacs Lisp code.
* [#631](https://github.com/clojure-emacs/cider/issues/631): Set `file` and `line` metadata when
doing interactive evaluation.
* nREPL sessions are now closed on `cider-quit`.
* Fix minibuffer history for `cider-read-and-eval`.

## 0.7.0 (2014-08-05)

### New features

* New `cider-auto-jump-to-error` control variable for auto jumping to error
  location.
* [#537](https://github.com/clojure-emacs/cider/pull/537): New support for
Java symbol lookup from cider-nrepl's info middleware.
* [#570](https://github.com/clojure-emacs/cider/pull/570): Enable toggling
of the 'all' filter on stacktraces.
* [#588](https://github.com/clojure-emacs/cider/pull/588): New `doc-mode`
for presenting fontified documentation, including Javadoc.
* New interactive command `cider-toggle-trace`.
* `cider-select` can now switch to the `*cider-error*` buffer (bound to `x`).
* [#613](https://github.com/clojure-emacs/cider/issues/613): New `clojure.test`
integration.
* [#22](https://github.com/clojure-emacs/cider/issues/22): New command
`cider-jump-to-resource` (bound to <kbd>C-c M-.</kbd>).
* [#664](https://github.com/clojure-emacs/cider/pull/664): New apropos support:
search function/var names (bound to <kbd>C-c C-d a</kbd>) or documentation
(bound to <kbd>C-c C-d A</kbd>).
* You can view Grimoire's entry for a particular Clojure (built-in) symbol in
Emacs with `cider-grimoire` (<kbd>C-c C-d g</kbd>) or your default browser with
`cider-grimoire-web` (<kbd>C-c C-d h</kbd>).
* `cider-mode` now displays the namespace of the current buffer in the mode-line
  (as SLIME does).

### Changes

* [#597](https://github.com/clojure-emacs/cider/issues/597): Don't process nREPL
  messages unless the whole message has been received.
* [#603](https://github.com/clojure-emacs/cider/pull/603): New variable
`cider-show-error-buffer` to control the behavior of the error buffer. Obsoletes
`cider-popup-on-error`, `cider-popup-stacktraces` and
`cider-repl-popup-stacktraces`.
* `cider-nrepl` is now required. Without it pretty much nothing will work.
* Removed redundant command `cider-src`.
* Renamed `nrepl-log-events` variable to `nrepl-log-messages`.
* Renamed `nrepl-log-events` command to `nrepl-log-messages`.
* Remove redundant `cider-src` command.
* [#582](https://github.com/clojure-emacs/cider/pull/582): Enable efficient
loading of jar/zip resources.
* [#589](https://github.com/clojure-emacs/cider/pull/589): Don't prefer local
paths over tramp by default.
* [#554](https://github.com/clojure-emacs/cider/issues/554): `cider-auto-select-error-buffer` is set to `t` by default.
* [#610](https://github.com/clojure-emacs/cider/pull/610): Present error and
stacktrace info for all exception causes.
* Removed `cider-repl-print-length` config option and
`cider-repl-toggle-print-length-limiting` command.
* Remapped `cider-doc` to <kbd>C-c C-d d</kbd>.
* Remapped `cider-javadoc` to <kbd>C-c C-d j</kbd>
* cider's scratch is now more consistent with an Emacs Lisp scratch buffer.

### Bugs fixed

* [#577](https://github.com/clojure-emacs/cider/pull/577): Fix bencode decoding
of negative integers.
* [#607](https://github.com/clojure-emacs/cider/pull/607): Respect
  `*print-length*` in `cider-pprint-eval-defun-at-point` and
  `cider-pprint-eval-last-sexp`.

## 0.6.0 (2014-04-24)

### New features

* New interactive command `cider-change-buffers-designation`.
* Cider command uses `cider-known-endpoints`.
* [#490](https://github.com/clojure-emacs/cider/pull/490): Dedicated
  support for `company-mode` in `cider-complete-at-point`.
* [#489](https://github.com/clojure-emacs/cider/issues/489): Enable
  cider-jack-in on tramp source buffers.
* [#460](https://github.com/clojure-emacs/cider/issues/460): Support for
cider-nrepl's complete middleware for CLJ/CLJS autocomplete.
* [#465](https://github.com/clojure-emacs/cider/issues/465): Support for
cider-nrepl's info middleware for jump-to-definition.
* [#469](https://github.com/clojure-emacs/cider/issues/469): Add option
`cider-prompt-save-file-on-load`.
* New interactive command `cider-insert-defun-in-repl`.
* New interactive command `cider-insert-ns-form-in-repl`.
* New inspector inspired by SLIME's inspector
* STDERR output is now font-locked with `cider-repl-err-output-face` to make it
visually distinctive from `cider-repl-output-face` (used for STDOUT output).
* New interactive command `cider-scratch`.
* [#521](https://github.com/clojure-emacs/cider/pull/521): New interactive
stacktrace filtering/navigation using cider-nrepl's stacktrace middleware.

### Changes

* [#513](https://github.com/clojure-emacs/cider/issues/513):
  Remove hardcoded use of IDO mode and use `completing-read`.
* Required Emacs version is now 24.1.
* [#486](https://github.com/clojure-emacs/cider/issues/486): Improve
  support for tramp, so tramp paths do not get used in compiled debug
  information.  `cider-jump` still uses tramp filenames to find
  definitions if used in a buffer associated with a tramp file.
* Renamed `cider` command to `cider-connect`.

### Bugs fixed

* [#515](https://github.com/clojure-emacs/cider/issues/515): Fix
inconsistent prompt used for load symbol functions.
* [#501](https://github.com/clojure-emacs/cider/issues/501): Fix
nil appearing in nrepl-server buffer name when no project directory.
* [#493](https://github.com/clojure-emacs/cider/issues/493) Fix rotate connection to handle no
nREPL connection.
* [#468](https://github.com/clojure-emacs/cider/issues/468): Fix
pretty-printing of evaluation results so that `*1` is set properly.
* [#439](https://github.com/clojure-emacs/cider/issues/439): Fix
race condition bug in `cider-restart`.
* [#441](https://github.com/clojure-emacs/cider/issues/441): Fix timing bug in `cider-jack-in`.
* [#482](https://github.com/clojure-emacs/cider/issues/482): Fix jump-to-def for cljx dependency jars.

## 0.5.0 (2014-01-24)

### New features

* <kbd>C-c M-f</kbd> Select a function from the current namespace using IDO and insert into the REPL buffer.
* `cider-read-and-eval` now supports completion and keeps history.
* Added ability to limit the number of objects printed in collections
  by managing `*print-length*`. `cider-repl-print-length` can be used
  to set a limit, and `cider-repl-toggle-print-length-limiting` can be
  used to toggle the enforcement of the limit.
* New config `cider-eval-result-prefix` controls the prefix displayed before results
from interactive evaluation displayed in the minibuffer.
* New config `cider-repl-result-prefix` controls the prefix displayed before results in the REPL.
* Font-lock interactive evaluation results as Clojure code.
* Added the ability to font-lock input and results in the REPL as Clojure code. This is controlled via
the option `cider-repl-use-clojure-font-lock`.
* Added `cider-pprint-eval-defun-at-point`, a companion to `cider-pprint-eval-last-sexp` which works on the top-level form.
* The REPL buffer name uses host if no project directory available; `*cider-repl*` will appear as `*cider-repl <host>*`.

### Bugs fixed

* [#316](https://github.com/clojure-emacs/cider/issues/316): Honor the `:init-ns` namespace on startup.
* [#436](https://github.com/clojure-emacs/cider/issues/436): Fix an infinite loop when evaluating ns forms.
* [#435](https://github.com/clojure-emacs/cider/issues/435): Fix trampling of `cider-switch-to-repl-buffer` by `cider-switch-to-relevant-repl-buffer`.

## 0.4.0 (2013-12-03)

### New features

* Added new interactive command `cider-read-and-eval` (bound to `C-c M-:` in `cider-mode`).
* Added new interactive command `cider-eval-last-sexp-to-repl` (`C-c M-e`). The command will output the result
of the evaluated code to the REPL buffer, so you can easily play with the output there afterwards.
* Added new interactive command `cider-insert-last-sexp-in-repl` (`C-c M-p`).
* Added new interactive command `cider-eval-last-expression-and-replace` (`C-c C-w`).
* Implemented REPL shortcuts, triggered by pressing `,` at the start of a REPL input line (similar to the ones in SLIME).
* Added new interactive command `cider-ping` to check connectivity with the server.

### Changes

* Renamed `cider-history-size` to `cider-repl-history-size`.
* Renamed `cider-history-file` to `cider-repl-history-file`.
* Renamed `cider-wrap-history` to `cider-repl-wrap-history`.
* Renamed `cider-eval-expression-at-point` to `cider-eval-defun-at-point`.
* Changed `last-expression` to `last-sexp` in a number of functions.

### Bugs fixed

* [#315](https://github.com/clojure-emacs/cider/issues/393): Removed spurious newlines in output.
* [#237](https://github.com/clojure-emacs/cider/issues/237): Don't swallow output from futures.
* Create non-existing namespaces, when evaluating code in Clojure buffers.

## 0.3.1 (2013-10-29)

* Fix REPL init

## 0.3.0 (2013-10-28)

### New features

* The variable `cider-repl-display-in-current-window` controls whether the REPL should be displayed in the current window when switched to.
* `cider-repl-set-ns` can now be invoked in the REPL.
* The content of `.nrepl-port`, if present, will be used as the
  default port for <kbd>M-x nrepl</kbd>. This is in addition to `target/repl-port`.
* Applies ANSI color to all output in the REPL buffer.

### Changes

* Renamed package to CIDER.
* Split package into several files.
* Renamed `cider-interaction-mode` to `cider-mode`.

### Bugs fixed

* [#393](https://github.com/clojure-emacs/cider/issues/393) - Error when evaluating strings with a namespace declaration in them.

## 0.2.0 (2013-10-10)

### New features

* <kbd>C-c M-d</kbd> will display default nREPL connection details.
* <kbd>C-c M-r</kbd> will rotate and display the default nREPL connection.
* Setting the variable `nrepl-buffer-name-show-port` will display the port on which the nREPL server is running.
* The REPL buffer name uses project directory name; `*nrepl*` will appear as `*nrepl project-directory-name*`.
* The nREPL connection buffer name uses project directory name; `*nrepl-connection*` will appear as `*nrepl-connection project-directory-name*`.
* nREPL server buffer name uses project directory name; `*nrepl-server*` will appear as `*nrepl-server project-directory-name*`.
* <kbd>C-c C-Z</kbd> will select the nrepl buffer based on the current namespace.
* <kbd>C-u C-c C-Z</kbd> will select the nrepl buffer based on a user project directory prompt.
* Bind <kbd>C-c C-q</kbd> to `nrepl-quit`
* Added an option to auto-select error popups (`nrepl-auto-select-error-buffer`)
* Made the display of the REPL buffer on connect optional

### Changes

* Renamed `nrepl-mode` to `nrepl-repl-mode`

### Bugs fixed

* <kbd>C-c M-s</kbd> (`nrepl-selector`) was bound to non-existing symbol.
* Fix indentation in REPL buffers.
* Fix `nrepl-doc` on Clojure 1.5

## 0.1.8 (2013-08-08)

### New features

* Evaluate all namespace forms `(ns ...)` in the user namespace.
* Add highlighting of compilation warnings in addition to existing highlighting of errors
* Add support for selecting last Clojure source buffer with keybinding
<kbd>C-c C-z</kbd> (the same as `nrepl-switch-to-repl-buffer`).
* The content of `target/repl-port`, if present, will be used as the
  default port for <kbd>M-x nrepl</kbd>
* Added an extendable slime-style selector command and binding <kbd>C-c M-s</kbd>

### Bugs fixed

* <kbd>M-.</kbd> (`nrepl-jump`) on remote nrepl connection (across OS hosts) has been fixed.

## 0.1.7 (2013-03-13)

### New features

* Add support for multiple nrepl sessions.  A single session is closed with
  `M-x nrepl-close`.  A REPL session is made default with
  `M-x nrepl-make-repl-connection-default`.
* Added support for pretty-printing in the REPL buffer.
* Added a check for the presence of an existing `*nrepl*` buffer before
creating a new one with `nrepl-jack-in` or `nrepl`.
* `M-.` learned about namespaces.
* Added new customization variable `nrepl-popup-stacktraces-in-repl`.
* Added some convenience keybindings to `clojure-mode` -
`nrepl-jack-in` is now bound to <kbd>C-c M-j</kbd> and `nrepl` is
bound to <kbd>C-c M-c</kbd>.
* Added `nrepl-hide-special-buffers` setting to control the display of special
buffers like `*nrepl-server*` and `*nrepl-connection*`.
* Apply ANSI color codes to output sent to nrepl buffers.
* Add a connection browser `nrepl-connection-browser` to allow control of
  multiple connections.
* Add macroexpand key bindings to `nrepl-mode-map`.
* Don't suppress namespaces in macroexpansion.
* Add explicit require of expected namespaces in the REPL buffer.

* Add command `nrepl-pprint-eval-last-expression`.
* Add an event buffer for debugging.
* Allow connections without REPL buffers.
* Add hook `nrepl-file-loaded-hook` which runs on load-file
  completion.
* Expand ido-completion to include "used" variables in addition to
  "interned" variables.

### Bugs fixed

* More accurate matching of filenames in stacktraces.
* Fix #290 - Macroexpand buffer truncates long expansions

## 0.1.6 (2013-01-29)

### New features

* Ported SLIME macroexpansion mode (see README for full documentation)
* Updated macroexpansion to use pprint with code-dispatch
* Eldoc argument highlighting
* Simplify popup buffer quit/restore using `quit-window'.
* Add nrepl-disconnected-hook and disable nrepl when disconnected.
* Get key bindings documentation into the minor mode descriptions (Ivan Necas)
* made the TAB command in the nrepl-mode buffers configurable (Bozhidar Batsov)
* Added convenience function to report the version of nREPL in use. (fogus)
* Shift-Home and Shift-Ctrl-a in repl, which select just the user input when on the input line. (Ivan Kozik)

### Bugs fixed

* Emit server log output at bottom of `*nrepl-server*` buffer. (Brian Rowe)
* Reset nrepl-buffer-ns on nrepl-restart.  Fixes issue #187.
* Implement nrepl-mode as a derived mode. (Bozhidar Batsov)
* fix #194 - stacktrace buffer was not respecting nrepl-popup-stacktraces (Bozhidar Batsov)
* Fix message formatting for results containing "%" (fixes issue #195).
* Fix NPE in nrepl-jump (issue #124).  (cola-zero)
* Fix nrepl to work with fish shell (issue #192). (Dario Bertini)
* Adjusted the javadoc keybinding and mentioned it in the README. (Bozhidar Batsov)
* Fix issue #163 - exclude ns from nrepl-load-file.
* Ignore "killed" and "hangup" events in sentinel (Chris Bilson)
* Clear the correct region when replacing the input line. (Ivan Kozik)
* Fix issue #146.  Include "@" in nrepl-input-complete-p.
* Handle stdout messages that arrive after status "done"

## 0.1.5 (2012-10-22)

### New features

* Support for describe op to determine which server ops are available at startup
* Support for the following server ops (if available): load-file, complete, and javadoc (available in ritz)
* Added nrepl-host and nrepl-port custom variables M-x nrepl default hostname/port
* Ported over the following REPL buffer functions from slime:
    History regexp filtering - M-s nrepl-next-matching-input, M-r nrepl-previous-matching-input
    C-c C-u nrepl-kill-input
    C-c C-n nrepl-next-prompt/C-c C-p nrepl-previous-prompt
* Added nrepl-quit and nrepl-restart commands
* Added menus for nrepl-mode and nrepl-interaction-mode
* Add nrepl-eval-print-last-expression

### Bugs fixed

* Ensure nrepl-eval-sync waits for :done when response is chunked

## 0.1.4 (2012-09-18)

### New features

* Improvements and simplifications for completion (Tassilo Horn)
* Documentation additions and fixes (Ryan Fowler, Nikita Beloglazov, Bozhidar Batsov, Juha Syrjl, Philipp Meier)
* Make completion back-end and error handler configurable (Hugo Duncan)
* Accept host as well as port on connect (Ken Restivo)
* Enable nrepl-interaction-mode in clojurescript-mode (Nelson Morris)
* Emit stdout from interactive evaluations into the REPL buffer

### Bugs fixed

* Fix paredit .. don't make clojure-mode-map parent of nrepl-interaction-mode-map (Tassilo Horn)
* Fixes for ECB interop (Matthew Willson)
* Namespace qualify tooling calls (Justin Kramer)
* Eldoc fixes (Jack Moffitt)
* Fix path quoting in load file for Windows (Philipp Meier)
* Fix nREPL / Emacs error "Unable to resolve symbol: if-let"

## 0.1.3 (2012-08-19)

### New features

* eldoc support for displaying arglists in the minibuffer (Stefan Kamphausen)
* persistent REPL history (Stefan Kamphausen)
* fix for jumbled stacktraces (Ryan Fowler)
* add a doc keybinding for the REPL buffer (Ken Restivo)
* plumbing to support ac-nrepl [https://github.com/purcell/ac-nrepl] (Steve Purcell)
* stdin support (which also provides support for debug-repl
  [https://github.com/GeorgeJahad/debug-repl] and limit-break [https://github.com/technomancy/limit-break])

## 0.1.2 (2012-07-24)

### New features

* convert nrepl-interaction-mode into a major mode
* display stacktrace on eval-error
* change lein command to `lein`
* add fn to eval current buffer's ns
* handle filter messages spanning multiple chunks of output
* Let nrepl-jack-in accept project dir when given a prefix arg.
* C-c C-b nrepl-interrupt
* client session management
* added words of inspiration + version at startup
* Add M-n and M-p to nrepl-mode-map.
* Implement M-.: nrepl-jump-to-def.
* Implement basic completion.
* Implement nrepl-doc.
* Prevent M-p at top of history from pushing position one step further.
* M-n after end of history should blank out input.
* Add M-n and M-p to nrepl-mode-map.
* Implement M-.: nrepl-jump-to-def.

## 0.1.1 (2012-07-11)

* Initial version
