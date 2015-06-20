# Changelog

## master (unreleased)

### Bugs fixed

* [#1153](https://github.com/clojure-emacs/cider/pull/1153): Fix behavior of `cider-switch-to-current-repl-buffer`.
* [#1139](https://github.com/clojure-emacs/cider/issues/1139): Fix evaluation of ns forms and of forms with unevaluated namespaces.
* Replace `assert` with `cl-assert` (we don't use anything from `cl` now).
* [#1135](https://github.com/clojure-emacs/cider/pull/1135): Fix a corner case with locals display in the debugger.
* [#1129](https://github.com/clojure-emacs/cider/issues/1129): Fix occasional `(wrong-type-argument stringp nil)` on clojure-android.

## 0.9.0 / 2015-06-16

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

## 0.8.2 / 2014-12-21

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
* Use the `field` text property to make move-beginning-of-line respect the repl prompt instead of writing our own beginning-of-line commands.

## 0.8.1 / 2014-11-20

### Bugs fixed

* Fixed version mismatch warning on CIDER startup (the actual bug was in `cider-nrepl`).

## 0.8.0 / 2014-11-20

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

## 0.7.0 / 2014-08-05

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

## 0.6.0 / 2014-04-24

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
* STDERR ouput is now font-locked with `cider-repl-err-output-face` to make it
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

## 0.5.0 / 2014-01-24

### New features

* <kbd>C-c M-f</kbd> Select a function from the current namespace using IDO and insert into the REPL buffer.
* `cider-read-and-eval` now supports completion and keeps history.
* Added ability to limit the number of objects printed in collections
  by managing `*print-length*`. `cider-repl-print-length` can be used
  to set a limit, and `cider-repl-toggle-print-length-limiting` can be
  used to toggle the enforcement of the limit.
* New config `cider-interactive-eval-result-prefix` controls the prefix displayed before results
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

## 0.4.0 / 2013-12-03

### New features

* Added new interactive command `cider-read-and-eval` (bound to `C-c M-:` in `cider-mode`).
* Added new interactive command `cider-eval-last-sexp-to-repl` (`C-c M-e`). The command will output the result
of the evaluated code to the REPL buffer, so you can easily play with the output there afterwords.
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

## 0.3.1 / 2013-10-29

* Fix REPL init

## 0.3.0 / 2013-10-28

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

## 0.2.0 / 2013-10-10

### New features

* <kbd>C-c M-d</kbd> will display current nREPL connection details.
* <kbd>C-c M-r</kbd> will rotate and display the current nREPL connection.
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

## 0.1.8 / 2013-08-08

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

## 0.1.7 / 2013-03-13

### New features

* Add support for multiple nrepl sessions.  A single session is closed with
  `M-x nrepl-close`.  A repl session is made default with
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
* Add explicit require of expected namespaces in the repl buffer.

* Add command `nrepl-pprint-eval-last-expression`.
* Add an event buffer for debugging.
* Allow connections without repl buffers.
* Add hook `nrepl-file-loaded-hook` which runs on load-file
  completion.
* Expand ido-completion to include "used" variables in addition to
  "interned" variables.

### Bugs fixed

* More accurate matching of filenames in stacktraces.
* Fix #290 - Macroexpand buffer truncates long expansions

## 0.1.6 / 2013-01-29

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

## 0.1.5 / 2012-10-22

### New features

* Support for describe op to determine which server ops are available at startup
* Support for the following server ops (if available): load-file, complete, and javadoc (available in ritz)
* Added nrepl-host and nrepl-port custom variables M-x nrepl default hostname/port
* Ported over the following repl buffer functions from slime:
    History regexp filtering - M-s nrepl-next-matching-input, M-r nrepl-previous-matching-input
    C-c C-u nrepl-kill-input
    C-c C-n nrepl-next-prompt/C-c C-p nrepl-previous-prompt
* Added nrepl-quit and nrepl-restart commands
* Added menus for nrepl-mode and nrepl-interaction-mode
* Add nrepl-eval-print-last-expression

### Bugs fixed

* Ensure nrepl-eval-sync waits for :done when response is chunked

## 0.1.4 / 2012-09-18

### New features

* Improvements and simplifications for completion (Tassilo Horn)
* Documentation additions and fixes (Ryan Fowler, Nikita Beloglazov, Bozhidar Batsov, Juha Syrjl, Philipp Meier)
* Make completion back-end and error handler configurable (Hugo Duncan)
* Accept host as well as port on connect (Ken Restivo)
* Enable nrepl-interaction-mode in clojurescript-mode (Nelson Morris)
* Emit stdout from interactive evaluations into the repl buffer

### Bugs fixed

* Fix paredit .. don't make clojure-mode-map parent of nrepl-interaction-mode-map (Tassilo Horn)
* Fixes for ECB interop (Matthew Willson)
* Namespace qualify tooling calls (Justin Kramer)
* Eldoc fixes (Jack Moffitt)
* Fix path quoting in load file for Windows (Philipp Meier)
* Fix nREPL / Emacs error "Unable to resolve symbol: if-let"

## 0.1.3 / 2012-08-19

### New features

* eldoc support for displaying arglists in the minibuffer (Stefan Kamphausen)
* persistent repl history (Stefan Kamphausen)
* fix for jumbled stacktraces (Ryan Fowler)
* add a doc keybinding for the repl buffer (Ken Restivo)
* plumbing to support ac-nrepl [https://github.com/purcell/ac-nrepl] (Steve Purcell)
* stdin support (which also provides support for debug-repl
  [https://github.com/GeorgeJahad/debug-repl] and limit-break [https://github.com/technomancy/limit-break])

## 0.1.2 / 2012-07-24

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

## 0.1.1 / 2012-07-11

* Initial version
