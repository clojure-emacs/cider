(require 'buttercup)
(require 'cider)
(require 'cider-util)

;;; cider-util tests
(describe "cider-symbol-at-point"
  (it "doesn't move the cursor"
    (with-temp-buffer
      (clojure-mode)
      (insert "something else\n")
      (expect (cider-symbol-at-point) :not :to-be-truthy)
      (expect (cider-symbol-at-point 'lookback) :to-equal "else")
      (expect (point) :to-equal (point-max)))))


(describe "cider--version"
  :var (cider-version cider-codename)

  (it "handles version unavailable error"
    (spy-on 'pkg-info-version-info :and-throw-error '(error "No version"))
    (setq cider-version "0.11.0"
          cider-codename "Victory")
    (expect (cider--version) :to-equal "0.11.0 (Victory)"))

  (it "returns correct version number when available"
    (spy-on 'pkg-info-version-info :and-return-value "0.11.0")
    (setq cider-version "0.11.0"
          cider-codename "Victory")
    (expect (cider--version) :to-equal "0.11.0 (Victory)"))

  (it "handles snapshot versions"
    (spy-on 'pkg-info-version-info :and-return-value "0.11.0snapshot (package: 20160301.2217)")
    (setq cider-version "0.11.0-snapshot"
          cider-codename "Victory")
    (expect (cider--version) :to-equal "0.11.0snapshot (package: 20160301.2217)")))

(describe "cider-symbol-at-point"
  (describe "when there is a symbol at point"
    (it "returns the symbol"
      (with-temp-buffer
        (insert "some-symbol    ")
        (should (not (cider-symbol-at-point)))
        (expect (cider-symbol-at-point 'look-back) :to-equal "some-symbol"))))

  (describe "when there's nothing at point"
    (it "returns nil"
      (spy-on 'thing-at-point :and-return-value nil)
      (expect (cider-symbol-at-point) :not :to-be-truthy)))

  (it "can identify symbols in a repl, ignoring the repl prompt"
    ;; ignores repl prompts
    (spy-on 'thing-at-point :and-return-value (propertize "user>" 'field 'cider-repl-prompt))
    (expect (cider-symbol-at-point) :not :to-be-truthy)
    (spy-on 'thing-at-point :and-return-value (propertize "boogie>" 'field 'cider-repl-prompt))
    (expect (cider-symbol-at-point) :not :to-be-truthy)

    ;; works for normal text in a repl buffer
    (spy-on 'thing-at-point :and-return-value "boogie>")
    (expect (cider-symbol-at-point) :to-equal "boogie>")))

(describe "cider-sexp-at-point"
  (describe "when the param 'bounds is not given"
    (it "returns the sexp at point"
      (with-temp-buffer
        (clojure-mode)
        (insert "a\n\n,")
        (save-excursion (insert "(defn ...)\n\nb"))
        (expect (cider-sexp-at-point) :to-equal "(defn ...)")
        (insert "@")
        (expect (cider-sexp-at-point) :to-equal "(defn ...)")
        (delete-char -1)
        (insert "'")
        (expect (cider-sexp-at-point) :to-equal "(defn ...)"))))

  (describe "when the param 'bounds is given"
    (it "returns the bounds of starting and ending positions of the sexp"
      (with-temp-buffer
        (clojure-mode)
        (insert "a\n\n,")
        (save-excursion (insert "(defn ...)\n\nb"))
        (delete-char -1)
        (insert "'")
        (expect (cider-sexp-at-point 'bounds) :to-equal '(5 15))))))

(describe "cider-defun-at-point"
  (describe "when the param 'bounds is not given"
    (it "returns the defun at point"
      (with-temp-buffer
        (clojure-mode)
        (insert "a\n\n(defn ...)")
        (save-excursion (insert "\n\nb"))
        (expect (cider-defun-at-point) :to-equal "(defn ...)\n")
        (forward-sexp -1)
        (expect (cider-defun-at-point) :to-equal "(defn ...)\n"))))

  (describe "when the param 'bounds is given"
    (it "returns the bounds of starting and ending positions of the defun"
      (with-temp-buffer
        (clojure-mode)
        (insert "a\n\n(defn ...)")
        (save-excursion (insert "\n\nb"))
        (expect (cider-defun-at-point 'bounds) :to-equal '(4 15))))))

(describe "cider-repl-prompt-function"
  (it "returns repl prompts"
    (expect (cider-repl-prompt-default "some.pretty.long.namespace.name")
            :to-equal "some.pretty.long.namespace.name> ")
    (expect (cider-repl-prompt-lastname "some.pretty.long.namespace.name")
            :to-equal "name> ")
    (expect (cider-repl-prompt-abbreviated "some.pretty.long.namespace.name")
            :to-equal "s.p.l.n.name> ")))

(describe "cider-repl--emit-output-at-pos"
  (it "formats and inserts the text in the given buffer"
    (with-temp-buffer
      (let* ((ansi-color-names-vector ["black" "red3" "green3" "yellow3" "blue2" "magenta3" "cyan3" "gray90"])
             (ansi-color-map (ansi-color-make-color-map)))
        (cider-repl-reset-markers)

        (cider-repl--emit-output-at-pos (current-buffer) "[30ma[0m" 'cider-repl-stdout-face (point))
        (cider-repl--emit-output-at-pos (current-buffer) "b" 'cider-repl-stdout-face (point))
        (cider-repl--emit-output-at-pos (current-buffer) "[31mc" 'cider-repl-stdout-face (point))
        (cider-repl--emit-output-at-pos (current-buffer) "d[0m" 'cider-repl-stdout-face (point))

        (expect (buffer-string) :to-equal "a\nb\nc\nd\n")
        (expect (get-text-property 1 'font-lock-face)
                :to-equal '(foreground-color . "black"))
        (expect (get-text-property 3 'font-lock-face)
                :to-equal 'cider-repl-stdout-face)
        (expect (get-text-property 5 'font-lock-face)
                :to-equal '(foreground-color . "red3"))
        (expect (get-text-property 7 'font-lock-face)
                :to-equal '(foreground-color . "red3"))))))

(describe "cider--url-to-file"
  (it "returns a url for a given file name"
    (expect (cider--url-to-file "file:/space%20test")
            :to-equal "/space test")
    (expect (cider--url-to-file "file:/C:/space%20test")
            :to-equal "C:/space test")))

(describe "cider-namespace-qualified-p"
  (it "returns true if given sym is namespace-qualified"
    (expect (cider-namespace-qualified-p "a/a") :to-be-truthy)
    (expect (cider-namespace-qualified-p "a.a/a") :to-be-truthy)
    (expect (cider-namespace-qualified-p "a-a/a") :to-be-truthy)
    (expect (cider-namespace-qualified-p "a.a-a/a-a") :to-be-truthy)
    (expect (cider-namespace-qualified-p "/") :not :to-be-truthy)
    (expect (cider-namespace-qualified-p "/a") :not :to-be-truthy)))

(describe "cider--deep-vector-to-list"
  (it "converts nested vectors to lists"
    (expect (cider--deep-vector-to-list '[1 2 3]) :to-equal '(1 2 3))
    (expect (cider--deep-vector-to-list '(1 2 3)) :to-equal '(1 2 3))
    (expect (cider--deep-vector-to-list '[[1] [2] [[3]]]) :to-equal '((1) (2) ((3))))
    (expect (cider--deep-vector-to-list '(1 [2] [([3])])) :to-equal '(1 (2) (((3)))))
    (expect (cider--deep-vector-to-list 'bug) :to-equal 'bug)
    (expect (cider--deep-vector-to-list '[bug]) :to-equal '(bug))
    (expect (cider--deep-vector-to-list '(bug)) :to-equal '(bug))))

(describe "cider-manual-url"
  :var (cider-version)
  (it "returns the manual correct url for stable cider versions"
    (setq cider-version "0.11.0")
    (expect (cider-manual-url) :to-equal "http://cider.readthedocs.org/en/stable/"))

  (it "returns the manual correct url for snapshot cider versions"
    (setq cider-version "0.11.0-snapshot")
    (expect (cider-manual-url) :to-equal "http://cider.readthedocs.org/en/latest/")))

(describe "cider-refcard-url"
  :var (cider-version)
  (it "returns the refcard correct url for stable cider versions"
    (setq cider-version "0.11.0")
    (expect (cider-refcard-url) :to-equal "https://github.com/clojure-emacs/cider/raw/v0.11.0/doc/cider-refcard.pdf"))

  (it "returns the refcard correct url for snapshot cider versions"
    (setq cider-version "0.11.0-snapshot")
    (expect (cider-refcard-url) :to-equal "https://github.com/clojure-emacs/cider/raw/master/doc/cider-refcard.pdf")))
