(require 'buttercup)
(require 'cider-browse-ns)

(describe "cider-browse-ns--text-face"
  :var (cider-resolve-var)
  (describe "when the namespace is not loaded in the REPL"
    (spy-on 'cider-resolve-var :and-return-value nil)
    (it "returns font-lock-function-name-face"
      (expect (cider-browse-ns--text-face "clojure.string" "blank?")
              :to-equal 'font-lock-function-name-face)))

  (describe "when the namespace is loaded in the REPL"
    (it "identifies a function"
      (spy-on 'cider-resolve-var :and-return-value
              '(dict "arglists" "fn arg list"))
      (expect (cider-browse-ns--text-face "clojure.core" "reduce")
              :to-equal 'font-lock-function-name-face))

    (it "identifies a macro"
      (spy-on 'cider-resolve-var :and-return-value
              '(dict "arglists" "fn arg list" "macro" "true"))
      (expect (cider-browse-ns--text-face "clojure.core" "defn")
              :to-equal 'font-lock-keyword-face))

    (it "identifies a variable"
      (spy-on 'cider-resolve-var :and-return-value
              '(dict))
      (expect (cider-browse-ns--text-face "clojure.core" "*clojure-version*")
              :to-equal 'font-lock-variable-name-face))))

(describe "cider-browse-ns"
  :var (cider-browse-ns-buffer)
  (it "lists out all forms of a namespace with correct font-locks"
    (spy-on 'cider-sync-request:ns-vars :and-return-value
            '("blank?"))
    (spy-on 'cider-resolve-var :and-return-value '(dict "arglists" "fn arg list"))

    (with-temp-buffer
      (setq cider-browse-ns-buffer (buffer-name (current-buffer)))
      (cider-browse-ns "clojure.string")
      (search-forward "clojure")
      (expect (get-text-property (point) 'face) :to-equal 'font-lock-type-face)
      (search-forward "blank")
      (expect (get-text-property (point) 'font-lock-face) :to-equal 'font-lock-function-name-face))))
