(require 'buttercup)
(require 'cider-browse-ns)

(describe "cider-browse-ns--text-face"
  (it "identifies a function"
    (expect (cider-browse-ns--text-face '(dict "arglists" "fn arg list"))
            :to-equal 'font-lock-function-name-face))

  (it "identifies a macro"
    (expect (cider-browse-ns--text-face '(dict "arglists" "fn arg list" "macro" "true"))
            :to-equal 'font-lock-keyword-face))

  (it "identifies a variable"
    (expect (cider-browse-ns--text-face '(dict))
            :to-equal 'font-lock-variable-name-face)))

(describe "cider-browse-ns"
  :var (cider-browse-ns-buffer)
  (it "lists out all forms of a namespace with correct font-locks"
    (spy-on 'cider-sync-request:ns-vars-with-meta :and-return-value
            '(dict "blank?" (dict "arglists" "fn arg list")))

    (with-temp-buffer
      (setq cider-browse-ns-buffer (buffer-name (current-buffer)))
      (cider-browse-ns "clojure.string")
      (search-forward "clojure")
      (expect (get-text-property (point) 'face) :to-equal 'font-lock-type-face)
      (search-forward "blank")
      (expect (get-text-property (point) 'font-lock-face) :to-equal 'font-lock-function-name-face))))
