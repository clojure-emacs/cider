(require 'buttercup)
(require 'cider)
(require 'cider-grimoire)

;;; grimoire tests

(describe "cider-grimoire-replace-special"
  (it "converts the input to a grimoire friendly format"
    (expect (cider-grimoire-replace-special "isa?") :to-equal "isa_QMARK")
    (expect (cider-grimoire-replace-special "really-isa?") :to-equal "really-isa_QMARK")
    (expect (cider-grimoire-replace-special "..") :to-equal "DOT__DOT")
    (expect (cider-grimoire-replace-special ".") :to-equal "DOT")
    (expect (cider-grimoire-replace-special "/") :to-equal "SLASH")
    ))

(describe "cider-grimoire-url"
  (it "creates a grimoire search URL"
    (expect (cider-grimoire-url "even?" "clojure.core") :to-equal "http://conj.io/search/v0/clojure.core/even_QMARK/")
    (expect (cider-grimoire-url nil "clojure.core") :to-equal nil)
    (expect (cider-grimoire-url "even?" nil) :to-equal nil)))
