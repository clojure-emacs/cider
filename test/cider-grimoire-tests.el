(require 'cider)
(require 'cider-grimoire)

;;; grimoire tests

(ert-deftest cider-grimoire-replace-special ()
  (should (equal (cider-grimoire-replace-special "isa?") "isa_QMARK"))
  (should (equal (cider-grimoire-replace-special "really-isa?") "really-isa_QMARK"))
  (should (equal (cider-grimoire-replace-special "..") "DOT__DOT"))
  (should (equal (cider-grimoire-replace-special ".") "DOT"))
  (should (equal (cider-grimoire-replace-special "/") "SLASH")))

(ert-deftest cider-grimoire-url ()
  (should (equal "http://conj.io/search/v0/clojure.core/even_QMARK/"
                 (cider-grimoire-url "even?" "clojure.core")))
  (should (equal nil
                 (cider-grimoire-url nil "clojure.core")))
  (should (equal nil
                 (cider-grimoire-url "even?" nil))))
