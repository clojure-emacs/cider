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
  (should (equal "http://grimoire.arrdem.com/1.5.0/clojure.core/even_QMARK/"
                 (cider-grimoire-url "even?" "clojure.core" "1.5.1")))
  (should (equal "http://grimoire.arrdem.com/1.5.0/clojure.core/"
                 (cider-grimoire-url nil "clojure.core" "1.5.1"))))
