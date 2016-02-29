(require 'cider)
(require 'cider-util)
(require 'ert)

;;; cider-util tests
(ert-deftest cider-symbol-at-point-dont-move ()
  (with-temp-buffer
    (clojure-mode)
    (insert "something else\n")
    (should (not (cider-symbol-at-point)))
    (should (equal "else" (cider-symbol-at-point 'lookback)))
    (should (= (point) (point-max)))))
