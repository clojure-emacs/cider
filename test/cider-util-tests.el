(require 'cider)
(require 'cider-util)
(require 'ert)
(require 'noflet)

;;; cider-util tests
(ert-deftest cider-symbol-at-point-dont-move ()
  (with-temp-buffer
    (clojure-mode)
    (insert "something else\n")
    (should (not (cider-symbol-at-point)))
    (should (equal "else" (cider-symbol-at-point 'lookback)))
    (should (= (point) (point-max)))))

(ert-deftest cider--version-fallback ()
  (noflet ((pkg-info-version-info (package) (error "No version")))
    (let ((cider-version "0.11.0")
          (cider-codename "Victory"))
      (should (string= (cider--version) "0.11.0 (Victory)")))))

(ert-deftest cider--version-stable-version ()
  (noflet ((pkg-info-version-info (package) "0.11.0"))
    (let ((cider-version "0.11.0")
          (cider-codename "Victory"))
      (should (string= (cider--version) "0.11.0 (Victory)")))))

(ert-deftest cider--version-snapshot-version ()
  (noflet ((pkg-info-version-info (package) "0.11.0snapshot (package: 20160301.2217)"))
    (let ((cider-version "0.11.0-snapshot")
          (cider-codename "Victory"))
      (should (string= (cider--version) "0.11.0snapshot (package: 20160301.2217)")))))
