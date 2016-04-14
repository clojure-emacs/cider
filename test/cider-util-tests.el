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
