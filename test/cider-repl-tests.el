(require 'buttercup)
(require 'cider-repl)

(describe "cider-repl--banner"
  :var (cider-version cider-codename)
  (before-all
    (spy-on 'cider--java-version :and-return-value "1.8.0_31")
    (spy-on 'cider--clojure-version :and-return-value "1.8.0")
    (spy-on 'cider--nrepl-version :and-return-value "0.2.12")
    (spy-on 'cider--connection-host :and-return-value "localhost")
    (spy-on 'cider--connection-port :and-return-value "54018")
    (setq cider-version "0.12.0")
    (setq cider-codename "Seattle"))

  (describe "when the cider package version information is available"
    (it "returns the repl banner string"
      (spy-on 'pkg-info-version-info :and-return-value "0.12.0")
      (expect (cider-repl--banner) :to-equal
              ";; Connected to nREPL server - nrepl://localhost:54018
;; CIDER 0.12.0 (Seattle), nREPL 0.2.12
;; Clojure 1.8.0, Java 1.8.0_31
;;     Docs: (doc function-name)
;;           (find-doc part-of-name)
;;   Source: (source function-name)
;;  Javadoc: (javadoc java-object-or-class)
;;     Exit: <C-c C-q>
;;  Results: Stored in vars *1, *2, *3, an exception in *e;")))

  (describe "when the cider package version information is not available"
    (it "returns the repl banner string"
      (spy-on 'pkg-info-version-info :and-throw-error '(error "No package version"))
      (expect (cider-repl--banner) :to-equal
              ";; Connected to nREPL server - nrepl://localhost:54018
;; CIDER 0.12.0 (Seattle), nREPL 0.2.12
;; Clojure 1.8.0, Java 1.8.0_31
;;     Docs: (doc function-name)
;;           (find-doc part-of-name)
;;   Source: (source function-name)
;;  Javadoc: (javadoc java-object-or-class)
;;     Exit: <C-c C-q>
;;  Results: Stored in vars *1, *2, *3, an exception in *e;"))))
