(require 'buttercup)
(require 'cider)
(require 'cider-common)

;;; cider-common tests
(describe "cider-abbreviate-ns"
  (it "handles nil input"
    (expect (cider-abbreviate-ns nil) :to-equal nil))

  (it "handles empty string intput"
    (expect (cider-abbreviate-ns "") :to-equal ""))

  (it "shortens all ns segments but the last"
    (expect (cider-abbreviate-ns "some.test.ns") :to-equal "s.t.ns"))

  (it "handles single-segment namespaces"
    (expect (cider-abbreviate-ns "ns") :to-equal "ns")))

(describe "cider-last-ns-segment"
  (it "handles nil input"
    (expect (cider-last-ns-segment nil) :to-equal nil))

  (it "handles empty string intput"
    (expect (cider-last-ns-segment "") :to-equal ""))

  (it "drops all ns segments but the last"
    (expect (cider-last-ns-segment "some.test.ns") :to-equal "ns"))

  (it "handles single-segment namespaces"
    (expect (cider-last-ns-segment "ns") :to-equal "ns")))
