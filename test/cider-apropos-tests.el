(require 'buttercup)
(require 'cider)
(require 'cider-apropos)

(describe "cider-apropos"
  (it "raises user-error when cider is not connected."
    (spy-on 'cider-connected-p :and-return-value nil)
    (expect (lambda () (cider-apropos "test")) :to-throw 'user-error))

  (it "raises user-error when the `apropos' op is not supported."
    (spy-on 'cider-ensure-op-supported :and-return-value nil)
    (expect (lambda () (cider-apropos "test")) :to-throw 'user-error)))

(describe "cider-apropos-documentation"
  (it "raises user-error when cider is not connected."
    (spy-on 'cider-connected-p :and-return-value nil)
    (expect (lambda () (cider-apropos-documentation)) :to-throw 'user-error))

  (it "raises user-error when the `apropos' op is not supported."
    (spy-on 'cider-ensure-op-supported :and-return-value nil)
    (expect (lambda () (cider-apropos-documentation)) :to-throw 'user-error)))
