(require 'buttercup)
(require 'cider)
(require 'cider-classpath)

(describe "cider-classpath"
  (it "raises user-error when cider is not connected."
    (spy-on 'cider-connected-p :and-return-value nil)
    (expect (lambda () (cider-classpath)) :to-throw 'user-error))

  (it "raises user-error when the `classpath' op is not supported."
    (spy-on 'cider-ensure-op-supported :and-return-value nil)
    (expect (lambda () (cider-classpath)) :to-throw 'user-error)))

(describe "cider-open-classpath-entry"
  (it "raises user-error when cider is not connected."
    (spy-on 'cider-connected-p :and-return-value nil)
    (expect (lambda () (cider-open-classpath-entry)) :to-throw 'user-error))

  (it "raises user-error when the `classpath' op is not supported."
    (spy-on 'cider-ensure-op-supported :and-return-value nil)
    (expect (lambda () (cider-open-classpath-entry)) :to-throw 'user-error)))
