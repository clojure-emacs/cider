(require 'ert)
(require 'noflet)
(require 'cider)
(require 'cider-apropos)

(ert-deftest cider-apropos-not-connected ()
  (noflet ((cider-connected-p () nil))
    (should-error (cider-apropos "test") :type 'user-error)))

(ert-deftest cider-apropos-unsupported-op ()
  (noflet ((cider-ensure-op-supported (op) nil))
    (should-error (cider-apropos "test") :type 'user-error)))

(ert-deftest cider-apropos-documentation-not-connected ()
  (noflet ((cider-connected-p () nil))
    (should-error (cider-apropos-documentation) :type 'user-error)))

(ert-deftest cider-apropos-documentation-unsupported-op ()
  (noflet ((cider-ensure-op-supported (op) nil))
    (should-error (cider-apropos-documentation) :type 'user-error)))
