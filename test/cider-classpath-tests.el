(require 'ert)
(require 'noflet)
(require 'cider)
(require 'cider-classpath)

(ert-deftest cider-classpath-not-connected ()
  (noflet ((cider-connected-p () nil))
    (should-error (cider-classpath) :type 'user-error)))

(ert-deftest cider-classpath-unsupported-op ()
  (noflet ((cider-ensure-op-supported (op) nil))
    (should-error (cider-classpath) :type 'user-error)))

(ert-deftest cider-open-classpath-entry-not-connected ()
  (noflet ((cider-connected-p () nil))
    (should-error (cider-open-classpath-entry) :type 'user-error)))

(ert-deftest cider-open-classpath-entry-unsupported-op ()
  (noflet ((cider-ensure-op-supported (op) nil))
    (should-error (cider-open-classpath-entry) :type 'user-error)))
