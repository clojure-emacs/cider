(require 'cider-version)

(ert-deftest test-cider--package-version-error-handling ()
  (noflet ((pkg-info-package-version () (error "No version header")))
          (should (equal (cider--package-version) nil))))

(ert-deftest test-cider-version-same ()
  (noflet ((cider--library-version () "0.1")
           (cider--package-version () "0.1"))
          (should (equal (cider-version) "0.1"))))

(ert-deftest test-cider-version-different ()
  (noflet ((cider--library-version () "0.1-cvs")
           (cider--package-version () "0.1"))
          (should (equal (cider-version) "0.1-cvs (package: 0.1)"))))

(ert-deftest test-cider-version-no-package-version ()
  (noflet ((cider--library-version () "0.1-cvs")
           (cider--package-version () nil))
          (should (equal (cider-version) "0.1-cvs"))))
