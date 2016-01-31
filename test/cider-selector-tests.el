(require 'noflet)
(require 'cider)
(require 'cider-selector)

;; selector
(defun cider-invoke-selector-method-by-key (ch)
  (let ((method (find ch cider-selector-methods :key #'car)))
    (funcall (third method))))

(ert-deftest test-cider-selector-n ()
  (with-temp-buffer
    (let ((b1 (current-buffer)))
      (setq-local cider-endpoint '("123.123.123.123" 4006))
      (let ((cider-connections (list b1)))
        (cider-connection-browser)
        (with-temp-buffer ;; switch to another buffer
          (cider-invoke-selector-method-by-key ?n)
          (should (equal (current-buffer)
                         (get-buffer cider--connection-browser-buffer-name))))))))

(ert-deftest test-cider-selector-c ()
  (with-temp-buffer
    (rename-buffer "*testfile*.clj")
    (let ((b1 (current-buffer)))
      (setq major-mode 'clojure-mode)
      (with-temp-buffer
        (rename-buffer "*testfile*.el")
        (setq major-mode 'emacs-lisp-mode)
        (with-temp-buffer
          (should (not (equal (current-buffer) b1)))
          (cider-invoke-selector-method-by-key ?e)
          (should (not (equal (current-buffer) b1)))
          (cider-invoke-selector-method-by-key ?c)
          (should (equal (current-buffer) b1)))))))

(ert-deftest test-cider-selector-e ()
  (with-temp-buffer
    (rename-buffer "*testfile*.el")
    (let ((b1 (current-buffer)))
      (setq major-mode 'emacs-lisp-mode)
      (with-temp-buffer
        (rename-buffer "*testfile*.clj")
        (setq major-mode 'clojure-mode)
        (with-temp-buffer
          (should (not (equal (current-buffer) b1)))
          (cider-invoke-selector-method-by-key ?c)
          (should (not (equal (current-buffer) b1)))
          (cider-invoke-selector-method-by-key ?e)
          (should (equal (current-buffer) b1)))))))

(ert-deftest test-cider-selector-m ()
  (noflet ((cider-current-messages-buffer () "*nrepl-messages conn-id*"))
    (with-temp-buffer
      (rename-buffer "*nrepl-messages conn-id*")
      (let ((b1 (current-buffer)))
        (with-temp-buffer
          (should (not (equal (current-buffer) b1)))
          (cider-invoke-selector-method-by-key ?m)
          (should (equal (current-buffer) b1)))))))
