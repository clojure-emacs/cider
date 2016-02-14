(require 'cider)
(require 'cider-client)
(require 'ert)

;;; cider-client tests

(defmacro with-connection-buffer (type symbol &rest body)
  "Run BODY in a temp buffer, with the given repl TYPE.
SYMBOL is locally let-bound to the current buffer."
  (declare (indent 2)
           (debug (sexp sexp &rest form)))
  `(with-temp-buffer
     (setq major-mode 'cider-repl-mode)
     (setq cider-repl-type ,type)
     ;; `with-current-buffer' doesn't bump the buffer up the list.
     (switch-to-buffer (current-buffer))
     (rename-buffer (format "*cider-repl %s-%s*" ,type (random 10000)) t)
     (let ((cider-connections (cons (current-buffer) cider-connections))
           (,symbol (current-buffer)))
       ,@body)))

(ert-deftest cider-current-connection-nil-case ()
  (let ((cider-connections nil))
    (should-not (cider-current-connection))
    (should-not (cider-current-connection "clj"))
    (should-not (cider-current-connection "cljs"))))

(ert-deftest cider-current-connection-follow-type-argument ()
  (with-connection-buffer "clj" b1
    (should (equal (cider-current-connection) b1))
    (should (equal (cider-current-connection "clj") b1))
    (should-not (cider-current-connection "cljs"))
    (with-connection-buffer "cljs" b2
      (should (equal (cider-current-connection) b2))
      (should (equal (cider-current-connection "cljs") b2))
      (should (equal (cider-current-connection "clj") b1))
      (with-connection-buffer "cljs" b3
        (should (equal (cider-current-connection) b3))
        (should (equal (cider-current-connection "cljs") b3))
        (should (equal (cider-current-connection "clj") b1)))
      (with-connection-buffer "clj" b4
        (should (equal (cider-current-connection) b4))
        (should (equal (cider-current-connection "cljs") b2))
        (should (equal (cider-current-connection "clj") b4))))))

(ert-deftest cider-current-connection-follow-file-extension ()
  ;; A single connection buffer.
  (with-connection-buffer "clj" b1
    (with-temp-buffer
      (setq major-mode 'clojure-mode)
      (should (equal (cider-current-connection) b1))
      (should (equal (cider-current-connection "clj") b1))
      (should-not (cider-current-connection "cljs")))
    (with-temp-buffer
      ;; No cljs repl exists, but the TYPE argument is nil, so the
      ;; next-best-thing is returned.
      (setq major-mode 'clojurescript-mode)
      (should (equal (cider-current-connection) b1))
      (should (equal (cider-current-connection "clj") b1))
      (should-not (cider-current-connection "cljs"))))
  (with-connection-buffer "cljs" b1
    (with-temp-buffer
      (setq major-mode 'clojure-mode)
      ;; No clj repl exists, but the TYPE argument is nil, so the
      ;; next-best-thing is returned.
      (should (equal (cider-current-connection) b1))
      (should (equal (cider-current-connection "cljs") b1))
      (should-not (cider-current-connection "clj")))
    (with-temp-buffer
      (setq major-mode 'clojurescript-mode)
      (should (equal (cider-current-connection) b1))
      (should (equal (cider-current-connection "cljs") b1))
      (should-not (cider-current-connection "clj"))))
  ;; Multiple connection buffers.
  (with-connection-buffer "clj" bb1
    (with-connection-buffer "cljs" bb2
      ;; The two buffers above are not used. We just want to ensure we are
      ;; returning the most recent connection in case of ambiguity.
      (with-connection-buffer "clj" b1
        (with-connection-buffer "cljs" b2
          (with-temp-buffer
            (setq major-mode 'clojure-mode)
            ;; TYPE argument is nil, a clj connection exists and we're in
            ;; clojure-mode, so we return the clj connection even though the top
            ;; connection is cljs.
            (should (equal (cider-current-connection) b1))
            (should (equal (cider-current-connection "clj") b1))
            (should (equal (cider-current-connection "cljs") b2)))
          (with-temp-buffer
            (setq major-mode 'clojurescript-mode)
            ;; A cljs repl exists, and the TYPE argument is nil.
            (should (equal (cider-current-connection) b2))
            (should (equal (cider-current-connection "clj") b1))
            (should (equal (cider-current-connection "cljs") b2))))))))

(ert-deftest cider-other-connection ()
  (let ((cider-connections nil))
    (should-not (cider-other-connection))
    (with-connection-buffer "clj" b1
      (should-not (cider-other-connection))
      (should-not (cider-other-connection b1))
      (with-connection-buffer "cljs" b2
        (should (equal (cider-other-connection) b1))
        (should (equal (cider-other-connection b1) b2))
        (should (equal (cider-other-connection b2) b1))
        (with-connection-buffer "cljs" b3
          (should (equal (cider-other-connection) b1))
          (should (equal (cider-other-connection b1) b3))
          (should (equal (cider-other-connection b3) b1))
          (should (equal (cider-other-connection b2) b1)))
        (with-connection-buffer "clj" b4
          (should (equal (cider-other-connection) b2))
          (should (equal (cider-other-connection b4) b2))
          (should (equal (cider-other-connection b2) b4))
          (should (equal (cider-other-connection b1) b2)))))))
