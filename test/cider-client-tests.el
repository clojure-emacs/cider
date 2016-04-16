(require 'buttercup)
(require 'cider)
(require 'cider-client)

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


(describe "cider-current-connection"

  (describe "when there are no active connections"
    :var (cider-connections)
    (it "returns nil"
      (setq cider-connections nil)
      (expect (cider-current-connection) :not :to-be-truthy)
      (expect (cider-current-connection "clj") :not :to-be-truthy)
      (expect (cider-current-connection "cljs") :not :to-be-truthy)))


  (describe "when active connections are available"

    (it "always returns the latest connection"
      (with-connection-buffer "clj" bb1
        (with-connection-buffer "cljs" bb2
          (with-connection-buffer "clj" b1
            (with-connection-buffer "cljs" b2
              (expect (cider-current-connection) :to-equal b2)

              ;; follows type arguments
              (expect (cider-current-connection "clj") :to-equal b1)
              (expect (cider-current-connection "cljs") :to-equal b2)

              ;; follows file type
              (with-temp-buffer
                (setq major-mode 'clojure-mode)
                (expect (cider-current-connection) :to-equal b1))

              (with-temp-buffer
                (setq major-mode 'clojurescript-mode)
                (expect (cider-current-connection) :to-equal b2)))))))

    (describe "when type argument is given"
      (describe "when connection of that type exists"
        (it "returns that connection buffer"
          ;; for clj
          (with-connection-buffer "clj" b1
            (with-connection-buffer "cljs" b2
              (expect (cider-current-connection "clj") :to-equal b1)))
          ;; for cljs
          (with-connection-buffer "cljs" b1
            (with-connection-buffer "clj" b2
              (expect (cider-current-connection "cljs") :to-equal b1)))))

      (describe "when connection of that type doesn't exists"
        (it "returns nil"
          ;; for clj
          (with-connection-buffer "cljs" b1
            (expect (cider-current-connection "clj") :to-equal nil))

          ;; for cljs
          (with-connection-buffer "clj" b2
            (expect (cider-current-connection "cljs") :to-equal nil)))))

    (describe "when type argument is not given"
      (describe "when a connection matching current file extension exists"
        (it "returns that connection buffer"
          ;; for clj
          (with-connection-buffer "clj" b1
            (with-connection-buffer "cljs" b2
              (with-temp-buffer
                (setq major-mode 'clojure-mode)
                (expect (cider-current-connection) :to-equal b1))))

          ;; for cljs
          (with-connection-buffer "cljs" b1
            (with-connection-buffer "clj" b2
              (with-temp-buffer
                (setq major-mode 'clojurescript-mode)
                (expect (cider-current-connection) :to-equal b1))))))

      (describe "when a connection matching current file extension doesn't exist"
        (it "returns the latest connection buffer"
          ;; for clj
          (with-connection-buffer "clj" b1
            (with-temp-buffer
              (setq major-mode 'clojurescript-mode)
              (expect (cider-current-connection) :to-equal b1)))

          ;; for cljs
          (with-connection-buffer "cljs" b2
            (with-temp-buffer
              (setq major-mode 'clojure-mode)
              (expect (cider-current-connection) :to-equal b2))))))))


(describe "cider-other-connection"
  (describe "when there are no active connections"
    :var (cider-connections)
    (it "returns nil"
      (setq cider-connections nil)
      (expect (cider-other-connection) :to-equal nil)))

  (describe "when there is only 1 active connection"
    (it "returns nil"
      ;; for clj
      (with-connection-buffer "clj" b1
        (expect (cider-other-connection) :to-equal nil)
        (expect (cider-other-connection b1) :to-equal nil))
      ;; for cljs
      (with-connection-buffer "cljs" b1
        (expect (cider-other-connection) :to-equal nil)
        (expect (cider-other-connection b1) :to-equal nil))))

  (describe "when active connections are available"
    (describe "when a connection of other type doesn't exist"
      (it "returns nil"
        ;; for clj
        (with-connection-buffer "clj" b1
          (with-connection-buffer "clj" b2
            (expect (cider-other-connection) :to-equal nil)
            (expect (cider-other-connection b1) :to-equal nil)
            (expect (cider-other-connection b2) :to-equal nil)))
        ;; for cljs
        (with-connection-buffer "cljs" b1
          (with-connection-buffer "cljs" b2
            (expect (cider-other-connection) :to-equal nil)
            (expect (cider-other-connection b1) :to-equal nil)
            (expect (cider-other-connection b2) :to-equal nil)))))

    (describe "when a connection of other type exists"
      (it "returns that connection"
        (with-connection-buffer "clj" b1
          (with-connection-buffer "cljs" b2
            (expect (cider-other-connection) :to-equal b1)
            (expect (cider-other-connection b1) :to-equal b2)
            (expect (cider-other-connection b2) :to-equal b1)))))

    (describe "when there are multiple active connections"
      (it "always returns the latest connection"

        (with-connection-buffer "clj" bb1
          (with-connection-buffer "cljs" bb2
            (with-connection-buffer "clj" b1
              (with-connection-buffer "cljs" b2
                (expect (cider-other-connection) :to-equal b1)
                (expect (cider-other-connection b1) :to-equal b2)
                (expect (cider-other-connection b2) :to-equal b1)
                ;; older connections still work
                (expect (cider-other-connection bb1) :to-equal b2)
                (expect (cider-other-connection bb2) :to-equal b1)))))))))
