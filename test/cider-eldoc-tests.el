(require 'buttercup)
(require 'cider-eldoc)

(describe "cider--find-rest-args-position"
  (it "returns the position of & in the arglist vector"
    (expect (cider--find-rest-args-position ["fmt" "&" "arg"])
            :to-equal 1)
    (expect (cider--find-rest-args-position ["fmt" "arg"])
            :to-equal nil)))

(describe "cider-eldoc-format-thing"
  (describe "when ns is given and it exists"
    (it "returns formatted eldoc strings of form ns/symbol"
      (expect (cider-eldoc-format-thing "clojure.core" "map" "map")
              :to-equal "clojure.core/map"))
    (describe "when the given ns doesnt exist"
      (it "returns eldoc formatted symbol"
        (let ((cider-eldoc-ns-function (lambda () nil)))
          (expect (cider-eldoc-format-thing "non-existent-ns" "" "my-map")
                  :to-equal "my-map")
          (expect (cider-eldoc-format-thing "" "" "my-map")
                  :to-equal "my-map")))))
  (describe "when ns is not given or it is a Java interop form"
    (it "returns eldoc formatted thing"
      (expect (cider-eldoc-format-thing "" "" ".toString")
              :to-equal ".toString"))))


(describe "cider-eldoc-beginning-of-sexp"
  (it "moves to the beginning of the sexp"
    (with-temp-buffer
      (save-excursion
        (insert "(a (b b) (c c) d)"))
      (search-forward "d")
      (cider-eldoc-beginning-of-sexp)
      (expect (point) :to-equal 2)
      (expect (char-after) :to-equal ?a)))

  (it "returns the number sexp the point was over or after"
    (with-temp-buffer
      (save-excursion
        (insert "(a (b b) (c c) d)"))
      (search-forward "d")
      (expect (cider-eldoc-beginning-of-sexp) :to-equal 4)
      (search-forward "a")
      (expect (cider-eldoc-beginning-of-sexp) :to-equal 1)))

  (it "returns nil if the maximum number of sexps to skip is exceeded"
    (with-temp-buffer
      (save-excursion
        (insert "(a (b b) (c c) d)"))
      (search-forward "d")
      (let ((cider-eldoc-max-num-sexps-to-skip 2))
        (expect (cider-eldoc-beginning-of-sexp) :to-equal nil)
        (expect (point) :to-equal 4)))))
