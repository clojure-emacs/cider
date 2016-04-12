(require 'nrepl-client)

(ert-deftest dict-merge ()
  (let ((input '(dict 2 4 1 2 "10" "90" "a" "b")))
    (should (equal (nrepl-dict-merge input '(dict 1 3 "10" me))
                   '(dict 2 4 1 3 "10" me "a" "b")))
    (should (equal input '(dict 2 4 1 3 "10" me "a" "b"))))
  (should (equal (nrepl-dict-merge nil '(dict 1 3 "10" me))
                 '(dict 1 3 "10" me)))
  (should (equal (nrepl-dict-merge '(dict 1 3 "10" me) nil)
                 '(dict 1 3 "10" me))))

(ert-deftest nrepl-dict-contains-successful ()
  "Tests `nrepl-dict-contains' on inputs where this predicate function
should return non-nil. Of note, the `nil' value itself is allowed to be a
key in the nREPL dict."
  (let ((input '(dict 1 "a" 2 "B" "3" "d" 4 nil sym yes nil nil-val)))
    (should (nrepl-dict-contains input 1))
    (should (nrepl-dict-contains input 2))
    (should (nrepl-dict-contains input "3"))
    (should (nrepl-dict-contains input 4))
    (should (nrepl-dict-contains input 'sym))
    (should (nrepl-dict-contains input nil))))

(ert-deftest nrepl-dict-contains-failure ()
  "Tests `nrepl-dict-contains' on inputs where this predicate function
should return nil."
  (let ((input '(dict 1 "a" 2 "B" "3" "d" 4 nil sym yes)))
    (should (equal (nrepl-dict-contains input 11) nil))
    (should (equal (nrepl-dict-contains input 12) nil))
    (should (equal (nrepl-dict-contains input "13") nil))
    (should (equal (nrepl-dict-contains input 14) nil))
    (should (equal (nrepl-dict-contains input 'missing) nil))
    (should (equal (nrepl-dict-contains input nil) nil))))

(ert-deftest nrepl-dict-get-found ()
  "Tests `nrepl-dict-get' on inputs where the keys exist in the nREPL
dict. The `nil' value itself can be used as either a value or a key in the
nrepl-dict."
  (let ((input '(dict 1 "a" 2 "B" "3" "d" 4 nil sym yes nil nil-val)))
    (should (equal (nrepl-dict-get input 1) "a"))
    (should (equal (nrepl-dict-get input 2) "B"))
    (should (equal (nrepl-dict-get input "3") "d"))
    (should (equal (nrepl-dict-get input 4) nil))
    (should (equal (nrepl-dict-get input 'sym) 'yes))
    (should (equal (nrepl-dict-get input nil) 'nil-val))))

(ert-deftest nrepl-dict-get-missing ()
  "Tests `nrepl-dict-get' on inputs where the keys do not exist in the
nREPL dict."
  (let ((input '(dict 1 "a" 2 "B" "3" "d" 4 nil sym yes)))
    (should (equal (nrepl-dict-get input 11) nil))
    (should (equal (nrepl-dict-get input "13") nil))
    (should (equal (nrepl-dict-get input 14) nil))
    (should (equal (nrepl-dict-get input 'missing) nil))
    (should (equal (nrepl-dict-get input nil) nil))))

(ert-deftest nrepl-dict-get-found-with-default ()
  "Tests `nrepl-dict-get' on inputs where the keys exist in the nREPL dict,
while also passing in a default value. The `nil' value itself can be used
as either a key or a value in the nrepl-dict, but in these cases
`nrepl-dict-get' should NOT return the `default' value."
  (let ((input '(dict 1 "a" 2 "B" "3" "d" 4 nil sym yes nil nil-val)))
    (should (equal (nrepl-dict-get input 1 "default") "a"))
    (should (equal (nrepl-dict-get input 2 "default") "B"))
    (should (equal (nrepl-dict-get input "3" "default") "d"))
    (should (equal (nrepl-dict-get input 4 "default") nil))
    (should (equal (nrepl-dict-get input 'sym "default") 'yes))
    (should (equal (nrepl-dict-get input nil "default") 'nil-val))))

(ert-deftest nrepl-dict-get-missing-with-default ()
  "Tests `nrepl-dict-get' on inputs where the keys do not exist in the
nREPL dict, while also passing in a default value."
  (let ((input '(dict 1 "a" 2 "B" "3" "d" 4 nil sym yes)))
    (should (equal (nrepl-dict-get input 11 "default") "default"))
    (should (equal (nrepl-dict-get input 21 "default") "default"))
    (should (equal (nrepl-dict-get input "31" "default") "default"))
    (should (equal (nrepl-dict-get input 41 "default") "default"))
    (should (equal (nrepl-dict-get input 'missing "default") "default"))
    (should (equal (nrepl-dict-get input nil "default") "default"))))
