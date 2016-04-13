(require 'buttercup)
(require 'nrepl-client)

(describe "nrepl-dict-merge"
  :var (input)
  (before-each
    (setq input '(dict 2 4 1 2 "10" "90" "a" "b")))

  (it "merges dictionaries"
    (expect (nrepl-dict-merge input '(dict 1 3 "10" me)) :to-equal '(dict 2 4 1 3 "10" me "a" "b"))
    (expect input :to-equal '(dict 2 4 1 3 "10" me "a" "b")))

  (it "handles nil values"
    (expect (nrepl-dict-merge nil '(dict 1 3 "10" me)) :to-equal '(dict 1 3 "10" me))
    (expect (nrepl-dict-merge '(dict 1 3 "10" me) nil) :to-equal '(dict 1 3 "10" me))))

(describe "nrepl-dict-contains"
  :var (input)

  (it "returns non-nil if dict contains the element"
    (setq input '(dict 1 "a" 2 "B" "3" "d" 4 nil sym yes nil nil-val))

    (expect (nrepl-dict-contains input 1) :to-be-truthy)
    (expect (nrepl-dict-contains input 2) :to-be-truthy)
    (expect (nrepl-dict-contains input "3") :to-be-truthy)
    (expect (nrepl-dict-contains input 4) :to-be-truthy)
    (expect (nrepl-dict-contains input 'sym) :to-be-truthy))

  (it "allows `nil' to be a key in the nREPL dict"
    (setq input '(dict 1 "a" 2 "B" "3" "d" 4 nil sym yes nil nil-val))
    (expect (nrepl-dict-contains input nil) :to-be-truthy))

  (it "returns `nil' if dict doesnt contain the element"
    (setq input '(dict 1 "a" 2 "B" "3" "d" 4 nil sym yes))

    (expect (nrepl-dict-contains input 11) :to-equal nil)
    (expect (nrepl-dict-contains input 12) :to-equal nil)
    (expect (nrepl-dict-contains input "13") :to-equal nil)
    (expect (nrepl-dict-contains input 14) :to-equal nil)
    (expect (nrepl-dict-contains input 'missing) :to-equal nil)
    (expect (nrepl-dict-contains input nil) :to-equal nil)))

(describe "nrepl-dict-get"
  :var (input)

  (describe "when key is present in the dict"
    (before-all
      (setq input '(dict 1 "a" 2 "B" "3" "d" 4 nil sym yes nil nil-val)))

    (it "returns the corresponding value"
      (expect (nrepl-dict-get input 1) :to-equal "a")
      (expect (nrepl-dict-get input 2) :to-equal "B")
      (expect (nrepl-dict-get input "3") :to-equal "d")
      (expect (nrepl-dict-get input 4) :to-equal nil)
      (expect (nrepl-dict-get input 'sym) :to-equal'yes)
      (expect (nrepl-dict-get input nil) :to-equal 'nil-val))

    (it "ignores the default value, if given"
      (expect (nrepl-dict-get input 1 "default") :to-equal "a")
      (expect (nrepl-dict-get input 2 "default") :to-equal "B")
      (expect (nrepl-dict-get input "3" "default") :to-equal "d")
      (expect (nrepl-dict-get input 4 "default") :to-equal nil)
      (expect (nrepl-dict-get input 'sym "default") :to-equal'yes)
      (expect (nrepl-dict-get input nil "default") :to-equal 'nil-val)))

  (describe "when key is not present in the dict"
    (before-all
      (setq input '(dict 1 "a" 2 "B" "3" "d" 4 nil sym yes)))

    (it "returns nil, when default value is not given"
      (expect (nrepl-dict-get input 11) :to-equal nil)
      (expect (nrepl-dict-get input "13") :to-equal nil)
      (expect (nrepl-dict-get input 14) :to-equal nil)
      (expect (nrepl-dict-get input 'missing) :to-equal nil)
      (expect (nrepl-dict-get input nil) :to-equal nil))

    (it "returns the default value, if given"
      (expect (nrepl-dict-get input 11 "default") :to-equal "default")
      (expect (nrepl-dict-get input 21 "default") :to-equal "default")
      (expect (nrepl-dict-get input "31" "default") :to-equal "default")
      (expect (nrepl-dict-get input 41 "default") :to-equal "default")
      (expect (nrepl-dict-get input 'missing "default") :to-equal "default")
      (expect (nrepl-dict-get input nil "default") :to-equal "default"))))
