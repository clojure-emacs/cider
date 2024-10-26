;;; cider-cheatsheet.el --- Quick reference for Clojure        -*- lexical-binding: t -*-

;; Copyright © 2019-2024 Kris Jenkins, Bozhidar Batsov and CIDER contributors
;;
;; Author: Kris Jenkins <krisajenkins@gmail.com>
;;         Kato Muso <m@katomuso.io>

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; This file is not part of GNU Emacs.

;;; Commentary:

;; A quick reference system for Clojure.  Fast, searchable & available offline.

;;; Code:

(require 'cider-doc)
(require 'cl-lib)
(require 'map)
(require 'seq)
(require 'subr-x)

(defgroup cider-cheatsheet nil
  "Clojure cheatsheet in CIDER."
  :prefix "cider-cheatsheet-"
  :group 'cider)

(defconst cider-cheatsheet-buffer "*cider-cheatsheet*")

(defcustom cider-cheatsheet-auto-select-buffer t
  "Whether to auto-select the cheatsheet popup buffer."
  :type 'boolean
  :package-version '(cider . "1.15.0"))

(defcustom cider-cheatsheet-default-action-function #'cider-doc-lookup
  "Function to use on a var when it is selected.

By default, documentation for a var is displayed using `cider-doc-lookup`,
but it can also be set to `cider-clojuredocs-lookup` to show documentation
from ClojureDocs or any other function accepting a var as an argument."
  :type '(choice (const cider-doc-lookup)
                 (const cider-clojuredocs-lookup)
                 function)
  :package-version '(cider . "1.15.0"))

(defconst cider-cheatsheet-hierarchy
  '(("Documentation"
     ("REPL"
      (clojure.repl doc find-doc apropos dir source pst)
      (clojure.java.javadoc javadoc)))
    ("Primitives"
     ("Numbers"
      ("Arithmetic"
       (clojure.core + - * / quot rem mod inc dec max min abs)
       (clojure.math floor-div floor-mod ceil floor rint round pow sqrt cbrt E exp expm1 log log10 log1p PI sin cos tan asin acos atan atan2))
      ("Arbitrary Precision Arithmetic"
       (clojure.core +\' -\' *\' inc\' dec\'))
      ("Compare"
       (clojure.core == < > <= >= compare))
      ("Bitwise"
       (clojure.core bit-and bit-or bit-xor bit-not bit-flip bit-set bit-shift-right bit-shift-left bit-and-not bit-clear bit-test unsigned-bit-shift-right))
      ("Cast"
       (clojure.core byte short int long float double bigdec bigint num rationalize biginteger))
      ("Test"
       (clojure.core zero? pos? neg? even? odd? number? rational? integer? ratio? decimal? float? double? int? nat-int? neg-int? pos-int? NaN? infinite?))
      ("Random"
       (clojure.core rand rand-int)
       (clojure.math random))
      ("BigDecimal"
       (clojure.core with-precision))
      ("Unchecked"
       (clojure.core *unchecked-math* unchecked-add unchecked-dec unchecked-inc unchecked-multiply unchecked-negate unchecked-subtract))
      ("Ratios"
       (clojure.core numerator denominator ratio?)))
     ("Strings"
      ("Create"
       (clojure.core str format))
      ("Use"
       (clojure.core count get subs compare parse-boolean parse-double parse-long parse-uuid)
       (clojure.string join escape split split-lines replace replace-first reverse index-of last-index-of))
      ("Regex"
       (clojure.core re-find re-seq re-matches re-pattern re-matcher re-groups)
       (clojure.string replace replace-first re-quote-replacement))
      ("Letters"
       (clojure.string capitalize lower-case upper-case))
      ("Trim"
       (clojure.string trim trim-newline triml trimr))
      ("Test"
       (clojure.core string?)
       (clojure.string blank? starts-with? ends-with? includes?)))
     ("Other"
      ("Characters"
       (clojure.core char char? char-name-string char-escape-string))
      ("Keywords"
       (clojure.core keyword keyword? find-keyword))
      ("Symbols"
       (clojure.core symbol symbol? gensym))))
    ("Collections"
     ("Generic ops"
      (clojure.core count empty not-empty into conj bounded-count))
     ("Tree walking"
      (clojure.walk walk prewalk prewalk-demo prewalk-replace postwalk postwalk-demo postwalk-replace keywordize-keys stringify-keys))
     ("Content tests"
      (clojure.core distinct? empty? every? not-every? some not-any?))
     ("Capabilities"
      (clojure.core sequential? associative? sorted? counted? reversible?))
     ("Type tests"
      (clojure.core coll? list? vector? set? map? seq? record? map-entry?))
     ("Lists"
      ("Create"
       (clojure.core list list*))
      ("Examine"
       (clojure.core first nth peek))
      ("Change"
       (clojure.core cons conj rest pop)))
     ("Vectors"
      ("Create"
       (clojure.core vector vec vector-of mapv filterv))
      ("Examine"
       (clojure.core nth get peek))
      ("Change"
       (clojure.core assoc assoc-in pop subvec replace conj rseq update update-in))
      ("Ops"
       (clojure.core reduce-kv)))
     ("Sets"
      ("Create unsorted"
       (clojure.core set hash-set))
      ("Create sorted"
       (clojure.core sorted-set sorted-set-by)
       (clojure.data.avl sorted-set sorted-set-by)
       (clojure.data.int-map int-set dense-int-set))
      ("Examine"
       (clojure.core get contains?))
      ("Change"
       (clojure.core conj disj))
      ("Set ops"
       (clojure.set union difference intersection select))
      ("Test"
       (clojure.set subset? superset?))
      ("Sorted sets"
       (clojure.core rseq subseq rsubseq)))
     ("Maps"
      ("Create unsorted"
       (clojure.core hash-map array-map zipmap bean frequencies group-by)
       (clojure.set index))
      ("Create sorted"
       (clojure.core sorted-map sorted-map-by)
       (clojure.data.avl sorted-map sorted-map-by)
       (clojure.data.priority-map priority-map)
       (clojure.data.int-map int-map))
      ("Examine"
       (clojure.core get get get-in contains? find keys vals))
      ("Change"
       (clojure.core assoc assoc-in dissoc merge merge-with select-keys update update-in update-keys update-vals)
       (clojure.set rename-keys map-invert))
      ("Ops"
       (clojure.core reduce-kv))
      ("Entry"
       (clojure.core key val))
      ("Sorted maps"
       (clojure.core rseq subseq rsubseq)))
     ("Queues"
      ("Examine"
       (clojure.core peek))
      ("Change"
       (clojure.core conj pop)))
     ("Relations"
      ("Relational algebra"
       (clojure.set join select project union difference intersection index rename)))
     ("Transients"
      ("Create"
       (clojure.core transient persistent!))
      ("Change"
       (clojure.core conj! pop! assoc! dissoc! disj!)))
     ("Misc"
      ("Compare"
       (clojure.core = identical? not= not compare)
       (clojure.data diff))
      ("Test"
       (clojure.core true? false? instance? nil? some?)))
     ("Hashes"
      (clojure.core hash hash-ordered-coll hash-unordered-coll mix-collection-hash)))
    ("Sequences"
     ("Creating a Lazy Seq"
      ("From collection"
       (clojure.core seq vals keys rseq subseq rsubseq sequence))
      ("From producer fn"
       (clojure.core lazy-seq repeatedly iterate iteration))
      ("From constant"
       (clojure.core repeat range))
      ("From other"
       (clojure.core file-seq line-seq resultset-seq re-seq tree-seq xml-seq iterator-seq enumeration-seq))
      ("From seq"
       (clojure.core keep keep-indexed)))
     ("Seq in, Seq out"
      ("Get shorter"
       (clojure.core distinct filter remove take-nth for dedupe random-sample))
      ("Get longer"
       (clojure.core cons conj concat lazy-cat mapcat cycle interleave interpose))
      ("Tail-items"
       (clojure.core rest nthrest next fnext nnext drop drop-while take-last for))
      ("Head-items"
       (clojure.core take take-while butlast drop-last for))
      ("Change"
       (clojure.core conj concat distinct flatten group-by partition partition-all partition-by split-at split-with filter remove replace shuffle))
      ("Rearrange"
       (clojure.core reverse sort sort-by compare))
      ("Process items"
       (clojure.core map pmap map-indexed mapcat for replace seque)))
     ("Using a Seq"
      ("Extract item"
       (clojure.core first second last rest next ffirst nfirst fnext nnext nth nthnext rand-nth when-first max-key min-key))
      ("Construct coll"
       (clojure.core zipmap into reduce reductions set vec into-array to-array-2d mapv filterv))
      ("Pass to fn"
       (clojure.core apply))
      ("Search"
       (clojure.core some filter))
      ("Force evaluation"
       (clojure.core doseq dorun doall run!))
      ("Check for forced"
       (clojure.core realized?))))
    ("Transducers"
     ("Off the shelf"
      (clojure.core map mapcat filter remove take take-while take-nth drop drop-while replace partition-by partition-all keep keep-indexed map-indexed distinct interpose cat dedupe random-sample halt-when))
     ("Create your own"
      (clojure.core completing ensure-reduced unreduced))
     ("Use"
      (clojure.core into sequence transduce eduction))
     ("Early termination"
      (clojure.core reduced reduced? deref)))
    ("Spec"
     ("Operations"
      (clojure.spec.alpha valid? conform unform explain explain-data explain-str explain-out form describe assert check-asserts check-asserts?))
     ("Generator ops"
      (clojure.spec.alpha gen exercise exercise-fn))
     ("Defn & Registry"
      (clojure.spec.alpha def fdef registry get-spec spec? spec with-gen))
     ("Logical"
      (clojure.spec.alpha and or))
     ("Collection"
      (clojure.spec.alpha coll-of map-of every every-kv keys merge))
     ("Regex"
      (clojure.spec.alpha cat alt * + \? & keys*))
     ("Range"
      (clojure.spec.alpha int-in inst-in double-in int-in-range? inst-in-range?))
     ("Other"
      (clojure.spec.alpha nilable multi-spec fspec conformer))
     ("Custom explain"
      (clojure.spec.alpha explain-printer *explain-out*))
     ("Predicates with test.check generators"
      ("Numbers"
       (clojure.core number? rational? integer? ratio? decimal? float? zero? double? int? nat-int? neg-int? pos-int?))
      ("Symbols, keywords"
       (clojure.core keyword? symbol? ident? qualified-ident? qualified-keyword? qualified-symbol? simple-ident? simple-keyword? simple-symbol?))
      ("Scalars"
       (clojure.core string? true? false? nil? some? boolean? bytes? inst? uri? uuid?))
      ("Collections"
       (clojure.core list? map? set? vector? associative? coll? sequential? seq? empty? indexed? seqable?))
      ("Other"
       (clojure.core any?))))
    ("IO"
     ("To/from ..."
      (clojure.core spit slurp))
     ("To *out*"
      (clojure.core pr prn print printf println newline)
      (clojure.pprint print-table))
     ("To writer"
      (clojure.pprint pprint cl-format))
     ("To string"
      (clojure.core format with-out-str pr-str prn-str print-str println-str))
     ("From *in*"
      (clojure.core read-line)
      (clojure.edn read)
      (clojure.tools.reader.edn read))
     ("From reader"
      (clojure.core line-seq)
      (clojure.edn read)
      (clojure.tools.reader.edn read))
     ("From string"
      (clojure.core with-in-str)
      (clojure.edn read-string)
      (clojure.tools.reader.edn read-string))
     ("Open"
      (clojure.core with-open)
      (clojure.java.io reader writer input-stream output-stream))
     ("Misc"
      (clojure.core flush file-seq *in* *out* *err*)
      (clojure.java.io file copy delete-file resource as-file as-url as-relative-path make-parents))
     ("Data readers"
      (clojure.core *data-readers* default-data-readers *default-data-reader-fn*))
     ("Tap"
      (clojure.core tap> add-tap remove-tap))
     ("Interop"
      (clojure.java.io make-writer make-reader make-output-stream make-input-stream)))
    ("Functions"
     ("Create"
      (:special fn)
      (clojure.core defn defn- definline identity constantly memfn comp complement partial juxt memoize fnil every-pred some-fn))
     ("Call"
      (clojure.core apply -> ->> trampoline as-> cond-> cond->> some-> some->>))
     ("Test"
      (clojure.core fn? ifn?)))
    ("Abstractions"
     ("Protocols"
      ("Define"
       (clojure.core defprotocol))
      ("Extend"
       (clojure.core extend-type))
      ("Reify"
       (clojure.core reify))
      ("Test"
       (clojure.core satisfies? extends?))
      ("Other"
       (clojure.core extend extend-protocol extenders)))
     ("Records"
      ("Define"
       (clojure.core defrecord))
      ("Test"
       (clojure.core record?)))
     ("Types"
      ("Define"
       (clojure.core deftype))
      ("With methods"
       (clojure.core deftype)))
     ("Multimethods"
      ("Define"
       (clojure.core defmulti))
      ("Method define"
       (clojure.core defmethod))
      ("Dispatch"
       (clojure.core get-method methods))
      ("Remove"
       (clojure.core remove-method remove-all-methods))
      ("Prefer"
       (clojure.core prefer-method prefers))
      ("Relation"
       (clojure.core derive underive isa? parents ancestors descendants make-hierarchy))))
    ("Macros"
     ("Create"
      (clojure.core defmacro definline))
     ("Debug"
      (clojure.core macroexpand-1 macroexpand)
      (clojure.walk macroexpand-all))
     ("Branch"
      (clojure.core and or when when-not when-let when-first if-not if-let cond condp case when-some if-some))
     ("Loop"
      (clojure.core for doseq dotimes while))
     ("Arrange"
      (clojure.core .. doto -> ->> as-> cond-> cond->> some-> some->>))
     ("Scope"
      (clojure.core binding locking time with-in-str with-local-vars with-open with-out-str with-precision with-redefs with-redefs-fn))
     ("Lazy"
      (clojure.core lazy-cat lazy-seq delay))
     ("Doc"
      (clojure.core assert comment)
      (clojure.repl doc)))
    ("Metadata"
     (clojure.core meta with-meta vary-meta alter-meta! reset-meta! test)
     (clojure.repl doc find-doc))
    ("Special Forms"
     ("General"
      (:special def if do let letfn quote var fn loop recur set! throw try monitor-enter monitor-exit))
     ("Binding Forms / Destructuring"
      (:special let fn loop)
      (clojure.core defn defmacro for doseq if-let when-let if-some when-some)))
    ("Vars and global environment"
     ("Def variants"
      (:special def)
      (clojure.core defn defn- definline defmacro defmethod defmulti defonce defrecord))
     ("Interned vars"
      (clojure.core declare intern binding find-var)
      (:special var))
     ("Var objects"
      (clojure.core with-local-vars var-get var-set alter-var-root var? bound? thread-bound?))
     ("Var validators"
      (clojure.core set-validator! get-validator)))
    ("Namespaces"
     ("Current"
      (clojure.core *ns*))
     ("Create/Switch"
      (clojure.core ns in-ns create-ns))
     ("Add"
      (clojure.core alias import intern refer)
      (:special def))
     ("Find"
      (clojure.core all-ns find-ns))
     ("Examine"
      (clojure.core ns-name ns-aliases ns-map ns-interns ns-publics ns-refers ns-imports))
     ("From symbol"
      (clojure.core resolve ns-resolve namespace the-ns requiring-resolve))
     ("Remove"
      (clojure.core ns-unalias ns-unmap remove-ns)))
    ("Loading"
     ("Load libs"
      (clojure.core require use import refer))
     ("List loaded"
      (clojure.core loaded-libs))
     ("Load misc"
      (clojure.core load load-file load-reader load-string)))
    ("Concurrency"
     ("Atoms"
      (clojure.core atom swap! reset! compare-and-set! swap-vals! reset-vals!))
     ("Futures"
      (clojure.core future future-call future-done? future-cancel future-cancelled? future?))
     ("Threads"
      (clojure.core bound-fn bound-fn* get-thread-bindings push-thread-bindings pop-thread-bindings thread-bound?))
     ("Volatiles"
      (clojure.core volatile! vreset! vswap! volatile?))
     ("Misc"
      (clojure.core locking pcalls pvalues pmap seque promise deliver))
     ("Refs and Transactions"
      ("Create"
       (clojure.core ref))
      ("Examine"
       (clojure.core deref))
      ("Transaction"
       (clojure.core sync dosync io!))
      ("In transaction"
       (clojure.core ensure ref-set alter commute))
      ("Validators"
       (clojure.core set-validator! get-validator))
      ("History"
       (clojure.core ref-history-count ref-min-history ref-max-history)))
     ("Agents and Asynchronous Actions"
      ("Create"
       (clojure.core agent))
      ("Examine"
       (clojure.core agent-error))
      ("Change state"
       (clojure.core send send-off restart-agent send-via set-agent-send-executor! set-agent-send-off-executor!))
      ("Block waiting"
       (clojure.core await await-for))
      ("Ref validators"
       (clojure.core set-validator! get-validator))
      ("Watchers"
       (clojure.core add-watch remove-watch))
      ("Thread handling"
       (clojure.core shutdown-agents))
      ("Error"
       (clojure.core error-handler set-error-handler! error-mode set-error-mode!))
      ("Misc"
       (clojure.core *agent* release-pending-sends))))
    ("Java Interoperation"
     ("General"
      (clojure.core .. doto bean comparator enumeration-seq import iterator-seq memfn class class? bases supers type gen-class gen-interface definterface)
      (:special new set!))
     ("Cast"
      (clojure.core boolean byte short char int long float double bigdec bigint num cast biginteger))
     ("Exceptions"
      (:special throw try catch finally)
      (clojure.repl pst)
      (clojure.core ex-info ex-data Throwable->map StackTraceElement->vec ex-cause ex-message)
      (clojure.main ex-triage ex-str err->msg report-error))
     ("Arrays"
      ("Create"
       (clojure.core make-array object-array boolean-array byte-array short-array char-array int-array long-array float-array double-array aclone to-array to-array-2d into-array))
      ("Use"
       (clojure.core aget aset aset-boolean aset-byte aset-short aset-char aset-int aset-long aset-float aset-double alength amap areduce))
      ("Cast"
       (clojure.core booleans bytes shorts chars ints longs floats doubles)))
     ("Proxy"
      ("Create"
       (clojure.core proxy get-proxy-class construct-proxy init-proxy))
      ("Misc"
       (clojure.core proxy-mappings proxy-super update-proxy))))
    ("Zippers"
     ("Create"
      (clojure.zip zipper seq-zip vector-zip xml-zip))
     ("Get loc"
      (clojure.zip up down left right leftmost rightmost))
     ("Get seq"
      (clojure.zip lefts rights path children))
     ("Change"
      (clojure.zip make-node replace edit insert-child insert-left insert-right append-child remove))
     ("Move"
      (clojure.zip next prev))
     ("Misc"
      (clojure.zip root node branch? end?))
     ("XML"
      (clojure.data.zip.xml attr attr= seq-test tag= text text= xml-> xml1->)))
    ("Other"
     ("XML"
      (clojure.xml parse)
      (clojure.core xml-seq))
     ("REPL"
      (clojure.core *1 *2 *3 *e *print-dup* *print-length* *print-level* *print-meta* *print-readably*))
     ("Code"
      (clojure.core *compile-files* *compile-path* *file* *warn-on-reflection* compile loaded-libs test))
     ("Misc"
      (clojure.core eval force hash *clojure-version* clojure-version *command-line-args* random-uuid))
     ("Browser / Shell"
      (clojure.java.browse browse-url)
      (clojure.java.shell sh with-sh-dir with-sh-env))
     ("EDN"
      (clojure.edn read read-string))
     ("Pretty Printing"
      (clojure.pprint pprint print-table pp *print-right-margin*))
     ("Compiling Code & Class Generation"
      (clojure.core *compile-files* *compile-path* *file* *warn-on-reflection* compile gen-class gen-interface loaded-libs test)))
    ("Reader Conditionals"
     (clojure.core reader-conditional reader-conditional? tagged-literal tagged-literal?))
    ("Unit Tests"
     ("Defining"
      (clojure.test deftest deftest- testing is are))
     ("Running"
      (clojure.test run-tests run-all-tests test-vars))
     ("Fixtures"
      (clojure.test use-fixtures join-fixtures compose-fixtures)))
    ("Async"
     ("Main"
      (clojure.core.async go go-loop <! <!! >! >!! chan put! take take! close! timeout offer! poll! promise-chan))
     ("Choice"
      (clojure.core.async alt! alt!! alts! alts!! do-alts))
     ("Buffering"
      (clojure.core.async buffer dropping-buffer sliding-buffer unblocking-buffer?))
     ("Pipelines"
      (clojure.core.async pipeline pipeline-async pipeline-blocking))
     ("Threading"
      (clojure.core.async thread thread-call))
     ("Mixing"
      (clojure.core.async admix solo-mode mix unmix unmix-all toggle merge pipe unique))
     ("Multiples"
      (clojure.core.async mult tap untap untap-all))
     ("Publish/Subscribe"
      (clojure.core.async pub sub unsub unsub-all))
     ("Higher Order"
      (clojure.core.async filter< filter> map map< map> mapcat< mapcat> partition partition-by reduce remove< remove> split))
     ("Pre-Populate"
      (clojure.core.async into onto-chan to-chan))))
  "A data structure for Clojure cheatsheet information.

It's a tree, where the head of each list determines the context of the rest
of the list.  The head may be:

  - A string, in which case it's a (sub)heading for the rest of the items.

  - A symbol, in which case it's the Clojure namespace of the symbols that
    follow it.

  - The keyword :special, in which case it's a Clojure special form

  - Any other keyword, in which case it's a typed item that will be passed
    through.

Note that some Clojure symbols appear in more than once.  This is entirely
intentional.  For instance, `map` belongs in the sections on collections
and transducers.")

(defun cider-cheatsheet--expand-vars (list)
  "Expand the symbols in LIST to fully-qualified var names.

This list is supposed to have the following format:

  (my-ns var1 var2 var3)"
  (let ((ns (car list))
        (vars (cdr list)))
    (if (eq ns :special)
        (mapcar #'symbol-name vars)
      (mapcar (lambda (var) (format "%s/%s" ns var)) vars))))

(defun cider-cheatsheet--flatten-hierarchy (hierarchy &optional sections)
  "Transform HIERARCHY to lists each representing a path with SECTIONS before var."
  (seq-mapcat (lambda (node)
                (if (stringp (car node))
                    (cider-cheatsheet--flatten-hierarchy (cdr node) (cons (car node) sections))
                  (mapcar (lambda (var) (reverse (cons var sections)))
                          (cider-cheatsheet--expand-vars node))))
              hierarchy))

;;;###autoload
(defun cider-cheatsheet-select (&optional flat)
  "Navigate cheatsheet sections and show documentation for selected var.

With a prefix argument FLAT, represent each candidate as a full path to var."
  (interactive "P")
  (if flat
      (let* ((hierarchy (cider-cheatsheet--flatten-hierarchy cider-cheatsheet-hierarchy))
             (paths (mapcar (lambda (sections) (string-join sections " > ")) hierarchy))
             (path (completing-read "Select path: " paths))
             (var (car (last (split-string path " > ")))))
        (funcall cider-cheatsheet-default-action-function var))
    (let ((hierarchy cider-cheatsheet-hierarchy))
      (while (stringp (caar hierarchy))
        (let* ((sections (mapcar #'car hierarchy))
               (section (completing-read "Select section: " sections)))
          (setq hierarchy (map-elt hierarchy section))))
      (let* ((vars (seq-mapcat #'cider-cheatsheet--expand-vars hierarchy))
             (var (completing-read "Select var: " vars)))
        (funcall cider-cheatsheet-default-action-function var)))))

(cl-defun cider-cheatsheet--insert-hierarchy (hierarchy &optional (level 0))
  "Insert HIERARCHY with visual indentation for LEVEL."
  (dolist (node hierarchy)
    (if (stringp (car node))
        (progn
          (insert (make-string (* level 2) ?\s) (car node) "\n")
          (cider-cheatsheet--insert-hierarchy (cdr node) (1+ level)))
      (dolist (var (cider-cheatsheet--expand-vars node))
        (insert (make-string (* level 2) ?\s))
        (insert-text-button var
                            'var var
                            'action (lambda (btn)
                                      (funcall cider-cheatsheet-default-action-function
                                               (button-get btn 'var)))
                            'help-echo (format "Show documentation for %s" var))
        (insert "\n")))))

(defun cider-cheatsheet--buffer-contents ()
  "Generate cheatsheet buffer contents based on the cheatsheet hierarchy."
  (with-temp-buffer
    (cider-cheatsheet--insert-hierarchy cider-cheatsheet-hierarchy)
    (buffer-string)))

;;;###autoload
(defun cider-cheatsheet ()
  "Display cheatsheet in a popup buffer."
  (interactive)
  (with-current-buffer (cider-popup-buffer cider-cheatsheet-buffer
                                           cider-cheatsheet-auto-select-buffer)
    (read-only-mode -1)
    (insert (cider-cheatsheet--buffer-contents))
    (read-only-mode 1)
    (goto-char (point-min))))

(provide 'cider-cheatsheet)

;;; cider-cheatsheet.el ends here
