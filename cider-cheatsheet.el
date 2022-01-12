;;; cider-cheatsheet.el --- Quick reference for Clojure        -*- lexical-binding: t -*-

;; Copyright © 2019-2022 Kris Jenkins, Bozhidar Batsov and CIDER contributors
;;
;; Author: Kris Jenkins <krisajenkins@gmail.com>

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

;; Mostly taken from Kris Jenkins' `clojure-cheatsheet'
;; See: https://github.com/clojure-emacs/clojure-cheatsheet

;;; Code:

(require 'cider-doc)
(require 'seq)

(defconst cider-cheatsheet-hierarchy
  '(("Primitives"
     ("Numbers"
      ("Arithmetic"
       (clojure.core + - * / quot rem mod dec inc max min))
      ("Compare"
       (clojure.core = == not= < > <= >= compare))
      ("Bitwise"
       (clojure.core bit-and bit-and-not bit-clear bit-flip bit-not bit-or bit-set bit-shift-left bit-shift-right bit-test bit-xor unsigned-bit-shift-right))
      ("Cast"
       (clojure.core byte short long int float double bigdec bigint biginteger num rationalize))
      ("Test"
       (clojure.core nil? some? identical? zero? pos? neg? even? odd?))
      ("Random"
       (clojure.core rand rand-int))
      ("BigDecimal"
       (clojure.core with-precision))
      ("Ratios"
       (clojure.core numerator denominator ratio?))
      ("Arbitrary Precision Arithmetic"
       (clojure.core +\' -\' *\' inc\' dec\'))
      ("Unchecked"
       (clojure.core *unchecked-math*
                     unchecked-add
                     unchecked-add-int
                     unchecked-byte
                     unchecked-char
                     unchecked-dec
                     unchecked-dec-int
                     unchecked-divide-int
                     unchecked-double
                     unchecked-float
                     unchecked-inc
                     unchecked-inc-int
                     unchecked-int
                     unchecked-long
                     unchecked-multiply
                     unchecked-multiply-int
                     unchecked-negate
                     unchecked-negate-int
                     unchecked-remainder-int
                     unchecked-short
                     unchecked-subtract
                     unchecked-subtract-int)))

     ("Strings"
      ("Create"
       (clojure.core str format))
      ("Use"
       (clojure.core count get subs compare)
       (clojure.string join escape split split-lines replace replace-first reverse re-quote-replacement index-of last-index-of starts-with? ends-with? includes?))
      ("Regex"
       (clojure.core re-find re-seq re-matches re-pattern re-matcher re-groups)
       (clojure.string replace replace-first re-quote-replacement))
      ("Letters"
       (clojure.string capitalize lower-case upper-case))
      ("Trim"
       (clojure.string trim trim-newline triml trimr))
      ("Test"
       (clojure.core char char? string?)
       (clojure.string blank?)))

     ("Other"
      ("Characters"
       (clojure.core char char-name-string char-escape-string))
      ("Keywords"
       (clojure.core keyword keyword? find-keyword))
      ("Symbols"
       (clojure.core symbol symbol? gensym))
      ("Data Readers"
       (clojure.core *data-readers* default-data-readers *default-data-reader-fn*))))

    ("Collections"
     ("Generic Ops"
      (clojure.core count bounded-count empty not-empty into conj))
     ("Tree Walking"
      (clojure.walk walk prewalk prewalk-demo prewalk-replace postwalk postwalk-demo postwalk-replace keywordize-keys stringify-keys))
     ("Content tests"
      (clojure.core distinct? empty? every? not-every? some not-any?))
     ("Capabilities"
      (clojure.core sequential? associative? sorted? counted? reversible?))
     ("Type tests"
      (clojure.core type class coll? list? vector? set? map? seq?
                    number? integer? float? decimal? class? rational? ratio?
                    chunked-seq? reduced? special-symbol? record?))
     ("Lists"
      ("Create"
       (clojure.core list list*))
      ("Examine"
       (clojure.core first nth peek))
      ("Change"
       (clojure.core cons conj rest pop)))

     ("Vectors"
      ("Create"
       (clojure.core vec vector vector-of))
      ("Examine"
       (clojure.core get peek))

      ("Change"
       (clojure.core assoc pop subvec replace conj rseq))
      ("Ops"
       (clojure.core mapv filterv reduce-kv)))

     ("Sets"
      ("Create"
       (clojure.core set hash-set sorted-set sorted-set-by))
      ("Examine"
       (clojure.core get contains?))
      ("Change"
       (clojure.core conj disj))
      ("Relational Algebra"
       (clojure.set join select project union difference intersection))
      ("Get map"
       (clojure.set index rename-keys rename map-invert))
      ("Test"
       (clojure.set subset? superset?))
      ("Sorted Sets"
       (clojure.core rseq subseq rsubseq)))

     ("Maps"
      ("Create"
       (clojure.core hash-map array-map zipmap sorted-map sorted-map-by bean frequencies group-by))
      ("Examine"
       (clojure.core get get-in contains? find keys vals map-entry?))
      ("Change"
       (clojure.core assoc assoc-in dissoc merge merge-with select-keys update update-in))
      ("Entry"
       (clojure.core key val))
      ("Sorted Maps"
       (clojure.core rseq subseq rsubseq)))

     ("Hashes"
      (clojure.core hash hash-ordered-coll hash-unordered-coll mix-collection-hash))

     ("Volatiles"
      (clojure.core volatile! volatile? vreset! vswap!)))

    ("Functions"
     ("Create"
      (clojure.core fn defn defn- definline identity constantly comp complement partial juxt memfn memoize fnil every-pred some-fn trampoline))
     ("Call"
      (clojure.core -> ->> some-> some->> as-> cond-> cond->>))
     ("Test"
      (clojure.core fn? ifn?)))

    ("Transducers"
     ("Create"
      (clojure.core cat dedupe distinct drop drop-while filter halt-when interpose keep keep-indexed map map-indexed mapcat partition-all partition-by random-sample remove replace take take-nth take-while))
     ("Call"
      (clojure.core ->Eduction eduction into sequence transduce completing run!))
     ("Early Termination"
      (clojure.core deref reduced reduced? ensure-reduced unreduced)))

    ("Spec"
     ("Operations"
      (clojure.spec.alpha valid? conform unform explain explain-data explain-str explain-out form describe assert check-asserts check-asserts?))
     ("Generator Ops"
      (clojure.spec.alpha gen exercise exercise-fn))
     ("Defn & Registry"
      (clojure.spec.alpha def fdef registry get-spec spec? spec with-gen))
     ("Logical"
      (clojure.spec.alpha and or))
     ("Collection"
      (clojure.spec.alpha coll-of map-of every every-kv keys merge))
     ("Regex "
      (clojure.spec.alpha cat alt * + \? & keys*))
     ("Range"
      (clojure.spec.alpha int-in inst-in double-in int-in-range? inst-in-range?))
     ("Custom Explain"
      (clojure.spec.alpha explain-printer *explain-out*))
     ("Other"
      (clojure.spec.alpha nilable multi-spec fspec conformer))

     ("Predicates with test.check generators"
      ("Numbers"
       (clojure.core number? rational? integer? ratio? decimal? float? zero? double? int? nat-int? neg-int? pos-int?))
      ("Symbols & Keywords"
       (clojure.core keyword? symbol? ident? qualified-ident? qualified-keyword? qualified-symbol? simple-ident? simple-keyword? simple-symbol?))
      ("Scalars"
       (clojure.core string? true? false? nil? some? boolean? bytes? inst? uri? uuid?))
      ("Collections"
       (clojure.core list? map? set? vector? associative? coll? sequential? seq? empty? indexed? seqable?))
      ("Other"
       (clojure.core any?))))

    ("Other"
     ("XML"
      (clojure.core xml-seq)
      (clojure.xml parse))
     ("REPL"
      (clojure.core *1 *2 *3 *e *print-dup* *print-length* *print-level* *print-meta* *print-readably*))
     ("EDN"
      (clojure.edn read read-string))
     ("Compiling Code & Class Generation"
      (clojure.core *compile-files* *compile-path* *file* *warn-on-reflection* compile gen-class gen-interface loaded-libs test))
     ("Misc"
      (clojure.core eval force name *clojure-version* clojure-version *command-line-args*))
     ("Pretty Printing"
      (clojure.pprint pprint print-table pp *print-right-margin*))
     ("Browser / Shell"
      (clojure.java.browse browse-url)
      (clojure.java.shell sh with-sh-dir with-sh-env)))

    ("Vars & Global Environment"
     ("Def Variants"
      (:special def)
      (clojure.core defn defn- definline defmacro defmethod defmulti defonce defrecord))
     ("Interned Vars"
      (:special var)
      (clojure.core declare intern binding find-var))
     ("Var Objects"
      (clojure.core with-local-vars var-get var-set alter-var-root var?))
     ("Var Validators"
      (clojure.core set-validator! get-validator)))

    ("Reader Conditionals"
     (clojure.core reader-conditional reader-conditional? tagged-literal tagged-literal?))

    ("Abstractions"
     ("Protocols"
      (clojure.core defprotocol extend extend-type extend-protocol reify extends? satisfies? extenders))
     ("Records & Types"
      (clojure.core defrecord deftype))
     ("Multimethods"
      ("Define"
       (clojure.core defmulti defmethod))
      ("Dispatch"
       (clojure.core get-method methods))
      ("Remove"
       (clojure.core remove-method remove-all-methods))
      ("Prefer"
       (clojure.core prefer-method prefers))
      ("Relation"
       (clojure.core derive isa? parents ancestors descendants make-hierarchy))))

    ("Macros"
     ("Create"
      (clojure.core defmacro definline))
     ("Debug"
      (clojure.core macroexpand-1 macroexpand)
      (clojure.walk macroexpand-all))
     ("Branch"
      (clojure.core and or when when-not when-let when-first if-not if-let cond condp case))
     ("Loop"
      (clojure.core for doseq dotimes while))
     ("Arrange"
      (clojure.core .. doto ->))
     ("Scope"
      (clojure.core binding locking time)
      (clojure.core with-in-str with-local-vars with-open with-out-str with-precision with-redefs with-redefs-fn))
     ("Lazy"
      (clojure.core lazy-cat lazy-seq delay delay?))
     ("Doc"
      (clojure.core assert comment)
      (clojure.repl doc dir dir-fn source-fn)))

    ("Java Interop"
     ("General"
      (:special new set!)
      (clojure.core .. doto bean comparator enumeration-seq import iterator-seq memfn definterface supers bases))
     ("Cast"
      (clojure.core boolean byte short char int long float double bigdec bigint num cast biginteger))
     ("Exceptions"
      (:special throw try catch finally)
      (clojure.core ex-info ex-data Throwable->map StackTraceElement->vec)
      (clojure.repl pst))
     ("Arrays"
      ("Create"
       (clojure.core boolean-array byte-array double-array char-array float-array int-array long-array make-array object-array short-array to-array))
      ("Manipulate"
       (clojure.core aclone aget aset alength amap areduce aset-int aset-long aset-short aset-boolean aset-byte aset-char aset-double aset-float))
      ("Cast"
       (clojure.core booleans bytes chars doubles floats ints longs shorts)))
     ("Proxy"
      ("Create"
       (clojure.core proxy get-proxy-class construct-proxy init-proxy))
      ("Misc"
       (clojure.core proxy-mappings proxy-super update-proxy))))

    ("Namespaces"
     ("Current"
      (clojure.core *ns*))
     ("Create Switch"
      (clojure.core ns in-ns create-ns))
     ("Add"
      (clojure.core alias import intern refer refer-clojure))
     ("Find"
      (clojure.core all-ns find-ns))
     ("Examine"
      (clojure.core ns-aliases ns-imports ns-interns ns-map ns-name ns-publics ns-refers))
     ("From symbol"
      (clojure.core resolve namespace ns-resolve the-ns))
     ("Remove"
      (clojure.core ns-unalias ns-unmap remove-ns)))
    ("Loading"
     ("Load libs"
      (clojure.core require use import refer))
     ("List Loaded"
      (clojure.core loaded-libs))
     ("Load Misc"
      (clojure.core load load-file load-reader load-string)))

    ("Concurrency"
     ("Atoms"
      (clojure.core atom swap! swap-vals! reset! reset-vals! compare-and-set!))
     ("Futures"
      (clojure.core future future-call future-cancel future-cancelled? future-done? future?))
     ("Threads"
      (clojure.core bound-fn bound-fn* get-thread-bindings pop-thread-bindings push-thread-bindings))

     ("Misc"
      (clojure.core locking pcalls pvalues pmap seque promise deliver))

     ("Refs & Transactions"
      ("Create"
       (clojure.core ref))
      ("Examine"
       (clojure.core deref))
      ("Transaction"
       (clojure.core sync dosync io!))
      ("In Transaction"
       (clojure.core ensure ref-set alter commute))
      ("Validators"
       (clojure.core get-validator set-validator!))
      ("History"
       (clojure.core ref-history-count ref-max-history ref-min-history)))

     ("Agents & Asynchronous Actions"
      ("Create"
       (clojure.core agent))
      ("Examine"
       (clojure.core agent-error))
      ("Change State"
       (clojure.core send send-off restart-agent send-via set-agent-send-executor! set-agent-send-off-executor!))
      ("Block Waiting"
       (clojure.core await await-for))
      ("Ref Validators"
       (clojure.core get-validator set-validator!))
      ("Watchers"
       (clojure.core add-watch remove-watch))
      ("Thread Handling"
       (clojure.core shutdown-agents))
      ("Error"
       (clojure.core error-handler set-error-handler! error-mode set-error-mode!))
      ("Misc"
       (clojure.core *agent* release-pending-sends))))

    ("Sequences"
     ("Creating a Lazy Seq"
      ("From Collection"
       (clojure.core seq sequence keys vals rseq subseq rsubseq))
      ("From Producer Fn"
       (clojure.core lazy-seq repeatedly iterate))
      ("From Constant"
       (clojure.core repeat range))
      ("From Other"
       (clojure.core file-seq line-seq resultset-seq re-seq tree-seq xml-seq iterator-seq enumeration-seq))
      ("From Seq"
       (clojure.core keep keep-indexed)))

     ("Seq in, Seq out"
      ("Get shorter"
       (clojure.core distinct dedupe filter remove for))
      ("Get longer"
       (clojure.core cons conj concat lazy-cat mapcat cycle interleave interpose)))
     ("Tail-items"
      (clojure.core rest nthrest fnext nnext drop drop-while take-last for))
     ("Head-items"
      (clojure.core take take-nth take-while butlast drop-last for))
     ("Change"
      (clojure.core conj concat distinct flatten group-by partition partition-all partition-by split-at split-with filter remove replace shuffle random-sample))
     ("Rearrange"
      (clojure.core reverse sort sort-by compare))
     ("Process items"
      (clojure.core map pmap map-indexed mapcat for replace seque))

     ("Using a Seq"
      ("Extract item"
       (clojure.core first second last rest next ffirst nfirst fnext nnext nth nthnext rand-nth when-first max-key min-key))
      ("Construct coll"
       (clojure.core zipmap into reduce reductions set vec into-array to-array-2d))
      ("Pass to fn"
       (clojure.core apply))
      ("Search"
       (clojure.core some filter))
      ("Force evaluation"
       (clojure.core doseq dorun doall))
      ("Check for forced"
       (clojure.core realized?))))

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
     ("XML"
      (clojure.data.zip.xml attr attr= seq-test tag= text text= xml-> xml1->))
     ("Misc"
      (clojure.zip root node branch? end?)))

    ("Documentation"
     ("REPL"
      (clojure.repl doc find-doc apropos source pst)
      (clojure.java.javadoc javadoc)))

    ("Transients"
     ("Create"
      (clojure.core transient persistent!))
     ("Change"
      (clojure.core conj! pop! assoc! dissoc! disj!)))
    ("Misc"
     ("Compare"
      (clojure.core = == identical? not= not compare)
      (clojure.data diff))
     ("Test"
      (clojure.core true? false? nil? instance?)))

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
      (clojure.core read-line read))
     ("From reader"
      (clojure.core line-seq read))
     ("From string"
      (clojure.core read-string with-in-str))
     ("Open"
      (clojure.core with-open)
      (clojure.java.io reader writer input-stream output-stream))
     ("Interop"
      (clojure.java.io make-writer make-reader make-output-stream make-input-stream))
     ("Misc"
      (clojure.core flush file-seq *in* *out* *err*)
      (clojure.java.io file copy delete-file resource as-file as-url as-relative-path make-parents)))

    ("Metadata"
     (clojure.core meta with-meta alter-meta! reset-meta! vary-meta))

    ("Special Forms"
     (:special def if do quote var recur throw try monitor-enter monitor-exit)
     (clojure.core fn loop)
     ("Binding / Destructuring"
      (clojure.core let fn letfn defn defmacro loop for doseq if-let if-some when-let when-some)))

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
      (clojure.core.async into onto-chan to-chan)))
    ("Unit Tests"
     ("Defining"
      (clojure.test deftest deftest- testing is are))
     ("Running"
      (clojure.test run-tests run-all-tests test-vars))
     ("Fixtures"
      (clojure.test use-fixtures join-fixtures compose-fixtures))))
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

(defun cider-cheatsheet--select-var (var-list)
  "Expand the symbols in VAR-LIST to fully-qualified var names.

The list can hold one or more lists inside - one per each namespace."
  (let ((namespaced-vars (seq-mapcat #'cider-cheatsheet--expand-vars
                                     (seq-remove (lambda (list)
                                                   (eq (car list) :url))
                                                 var-list))))
    (cider-doc-lookup (completing-read "Select var: " namespaced-vars))))

;;;###autoload
(defun cider-cheatsheet ()
  "Navigate `cider-cheatsheet-hierarchy' with `completing-read'.

When you make it to a Clojure var its doc buffer gets displayed."
  (interactive)
  (let ((cheatsheet-data cider-cheatsheet-hierarchy))
    (while (stringp (caar cheatsheet-data))
      (let* ((sections (mapcar #'car cheatsheet-data))
             (sel-section (completing-read "Select cheatsheet section: " sections))
             (section-data (seq-find (lambda (elem) (equal (car elem) sel-section)) cheatsheet-data)))
        (setq cheatsheet-data (cdr section-data))))
    (cider-cheatsheet--select-var cheatsheet-data)))

(provide 'cider-cheatsheet)

;;; cider-cheatsheet.el ends here
