;;; cider-log-tests.el  -*- lexical-binding: t; -*-

;; Copyright © 2023-2026 Bozhidar Batsov and CIDER contributors

;; Author: r0man <roman@burningswell.com>

;; This file is NOT part of GNU Emacs.

;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see `http://www.gnu.org/licenses/'.

;;; Commentary:

;; This file is part of CIDER

;;; Code:

(require 'buttercup)
(require 'cider-log)
(require 'cider-test-utils)

;; Please, for each `describe', ensure there's an `it' block, so that its execution is visible in CI.

(describe "cider-log"
  (let ((framework (nrepl-dict "id" "jul" "name" "Java Util Logging"))
        (appender (nrepl-dict "id" "cider-log")))

    (it "raises user-error when cider is not connected."
      (spy-on 'cider-connected-p :and-return-value nil)
      (expect (cider-log framework appender) :to-throw 'user-error))

    (it "doesn't add an appender when it already exists server-side."
      (spy-on 'cider-sync-request:log-frameworks :and-return-value (list framework))
      (spy-on 'cider-log-appender-reload :and-return-value appender) ; exists
      (spy-on 'cider-sync-request:log-add-appender)
      (spy-on 'transient-setup)
      (cider-log framework appender)
      (expect 'cider-sync-request:log-add-appender :not :to-have-been-called)
      (expect 'transient-setup :to-have-been-called-with 'cider-log))

    (it "adds an appender when it is missing server-side."
      ;; Regression: this must keep working across an appender kill or a REPL
      ;; restart, which a stuck once-per-session flag used to prevent.
      (spy-on 'cider-sync-request:log-frameworks :and-return-value (list framework))
      (spy-on 'cider-log-appender-reload :and-return-value nil) ; missing
      (spy-on 'cider-sync-request:log-add-appender :and-return-value appender)
      (spy-on 'transient-setup)
      (cider-log framework appender)
      (expect 'cider-sync-request:log-add-appender :to-have-been-called)
      (expect 'transient-setup :to-have-been-called-with 'cider-log))

    (it "applies the consumer's own filters when initializing, not the appender's."
      ;; Regression: the consumer branch of `cider-log--ensure-initialized'
      ;; used to read the appender's filters instead of the consumer's.
      (let* ((appender-filters (nrepl-dict "pattern" "appender-pattern"))
             (consumer-filters (nrepl-dict "pattern" "consumer-pattern"))
             (appender (nrepl-dict "id" "cider-log" "filters" appender-filters))
             (consumer (nrepl-dict "id" "consumer" "filters" consumer-filters)))
        (spy-on 'cider-log-appender-reload :and-return-value appender) ; skip add path
        (spy-on 'cider-log--set-filters)
        (with-temp-buffer
          (cider-log--ensure-initialized framework appender consumer))
        (expect 'cider-log--set-filters :to-have-been-called-with consumer-filters)))))

(describe "cider-log--format-value"
  (it "renders nil as an empty string"
    (expect (cider-log--format-value nil) :to-equal ""))
  (it "renders a scalar as its printed form"
    (expect (substring-no-properties (cider-log--format-value "x")) :to-equal "x"))
  (it "joins a list or vector with commas"
    (expect (substring-no-properties (cider-log--format-value '("a" "b"))) :to-equal "a, b")
    (expect (substring-no-properties (cider-log--format-value ["a" "b"])) :to-equal "a, b")))

(describe "cider-log--strip-whitespace"
  (it "collapses runs of spaces and newlines into a single space"
    (expect (cider-log--strip-whitespace "a  b\nc") :to-equal "a b c")))

(describe "cider-log--format-time"
  (it "formats a time in ISO8601"
    (expect (cider-log--format-time (encode-time 0 0 12 1 1 2021))
            :to-match "\\`[0-9]\\{4\\}-[0-9][0-9]-[0-9][0-9]T[0-9][0-9]:[0-9][0-9]:[0-9][0-9][-+][0-9]\\{4\\}\\'")))

(describe "cider-log-event--format-logback"
  (it "renders the thread, level, logger and message (timezone-independent tail)"
    (expect (substring-no-properties
             (cider-log-event--format-logback
              (nrepl-dict "level" "info" "logger" "my.log" "message" "hello"
                          "thread" "main" "timestamp" 1609459200000)))
            :to-match " \\[main\\] INFO my\\.log - hello\n\\'"))
  (it "truncates the message to cider-log-max-message-length"
    (let ((cider-log-max-message-length 3))
      (expect (substring-no-properties
               (cider-log-event--format-logback
                (nrepl-dict "level" "warn" "logger" "l" "message" "hello"
                            "thread" "t" "timestamp" "ts")))
              :to-equal "ts [t] WARN l - hel\n"))))

(describe "cider-log framework/appender accessors and finders"
  (it "reads framework id, name and level names"
    (let ((fw (nrepl-dict "id" "logback" "name" "Logback"
                          "levels" (list (nrepl-dict "name" "DEBUG")
                                         (nrepl-dict "name" "INFO")))))
      (expect (cider-log-framework-id fw) :to-equal "logback")
      (expect (cider-log-framework-name fw) :to-equal "Logback")
      (expect (cider-log-framework-level-names fw) :to-equal '("DEBUG" "INFO"))))

  (it "finds a framework by id, or nil when absent"
    (let ((fws (list (nrepl-dict "id" "jul") (nrepl-dict "id" "logback"))))
      (expect (cider-log-framework-id (cider-log-framework-by-id fws "logback"))
              :to-equal "logback")
      (expect (cider-log-framework-by-id fws "nope") :to-be nil)))

  (it "finds an appender on a framework by id"
    (let ((fw (nrepl-dict "appenders" (list (nrepl-dict "id" "cider-log")))))
      (expect (cider-log-appender-id (cider-log-framework-appender fw "cider-log"))
              :to-equal "cider-log")
      (expect (cider-log-framework-appender fw "nope") :to-be nil))))

(describe "cider-log event accessors"
  (it "reads the event id and exception"
    (let ((ev (nrepl-dict "id" "ev-42" "exception" "NullPointerException")))
      (expect (cider-log-event-id ev) :to-equal "ev-42")
      (expect (cider-log-event-exception ev) :to-equal "NullPointerException"))))

(describe "cider-log--filters"
  (it "collects the filter variables into a dict"
    (let ((cider-log--level-filter "INFO")
          (cider-log--pattern-filter "foo")
          (cider-log--end-time-filter nil)
          (cider-log--exceptions-filter nil)
          (cider-log--loggers-filter nil)
          (cider-log--start-time-filter nil)
          (cider-log--threads-filter nil))
      (let ((filters (cider-log--filters)))
        (expect (nrepl-dict-get filters "level") :to-equal "INFO")
        (expect (nrepl-dict-get filters "pattern") :to-equal "foo")
        (expect (nrepl-dict-get filters "threads") :to-be nil)))))

(describe "cider-log--set-filters"
  (it "sets the filter variables from the filters dict"
    (let (cider-log--end-time-filter
          cider-log--exceptions-filter
          cider-log--level-filter
          cider-log--loggers-filter
          cider-log--pattern-filter
          cider-log--start-time-filter
          cider-log--threads-filter)
      (cider-log--set-filters (nrepl-dict "level" "INFO"
                                          "pattern" "foo"
                                          "threads" '("main")))
      (expect cider-log--level-filter :to-equal "INFO")
      (expect cider-log--pattern-filter :to-equal "foo")
      (expect cider-log--threads-filter :to-equal '("main"))
      (expect cider-log--loggers-filter :to-be nil)))

  (it "leaves the filter variables alone when the filters are nil"
    (let ((cider-log--level-filter "WARN"))
      (cider-log--set-filters nil)
      (expect cider-log--level-filter :to-equal "WARN"))))

(describe "cider-log--bold"
  (it "returns nil for nil"
    (expect (cider-log--bold nil) :to-be nil))
  (it "renders any value with a bold face"
    (let ((s (cider-log--bold 42)))
      (expect (substring-no-properties s) :to-equal "42")
      (expect (get-text-property 0 'face s) :to-equal 'bold))))

(describe "cider-log-buffer-has-content-p"
  (it "returns nil when the buffer doesn't exist"
    (expect (cider-log-buffer-has-content-p "*cider-log-tests-no-such-buffer*")
            :to-be nil))
  (it "returns nil for an empty buffer and non-nil once it has content"
    (with-temp-buffer
      (expect (cider-log-buffer-has-content-p (current-buffer)) :to-be nil)
      (insert "an event")
      (expect (cider-log-buffer-has-content-p (current-buffer)) :to-be-truthy))))

(describe "cider-log transient descriptions"
  (it "shows the current framework name, or n/a"
    (let ((cider-log-framework-name "Logback"))
      (expect (substring-no-properties (cider-log--description-set-framework))
              :to-equal "Select framework Logback"))
    (let ((cider-log-framework-name nil))
      (expect (substring-no-properties (cider-log--description-set-framework))
              :to-equal "Select framework n/a")))
  (it "shows the current log buffer, or n/a"
    (let ((cider-log-buffer "*cider-log*"))
      (expect (substring-no-properties (cider-log--description-set-buffer))
              :to-equal "Select buffer *cider-log*"))
    (let ((cider-log-buffer nil))
      (expect (substring-no-properties (cider-log--description-set-buffer))
              :to-equal "Select buffer n/a")))
  (it "describes clearing the current log buffer"
    (let ((cider-log-buffer "*cider-log*"))
      (expect (substring-no-properties (cider-log--description-clear-events-buffer))
              :to-equal "Clear *cider-log* buffer"))))

(describe "cider-log nREPL request construction"
  :var (framework appender event)
  (before-each
    (setq framework (nrepl-dict "id" "logback" "name" "Logback")
          appender (nrepl-dict "id" "my-appender"
                               "filters" (nrepl-dict "level" "INFO")
                               "size" 100
                               "threshold" 10)
          event (nrepl-dict "id" "ev-42")))

  (it "cider-sync-request:log-add-appender sends the appender settings"
    (spy-on 'cider-nrepl-sync-request
            :and-return-value (nrepl-dict "cider/log-add-appender" appender))
    (expect (cider-sync-request:log-add-appender framework appender)
            :to-equal appender)
    (expect 'cider-nrepl-sync-request :to-have-sent-op "cider/log-add-appender"
            "framework" "logback" "appender" "my-appender"
            "size" 100 "threshold" 10)
    (expect (cider-request-get (cider-sent-request 'cider-nrepl-sync-request) "filters")
            :to-equal-dict (nrepl-dict "level" "INFO")))

  (it "cider-sync-request:log-clear sends the clear-appender op"
    (spy-on 'cider-nrepl-sync-request :and-return-value (nrepl-dict))
    (cider-sync-request:log-clear framework appender)
    (expect 'cider-nrepl-sync-request :to-have-sent-op "cider/log-clear-appender"
            "framework" "logback" "appender" "my-appender"))

  (it "cider-sync-request:log-search sends the filters and pagination"
    (spy-on 'cider-nrepl-sync-request
            :and-return-value (nrepl-dict "cider/log-search" (list event)))
    (let ((filters (nrepl-dict "pattern" "foo")))
      (expect (cider-sync-request:log-search framework appender
                                             :filters filters :limit 25 :offset 5)
              :to-equal (list event))
      (expect 'cider-nrepl-sync-request :to-have-sent-op "cider/log-search"
              "filters" filters "limit" 25 "offset" 5)))

  (it "cider-sync-request:log-inspect-event asks for the event by id"
    (spy-on 'cider-nrepl-sync-request
            :and-return-value (nrepl-dict "value" "inspected"))
    (expect (cider-sync-request:log-inspect-event framework appender event)
            :to-equal "inspected")
    (expect 'cider-nrepl-sync-request :to-have-sent-op "cider/log-inspect-event"
            "event" "ev-42"))

  (it "cider-sync-request:log-format-event includes the print options"
    (spy-on 'cider-nrepl-sync-request
            :and-return-value (nrepl-dict "cider/log-format-event" "formatted"))
    (expect (cider-sync-request:log-format-event framework appender event)
            :to-equal "formatted")
    (expect 'cider-nrepl-sync-request :to-have-sent-op "cider/log-format-event"
            "event" "ev-42" "nrepl.middleware.print/stream?" "1"))

  (it "cider-request:log-add-consumer sends the consumer's filters and the callback"
    (spy-on 'cider-nrepl-send-request)
    (let ((consumer (nrepl-dict "filters" (nrepl-dict "level" "INFO"))))
      (cider-request:log-add-consumer framework appender consumer #'ignore)
      (expect 'cider-nrepl-send-request :to-have-sent-op "cider/log-add-consumer"
              "framework" "logback" "appender" "my-appender")
      (let ((args (spy-context-args (spy-calls-most-recent 'cider-nrepl-send-request))))
        (expect (nrepl-dict-get (cider-request-get (car args) "filters") "level")
                :to-equal "INFO")
        (expect (cadr args) :to-be #'ignore))))

  (it "cider-sync-request:log-remove-consumer identifies the consumer by id"
    (spy-on 'cider-nrepl-sync-request :and-return-value (nrepl-dict))
    (cider-sync-request:log-remove-consumer
     framework appender (nrepl-dict "id" "my-consumer"))
    (expect 'cider-nrepl-sync-request :to-have-sent-op "cider/log-remove-consumer"
            "consumer" "my-consumer")))

(describe "cider-log appender and consumer accessors"
  (it "reads the appender size, threshold, filters and consumers"
    (let* ((consumers (list (nrepl-dict "id" "c1")))
           (filters (nrepl-dict "level" "INFO"))
           (appender (nrepl-dict "id" "a1" "size" 100 "threshold" 10
                                 "filters" filters "consumers" consumers)))
      (expect (cider-log-appender-size appender) :to-equal 100)
      (expect (cider-log-appender-threshold appender) :to-equal 10)
      (expect (cider-log-appender-filters appender) :to-equal filters)
      (expect (cider-log-appender-consumers appender) :to-equal consumers)))

  (it "finds a consumer of an appender by its id"
    (let ((appender (nrepl-dict "consumers" (list (nrepl-dict "id" "c1")
                                                  (nrepl-dict "id" "c2")))))
      (expect (cider-log-consumer-id
               (cider-log-appender-consumer appender (nrepl-dict "id" "c2")))
              :to-equal "c2")
      (expect (cider-log-appender-consumer appender (nrepl-dict "id" "nope"))
              :to-be nil)))

  (it "reads the consumer id and filters"
    (let* ((filters (nrepl-dict "pattern" "foo"))
           (consumer (nrepl-dict "id" "c1" "filters" filters)))
      (expect (cider-log-consumer-id consumer) :to-equal "c1")
      (expect (cider-log-consumer-filters consumer) :to-equal filters)))

  (it "finds the buffers whose local consumer matches"
    (let ((consumer (nrepl-dict "id" "c1")))
      (with-temp-buffer
        (setq-local cider-log-consumer (nrepl-dict "id" "c1"))
        (let ((match (current-buffer)))
          (with-temp-buffer
            (setq-local cider-log-consumer (nrepl-dict "id" "other"))
            (expect (cider-log-consumer-buffers consumer)
                    :to-equal (list match))))))))

(describe "cider-log--appender"
  (it "builds the appender dict from the current settings and filters"
    (let ((cider-log-appender-id "my-appender")
          (cider-log-appender-size 50)
          (cider-log-appender-threshold 20)
          (cider-log--level-filter "INFO")
          (cider-log--end-time-filter nil)
          (cider-log--exceptions-filter nil)
          (cider-log--loggers-filter nil)
          (cider-log--pattern-filter nil)
          (cider-log--start-time-filter nil)
          (cider-log--threads-filter nil))
      (let ((appender (cider-log--appender)))
        (expect (nrepl-dict-get appender "id") :to-equal "my-appender")
        (expect (nrepl-dict-get appender "size") :to-equal 50)
        (expect (nrepl-dict-get appender "threshold") :to-equal 20)
        (expect (nrepl-dict-get (nrepl-dict-get appender "filters") "level")
                :to-equal "INFO"))))
  (it "returns nil without an appender id"
    (let ((cider-log-appender-id nil))
      (expect (cider-log--appender) :to-be nil))))

(describe "cider-log--consumer"
  (it "includes the current consumer's id when there is one"
    (with-temp-buffer
      (setq-local cider-log-consumer (nrepl-dict "id" "c1"))
      (expect (nrepl-dict-get (cider-log--consumer) "id") :to-equal "c1")))
  (it "builds a consumer with only filters otherwise"
    (with-temp-buffer
      (setq-local cider-log-consumer nil)
      (let ((consumer (cider-log--consumer)))
        (expect (nrepl-dict-get consumer "id") :to-be nil)
        (expect (nrepl-dict-p (nrepl-dict-get consumer "filters")) :to-be-truthy)))))

(describe "cider-log--event-options"
  (it "includes the filters and the pagination settings"
    (let ((cider-log-pagination-limit 100)
          (cider-log-pagination-offset 5)
          (cider-log--level-filter "INFO")
          (cider-log--end-time-filter nil)
          (cider-log--exceptions-filter nil)
          (cider-log--loggers-filter nil)
          (cider-log--pattern-filter nil)
          (cider-log--start-time-filter nil)
          (cider-log--threads-filter nil))
      (let ((options (cider-log--event-options)))
        (expect (nrepl-dict-get options "limit") :to-equal 100)
        (expect (nrepl-dict-get options "offset") :to-equal 5)
        (expect (nrepl-dict-get (nrepl-dict-get options "filters") "level")
                :to-equal "INFO")))))

(describe "cider-log--insert-events"
  (it "appends formatted events carrying the event as a text property"
    (with-temp-buffer
      (let ((event1 (nrepl-dict "id" "e1" "level" "info" "logger" "l"
                                "message" "one" "thread" "t" "timestamp" "ts1"))
            (event2 (nrepl-dict "id" "e2" "level" "warn" "logger" "l"
                                "message" "two" "thread" "t" "timestamp" "ts2")))
        (cider-log--insert-events (current-buffer) (list event1 event2))
        (expect (buffer-substring-no-properties (point-min) (point-max))
                :to-equal "ts1 [t] INFO l - one\nts2 [t] WARN l - two\n")
        (goto-char (point-min))
        (expect (cider-log-event-at-point) :to-equal event1)
        (forward-line 1)
        (expect (cider-log-event-at-point) :to-equal event2)))))

(describe "cider-log-event--pretty-print"
  :var (framework appender event)
  (before-each
    (setq framework (nrepl-dict "id" "logback")
          appender (nrepl-dict "id" "my-appender")
          event (nrepl-dict "id" "ev-42")))

  (it "shows the formatted event in the log event popup buffer"
    (let ((cider-log-event-buffer "*cider-log-event-test*"))
      (spy-on 'cider-sync-request:log-format-event
              :and-return-value "{:formatted :event}")
      (spy-on 'cider-popup-buffer :and-call-fake
              (lambda (name &rest _) (get-buffer-create name)))
      (unwind-protect
          (progn
            (cider-log-event--pretty-print framework appender event)
            (with-current-buffer cider-log-event-buffer
              (expect (buffer-string) :to-equal "{:formatted :event}")
              (expect (point) :to-equal (point-min))))
        (kill-buffer "*cider-log-event-test*"))))

  (it "doesn't pop up a buffer when the event can't be formatted"
    (spy-on 'cider-sync-request:log-format-event :and-return-value nil)
    (spy-on 'cider-popup-buffer)
    (cider-log-event--pretty-print framework appender event)
    (expect 'cider-popup-buffer :not :to-have-been-called)))

(describe "cider-log-event--inspect"
  (it "renders the inspected event value in the inspector"
    (spy-on 'cider-sync-request:log-inspect-event :and-return-value "inspected")
    (spy-on 'cider-inspector--render-value)
    (cider-log-event--inspect (nrepl-dict "id" "logback")
                              (nrepl-dict "id" "my-appender")
                              (nrepl-dict "id" "ev-42"))
    (expect 'cider-inspector--render-value :to-have-been-called-with "inspected")))

(describe "cider-log--completion-extra-properties"
  (it "annotates completion candidates with the given keys, whitespace collapsed"
    (let* ((props (cider-log--completion-extra-properties '("name")))
           (annotate (plist-get props :annotation-function))
           (minibuffer-completion-table
            (list (list "logback" (nrepl-dict "name" "The Logback\n  framework")))))
      (expect (substring-no-properties (funcall annotate "logback"))
              :to-equal " - The Logback framework")
      (expect (funcall annotate "unknown") :to-be nil))))

(describe "cider-log-info"
  (it "summarizes the buffer, framework, appender and consumer"
    (spy-on 'message)
    (with-temp-buffer
      (setq-local cider-log-consumer (nrepl-dict "id" "c1"))
      (let ((cider-log-buffer "*cider-log*")
            (cider-log-framework-name "Logback")
            (cider-log-appender-id "my-appender"))
        (cider-log-info)))
    (expect (substring-no-properties
             (apply #'format (spy-context-args (spy-calls-most-recent 'message))))
            :to-equal
            "Buffer: *cider-log* Framework: Logback Appender: my-appender Consumer: c1")))
