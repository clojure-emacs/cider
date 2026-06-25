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

;; Please, for each `describe', ensure there's an `it' block, so that its execution is visible in CI.

(describe "cider-log"
  (let ((framework (nrepl-dict "id" "jul" "name" "Java Util Logging"))
        (appender (nrepl-dict "id" "cider-log")))

    (it "raises user-error when cider is not connected."
      (spy-on 'cider-connected-p :and-return-value nil)
      (expect (cider-log framework appender) :to-throw 'user-error))

    (it "doesn't add an appender when initialized."
      (let ((cider-log--initialized-once-p t))
        (spy-on 'cider-sync-request:log-frameworks :and-return-value (list framework))
        (spy-on 'transient-setup)
        (cider-log framework appender)
        (expect 'transient-setup :to-have-been-called-with 'cider-log)))

    (it "does add an appender when not initialized."
      (let ((cider-log--initialized-once-p nil))
        (spy-on 'cider-sync-request:log-frameworks :and-return-value (list framework))
        (spy-on 'cider-sync-request:log-add-appender :and-return-value appender)
        (spy-on 'transient-setup)
        (cider-log framework appender)
        (expect 'transient-setup :to-have-been-called-with 'cider-log)))

    (it "applies the consumer's own filters when initializing, not the appender's."
      ;; Regression: the consumer branch of `cider-log--ensure-initialized'
      ;; used to read the appender's filters instead of the consumer's.
      (let* ((appender-filters (nrepl-dict "pattern" "appender-pattern"))
             (consumer-filters (nrepl-dict "pattern" "consumer-pattern"))
             (appender (nrepl-dict "id" "cider-log" "filters" appender-filters))
             (consumer (nrepl-dict "id" "consumer" "filters" consumer-filters))
             (cider-log--initialized-once-p t)) ; skip the add-appender network path
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
