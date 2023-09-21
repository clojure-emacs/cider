;;; cider-log.el --- Log inspection functionality for Clojure -*- lexical-binding: t -*-

;; Copyright © 2023 Bozhidar Batsov and CIDER contributors

;; Author: r0man <roman@burningswell.com>

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

;; Log inspection functionality for Clojure.
;;
;; Please, refer to the online documentation for more details
;; https://docs.cider.mx/cider/debugging/logging.html.

;;; Code:

(require 'cider-inspector)
(require 'cider-stacktrace)
(require 'cl-lib)
(require 'logview nil t)
(require 'org)
(require 'seq)
(require 'transient)

(defcustom cider-log-framework-name nil
  "The name of the current log framework."
  :group 'cider
  :package-version '(cider . "1.8.0")
  :safe #'stringp
  :type 'string)

(defcustom cider-log-appender-id "cider-log"
  "The name of the default log appender."
  :group 'cider
  :package-version '(cider . "1.8.0")
  :safe #'stringp
  :type 'string)

(defcustom cider-log-buffer "*cider-log*"
  "The name of the log buffer."
  :group 'cider
  :package-version '(cider . "1.8.0")
  :safe #'stringp
  :type 'string)

(defcustom cider-log-event-buffer "*cider-log-event*"
  "The name of the log event buffer."
  :group 'cider
  :package-version '(cider . "1.8.0")
  :safe #'stringp
  :type 'string)

(defcustom cider-log-max-message-length 500
  "The maximum length of the log message to display."
  :group 'cider
  :package-version '(cider . "1.8.0")
  :safe #'integerp
  :type 'integer)

(defcustom cider-log-pagination-limit 250
  "The maximum number of log events to return when searching events."
  :group 'cider
  :package-version '(cider . "1.8.0")
  :safe #'integerp
  :type 'integer)

(defcustom cider-log-pagination-offset 0
  "The offset from which to return results when searching events."
  :group 'cider
  :package-version '(cider . "1.8.0")
  :safe #'integerp
  :type 'integer)

(defcustom cider-log-use-logview (fboundp 'logview-mode)
  "Whether to use `logview-mode' or not."
  :group 'cider
  :package-version '(cider . "1.8.0")
  :safe #'booleanp
  :type 'boolean)

(defvar logview-mode-map)
(declare-function logview--guess-submode "logview" () t)
(declare-function logview-initialized-p "logview" () t)
(declare-function logview-mode "logview" () t)

(defvar cider-log--initialized-once-p nil
  "Set to t if log framework and appender have been initialized once.")

(defvar cider-log-framework nil
  "The current log framework to use.")

(defvar cider-log-appender nil
  "The current log appender.")

(defvar cider-log-appender-size 100000
  "The size of the log appender.")

(defvar cider-log-appender-threshold 10
  "The threshold in percent of the log appender.")

(defvar-local cider-log-consumer nil
  "The current log consumer.")

;; Filters

(defvar cider-log--end-time-filter nil)
(defvar cider-log--exceptions-filter nil)
(defvar cider-log--level-filter nil)
(defvar cider-log--loggers-filter nil)
(defvar cider-log--pattern-filter nil)
(defvar cider-log--start-time-filter nil)
(defvar cider-log--threads-filter nil)

(defun cider-log--bold (s)
  "Return S with a bold face."
  (when s (propertize (format "%s" s) 'face 'bold)))

(defun cider-log-buffer-clear-p (&optional buffer)
  "Return non-nil if BUFFER is not empty, otherwise nil."
  (when-let (buffer (get-buffer (or buffer cider-log-buffer)))
    (> (buffer-size buffer) 0)))

(defun cider-log--description-clear-events-buffer ()
  "Return the description for the set framework action."
  (format "Clear %s buffer"
          (if cider-log-buffer
              (cider-log--format-value cider-log-buffer)
            (propertize "n/a" 'face 'font-lock-comment-face))))

(defun cider-log--description-set-framework ()
  "Return the description for the set framework action."
  (format "Select framework %s"
          (if cider-log-framework-name
              (cider-log--format-value cider-log-framework-name)
            (propertize "n/a" 'face 'font-lock-comment-face))))

(defun cider-log--description-set-buffer ()
  "Return the description for the set buffer action."
  (format "Select buffer %s"
          (if cider-log-buffer
              (cider-log--format-value cider-log-buffer)
            (propertize "n/a" 'face 'font-lock-comment-face))))

(defun cider-log--buffers-in-major-mode (expected)
  "Return all buffers which are in the EXPECTED major mode."
  (seq-filter (lambda (buffer)
                (with-current-buffer buffer
                  (equal expected major-mode)))
              (buffer-list)))

(defun cider-log--format-time (time)
  "Format TIME in ISO8601 format."
  (format-time-string "%FT%T%z" time))

(defun cider-log--format-value (value)
  "Format the VALUE for display in a transient menu."
  (cond ((null value) "")
        ((or (listp value) (vectorp value))
         (string-join (seq-map #'cider-log--format-value value)
                      (propertize ", " 'face 'font-lock-comment-face)))
        (t (propertize (format "%s" value) 'face 'transient-value))))

(defun cider-log--strip-whitespace (s)
  "Replace multiple white space characters in S with a single one."
  (replace-regexp-in-string "[\n ]+" " " s))

;; NREPL

(defun cider-request:log-add-consumer (framework appender consumer &optional callback)
  "Add CONSUMER to the APPENDER of FRAMEWORK and call CALLBACK on log events."
  (cider-ensure-op-supported "cider/log-add-consumer")
  (thread-first `("op" "cider/log-add-consumer"
                  "framework" ,(cider-log-framework-id framework)
                  "appender" ,(cider-log-appender-id appender)
                  "filters" ,(cider-log-consumer-filters consumer))
                (cider-nrepl-send-request callback)))

(defun cider-request:log-analyze-stacktrace (framework appender event &optional callback)
  "Analyze the EVENT stacktrace of the APPENDER of FRAMEWORK and call CALLBACK."
  (cider-ensure-op-supported "cider/log-analyze-stacktrace")
  (thread-first `("op" "cider/log-analyze-stacktrace"
                  "framework" ,(cider-log-framework-id framework)
                  "appender" ,(cider-log-appender-id appender)
                  "event" ,(cider-log-event-id event))
                (cider-nrepl-send-request callback)))

(defun cider-sync-request:log-update-consumer (framework appender consumer)
  "Add CONSUMER to the APPENDER of FRAMEWORK and call CALLBACK on log events."
  (cider-ensure-op-supported "cider/log-update-consumer")
  (thread-first `("op" "cider/log-update-consumer"
                  "framework" ,(cider-log-framework-id framework)
                  "appender" ,(cider-log-appender-id appender)
                  "consumer" ,(cider-log-consumer-id consumer)
                  "filters" ,(cider-log-consumer-filters consumer))
                (cider-nrepl-send-sync-request)
                (nrepl-dict-get "cider/log-update-consumer")))

(defun cider-sync-request:log-add-appender (framework appender)
  "Add the APPENDER to the log FRAMEWORK."
  (cider-ensure-op-supported "cider/log-add-appender")
  (thread-first `("op" "cider/log-add-appender"
                  "framework" ,(cider-log-framework-id framework)
                  "appender" ,(cider-log-appender-id appender)
                  "filters" ,(cider-log-appender-filters appender)
                  "size" ,(cider-log-appender-size appender)
                  "threshold" ,(cider-log-appender-threshold appender))
                (cider-nrepl-send-sync-request)
                (nrepl-dict-get "cider/log-add-appender")))

(defun cider-sync-request:log-update-appender (framework appender)
  "Update the APPENDER of the log FRAMEWORK."
  (cider-ensure-op-supported "cider/log-update-appender")
  (thread-first `("op" "cider/log-update-appender"
                  "framework" ,(cider-log-framework-id framework)
                  "appender" ,(cider-log-appender-id appender)
                  "filters" ,(cider-log-appender-filters appender)
                  "size" ,(cider-log-appender-size appender)
                  "threshold" ,(cider-log-appender-threshold appender))
                (cider-nrepl-send-sync-request)
                (nrepl-dict-get "cider/log-update-appender")))

(defun cider-sync-request:log-clear (framework appender)
  "Clear the log events for FRAMEWORK and APPENDER."
  (cider-ensure-op-supported "cider/log-clear-appender")
  (thread-first `("op" "cider/log-clear-appender"
                  "framework" ,(cider-log-framework-id framework)
                  "appender" ,(cider-log-appender-id appender))
                (cider-nrepl-send-sync-request)
                (nrepl-dict-get "cider/log-clear-appender")))

(defun cider-sync-request:log-inspect-event (framework appender event)
  "Inspect the log event with the ID in the APPENDER of the log FRAMEWORK."
  (cider-ensure-op-supported "cider/log-inspect-event")
  (thread-first `("op" "cider/log-inspect-event"
                  "framework" ,(cider-log-framework-id framework)
                  "appender" ,(cider-log-appender-id appender)
                  "event" ,(cider-log-event-id event))
                (cider-nrepl-send-sync-request)
                (nrepl-dict-get "value")))

(defun cider-sync-request:log-format-event (framework appender event)
  "Format the log EVENT from the APPENDER of the log FRAMEWORK."
  (cider-ensure-op-supported "cider/log-format-event")
  (thread-first
    (seq-mapcat #'identity
                (map-merge 'list
                           (cider--nrepl-print-request-map fill-column)
                           `(("op" "cider/log-format-event")
                             ("framework" ,(cider-log-framework-id framework))
                             ("appender" ,(cider-log-appender-id appender))
                             ("event" ,(cider-log-event-id event)))))
    (cider-nrepl-send-sync-request)
    (nrepl-dict-get "cider/log-format-event")))

(defun cider-sync-request:log-frameworks ()
  "Return the available log frameworks."
  (cider-ensure-op-supported "cider/log-frameworks")
  (thread-first `("op" "cider/log-frameworks")
                (cider-nrepl-send-sync-request)
                (nrepl-dict-get "cider/log-frameworks")))

(cl-defun cider-sync-request:log-search (framework appender &key filters limit offset)
  "Search log events of FRAMEWORK and APPENDER using FILTERS, LIMIT and OFFSET."
  (cider-ensure-op-supported "cider/log-search")
  (thread-first `("op" "cider/log-search"
                  "framework" ,(cider-log-framework-id framework)
                  "appender" ,(cider-log-appender-id appender)
                  "filters" ,filters
                  "limit" ,limit
                  "offset" ,offset)
                (cider-nrepl-send-sync-request)
                (nrepl-dict-get "cider/log-search")))

(defun cider-sync-request:log-exceptions (framework appender)
  "Return the Cider log exceptions for FRAMEWORK and APPENDER."
  (cider-ensure-op-supported "cider/log-exceptions")
  (thread-first `("op" "cider/log-exceptions"
                  "framework" ,(cider-log-framework-id framework)
                  "appender" ,(cider-log-appender-id appender))
                (cider-nrepl-send-sync-request)
                (nrepl-dict-get "cider/log-exceptions")))

(defun cider-sync-request:log-levels (framework appender)
  "Return the Cider log levels for FRAMEWORK and APPENDER."
  (cider-ensure-op-supported "cider/log-levels")
  (thread-first `("op" "cider/log-levels"
                  "framework" ,(cider-log-framework-id framework)
                  "appender" ,(cider-log-appender-id appender))
                (cider-nrepl-send-sync-request)
                (nrepl-dict-get "cider/log-levels")))

(defun cider-sync-request:log-loggers (framework appender)
  "Return the Cider loggers for FRAMEWORK and APPENDER."
  (cider-ensure-op-supported "cider/log-loggers")
  (thread-first `("op" "cider/log-loggers"
                  "framework" ,(cider-log-framework-id framework)
                  "appender" ,(cider-log-appender-id appender))
                (cider-nrepl-send-sync-request)
                (nrepl-dict-get "cider/log-loggers")))

(defun cider-sync-request:log-remove-appender (framework appender)
  "Remove the APPENDER from the log FRAMEWORK."
  (cider-ensure-op-supported "cider/log-remove-appender")
  (thread-first `("op" "cider/log-remove-appender"
                  "framework" ,(cider-log-framework-id framework)
                  "appender" ,(cider-log-appender-id appender))
                (cider-nrepl-send-sync-request)
                (nrepl-dict-get "cider/log-remove-appender")))

(defun cider-sync-request:log-remove-consumer (framework appender consumer)
  "Remove the CONSUMER from the APPENDER of the log FRAMEWORK."
  (cider-ensure-op-supported "cider/log-remove-consumer")
  (thread-first `("op" "cider/log-remove-consumer"
                  "framework" ,(cider-log-framework-id framework)
                  "appender" ,(cider-log-appender-id appender)
                  "consumer" ,(cider-log-consumer-id consumer))
                (cider-nrepl-send-sync-request)
                (nrepl-dict-get "cider/log-remove-consumer")))

(defun cider-sync-request:log-threads (framework appender)
  "Return the threads for FRAMEWORK and APPENDER."
  (cider-ensure-op-supported "cider/log-threads")
  (thread-first `("op" "cider/log-threads"
                  "framework" ,(cider-log-framework-id framework)
                  "appender" ,(cider-log-appender-id appender))
                (cider-nrepl-send-sync-request)
                (nrepl-dict-get "cider/log-threads")))

(defun cider-log--completion-extra-properties (keys &optional separator)
  "Return the completion properties for NREPL dictionaries.

The KEYS are used to lookup the values and are joined by SEPARATOR."
  `(:annotation-function
    ,(lambda (identifier)
       (when-let (dict (cadr (assoc identifier minibuffer-completion-table)))
         (let ((annotation (string-join (seq-map (lambda (key) (nrepl-dict-get dict key)) keys)
                                        (or separator " "))))
           (unless (string-blank-p annotation)
             (propertize (format " - %s" (cider-log--strip-whitespace annotation))
                         'face 'font-lock-comment-face)))))))

(defun cider-log--read-appender-id (prompt initial-input history)
  "Read a appender from the minibuffer using PROMPT, INITIAL-INPUT and HISTORY."
  (let ((table (when cider-log-framework
                 (when-let (framework (cider-log-framework-reload cider-log-framework))
                   (seq-map #'cider-log-appender-id (cider-log-framework-appenders framework))))))
    (completing-read (or prompt "Log appender: ") table nil nil
                     (or initial-input cider-log-appender-id)
                     history cider-log-appender-id)))

(defun cider-log--read-buffer (&optional prompt initial-input history)
  "Read the log buffer name using PROMPT, INITIAL-INPUT and HISTORY."
  (let ((table (seq-map #'buffer-name (cider-log--buffers-in-major-mode 'cider-log-mode))))
    (completing-read (or prompt "Buffer: ") table nil nil
                     (or initial-input cider-log-buffer)
                     history cider-log-buffer)))

(defun cider-log--read-exceptions (&optional prompt initial-input history)
  "Read a list of exceptions using PROMPT, INITIAL-INPUT and HISTORY."
  (let ((table (when (cider-log-appender-attached-p)
                 (nrepl-dict-keys (cider-sync-request:log-exceptions
                                   cider-log-framework cider-log-appender)))))
    (completing-read-multiple (or prompt "Exceptions: ") table nil nil initial-input history)))

(defun cider-log--read-framework-name (&optional prompt initial-input history)
  "Read a framework name using PROMPT, INITIAL-INPUT and HISTORY."
  (let ((completion-extra-properties (cider-log--completion-extra-properties '("name")))
        (frameworks (cider-sync-request:log-frameworks)))
    (completing-read (or prompt "Log framework: ")
                     (seq-map (lambda (framework)
                                (list (cider-log-framework-name framework) framework))
                              frameworks)
                     nil nil initial-input history)))

(defun cider-log--read-level (&optional prompt initial-input history)
  "Read a log level using PROMPT, INITIAL-INPUT and HISTORY."
  (let ((table (when cider-log-framework (cider-log-framework-level-names cider-log-framework))))
    (completing-read (or prompt "Level: ") table nil nil initial-input history)))

(defun cider-log--read-loggers (&optional prompt initial-input history)
  "Read a list of loggers using PROMPT, INITIAL-INPUT and HISTORY."
  (let ((table (when (cider-log-appender-attached-p)
                 (nrepl-dict-keys (cider-sync-request:log-loggers
                                   cider-log-framework cider-log-appender)))))
    (completing-read-multiple (or "Loggers: " prompt) table nil nil initial-input history)))

(defun cider-log--read-number-N0 (&optional prompt initial-input history)
  "Read a natural number (including zero) using PROMPT, INITIAL-INPUT and HISTORY."
  (when-let (value (transient-read-number-N0 (or prompt "Number: ") initial-input history))
    (string-to-number value)))

(defun cider-log--read-number-N+ (&optional prompt initial-input history)
  "Read a natural number (excluding zero) using PROMPT, INITIAL-INPUT and HISTORY."
  (when-let (value (transient-read-number-N+ (or prompt "Number: ") initial-input history))
    (string-to-number value)))

(defun cider-log--read-threads (&optional prompt initial-input history)
  "Read a list of threads using PROMPT, INITIAL-INPUT and HISTORY."
  (let ((table (when (cider-log-appender-attached-p)
                 (nrepl-dict-keys (cider-sync-request:log-threads
                                   cider-log-framework cider-log-appender)))))
    (completing-read-multiple (or prompt "Threads: ") table nil nil initial-input history)))

(defun cider-log--read-time (&optional prompt initial-input _)
  "Read a time from the minibuffer using PROMPT and INITIAL-INPUT."
  (cider-log--format-time (org-read-date t 'to-time nil prompt nil initial-input)))

;; Log Framework

(defun cider-log-framework-appender (framework id)
  "Return the appender of the log FRAMEWORK with the given ID."
  (seq-find (lambda (appender) (equal id (cider-log-appender-id appender)))
            (cider-log-framework-appenders framework)))

(defun cider-log-framework-appenders (framework)
  "Return the appenders of the log FRAMEWORK."
  (nrepl-dict-get framework "appenders"))

(defun cider-log-framework-id (framework)
  "Return the id of the log FRAMEWORK."
  (nrepl-dict-get framework "id"))

(defun cider-log-framework-javadoc-url (framework)
  "Return the Javadoc URL of the log FRAMEWORK."
  (nrepl-dict-get framework "javadoc-url"))

(defun cider-log-framework-name (framework)
  "Return the name of the log FRAMEWORK."
  (nrepl-dict-get framework "name"))

(defun cider-log-framework-level-names (framework)
  "Return the log level names of the log FRAMEWORK."
  (seq-map (lambda (level) (nrepl-dict-get level "name"))
           (nrepl-dict-get framework "levels")))

(defun cider-log-framework-website-url (framework)
  "Return the website URL of the log FRAMEWORK."
  (nrepl-dict-get framework "website-url"))

(defun cider-log-framework-display-name (framework)
  "Return the display name of the log FRAMEWORK."
  (cider-log--bold (cider-log-framework-name framework)))

(defun cider-log-framework-add-appender (framework appender)
  "Add the APPENDER to the log FRAMEWORK."
  (cider-sync-request:log-add-appender framework appender))

(defun cider-log-framework-by-id (frameworks id)
  "Find the log framework in FRAMEWORKS by ID."
  (seq-find (lambda (framework) (equal id (cider-log-framework-id framework))) frameworks))

(defun cider-log-framework-by-name (frameworks name)
  "Find the log framework in FRAMEWORKS by NAME."
  (seq-find (lambda (framework) (equal name (cider-log-framework-name framework))) frameworks))

(defun cider-log-framework-reload (framework)
  "Reload the log FRAMEWORK."
  (cider-log-framework-by-id
   (cider-sync-request:log-frameworks)
   (cider-log-framework-id framework)))

;; Log Appender

(defun cider-log-appender-attached-p (&optional framework appender)
  "Return non-nil if the log APPENDER is attached to FRAMEWORK, otherwise nil."
  (when-let ((framework (or framework
                            (cider-log-framework-by-name
                             (cider-sync-request:log-frameworks)
                             cider-log-framework-name)))
             (appender-id (if appender
                              (cider-log-appender-id appender)
                            cider-log-appender-id)))
    (cider-log-framework-appender framework appender-id)))

(defun cider-log-appender-consumers (appender)
  "Return the consumers of the log APPENDER."
  (nrepl-dict-get appender "consumers"))

(defun cider-log-appender-id (appender)
  "Return the id of the log APPENDER."
  (nrepl-dict-get appender "id"))

(defun cider-log-appender-size (appender)
  "Return the size of the log APPENDER."
  (nrepl-dict-get appender "size"))

(defun cider-log-appender-threshold (appender)
  "Return the threshold of the log APPENDER."
  (nrepl-dict-get appender "threshold"))

(defun cider-log-appender-filters (appender)
  "Return the filters of the log APPENDER."
  (nrepl-dict-get appender "filters"))

(defun cider-log-appender-display-name (appender)
  "Return the display name of the log APPENDER."
  (cider-log--bold (cider-log-appender-id appender)))

(defun cider-log-appender-consumer (appender consumer)
  "Find the consumer in the log APPENDER by the id slot of CONSUMER."
  (let ((id (cider-log-consumer-id consumer)))
    (seq-find (lambda (consumer) (equal id (cider-log-consumer-id consumer)))
              (cider-log-appender-consumers appender))))

(defun cider-log-appender-reload (framework appender)
  "Reload the APPENDER of the log FRAMEWORK."
  (when-let (framework (cider-log-framework-reload framework))
    (cider-log-framework-appender framework (cider-log-appender-id appender))))

;; Log Consumer

(defun cider-log-consumer-attached-p (&optional framework appender consumer)
  "Return non-nil if the CONSUMER is attached to the APPENDER of FRAMEWORK."
  (when-let ((framework (or framework cider-log-framework))
             (appender (or appender cider-log-appender))
             (consumer (or consumer cider-log-consumer)))
    (cider-log-consumer-reload framework appender consumer)))

(defun cider-log-consumer-id (consumer)
  "Return the id of the log CONSUMER."
  (nrepl-dict-get consumer "id"))

(defun cider-log-consumer-filters (consumer)
  "Return the filters of the log CONSUMER."
  (nrepl-dict-get consumer "filters"))

(defun cider-log-consumer-buffers (consumer)
  "Find all buffers in which `cider-log-consumer' is bound to CONSUMER."
  (seq-filter (lambda (buffer)
                (with-current-buffer buffer
                  (and (nrepl-dict-p cider-log-consumer)
                       (equal (cider-log-consumer-id consumer)
                              (cider-log-consumer-id cider-log-consumer)))))
              (buffer-list)))

(defun cider-log-consumer-display-name (consumer)
  "Return the display name of the log CONSUMER."
  (cider-log--bold (cider-log-consumer-id consumer)))

(defun cider-log-consumer-reload (framework appender consumer)
  "Reload the CONSUMER attached to APPENDER of the log FRAMEWORK."
  (when-let (appender (cider-log-appender-reload framework appender))
    (cider-log-appender-consumer appender consumer)))

(defun cider-log--consumer-add (framework appender consumer buffer)
  "Add the CONSUMER to the APPENDER of FRAMEWORK and write events to BUFFER."
  (cider-request:log-add-consumer
   framework appender consumer
   (lambda (msg)
     (nrepl-dbind-response msg (cider/log-add-consumer cider/log-consumer cider/log-event status)
       (cond ((member "done" status)
              (with-current-buffer (get-buffer-create buffer)
                (setq-local cider-log-framework framework)
                (setq-local cider-log-appender appender)
                (setq cider-log-consumer cider/log-add-consumer)
                (switch-to-buffer buffer)))
             ((member "cider/log-event" status)
              (let* ((consumer (nrepl-dict "id" cider/log-consumer))
                     (buffers (cider-log-consumer-buffers consumer)))
                (when (seq-empty-p buffers)
                  (message "WARNING: No buffers found for %s log consumer %s of appender %s."
                           (cider-log-framework-display-name framework)
                           (cider-log-consumer-display-name consumer)
                           (cider-log-appender-display-name appender))
                  (cider-sync-request:log-remove-consumer framework appender consumer))
                (seq-doseq (buffer buffers)
                  (with-current-buffer buffer
                    (cider-log--insert-events buffer (list cider/log-event))
                    (when (and cider-log-use-logview (not (logview-initialized-p)))
                      (let ((framework cider-log-framework)
                            (appender cider-log-appender)
                            (consumer cider-log-consumer))
                        (logview--guess-submode)
                        (cider-log-mode)
                        ;; Restore buffer local vars reset by calling major mode.
                        (setq-local cider-log-framework framework
                                    cider-log-appender appender
                                    cider-log-consumer consumer))))))))))))

(defun cider-log--remove-current-buffer-consumer ()
  "Cleanup the log consumer of the current buffer."
  (when-let ((framework cider-log-framework)
             (appender cider-log-appender)
             (consumer cider-log-consumer))
    (setq-local cider-log-consumer nil)
    (when-let (consumer (cider-log-consumer-reload framework appender consumer))
      (cider-sync-request:log-remove-consumer framework appender consumer)
      consumer)))

;; Event

(defun cider-log-event-id (event)
  "Return the id of the log EVENT."
  (nrepl-dict-get event "id"))

(defun cider-log-event-exception (event)
  "Return the exception of the log EVENT."
  (nrepl-dict-get event "exception"))

(defun cider-log-event--format-logback (event)
  "Format the log EVENT in logview's Logback format."
  (nrepl-dbind-response event (_exception level logger message thread timestamp)
    (propertize (format "%s [%s] %s %s - %s\n"
                        (if (numberp timestamp)
                            (format-time-string "%F %T.%3N" (/ timestamp 1000))
                          (format "%s" timestamp))
                        thread
                        (upcase level)
                        logger
                        (if (and (stringp message)
                                 (numberp cider-log-max-message-length))
                            (substring message 0 (min (length message) cider-log-max-message-length))
                          ""))
                :cider-log-event event)))

(defun cider-log-event--pretty-print (framework appender event)
  "Format the log EVENT of FRAMEWORK and APPENDER."
  (when-let (event (cider-sync-request:log-format-event framework appender event))
    (cider-popup-buffer cider-log-event-buffer cider-auto-select-error-buffer
                        'clojure-mode 'ancillary)
    (with-current-buffer cider-log-event-buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert event)
        (goto-char (point-min))))))

(defun cider-log-event--inspect (framework appender event)
  "Inspect the log EVENT of FRAMEWORK and APPENDER."
  (thread-last (cider-sync-request:log-inspect-event framework appender event)
               (cider-inspector--render-value)))

(defun cider-log--insert-events (buffer events)
  "Insert the log EVENTS into BUFFER."
  (with-current-buffer (get-buffer-create buffer)
    (let ((windows (seq-filter (lambda (window) (= (window-point window) (point-max)))
                               (get-buffer-window-list buffer))))
      (save-excursion
        (let ((inhibit-read-only t))
          (goto-char (point-max))
          (seq-doseq (event events)
            (insert (cider-log-event--format-logback event)))))
      (seq-doseq (window windows)
        (set-window-point window (point-max))))))

(defun cider-log-event--show-stacktrace (framework appender event)
  "Show the stacktrace of the log EVENT of FRAMEWORK and APPENDER."
  (when (and framework appender event (cider-log-event-exception event))
    (let ((auto-select-buffer cider-auto-select-error-buffer)
          (causes nil))
      (cider-request:log-analyze-stacktrace
       framework appender event
       (lambda (response)
         (nrepl-dbind-response response (class status)
           (cond (class  (setq causes (cons response causes)))
                 (status (when causes
                           (cider-stacktrace-render
                            (cider-popup-buffer cider-error-buffer
                                                auto-select-buffer
                                                #'cider-stacktrace-mode
                                                'ancillary)
                            (reverse causes)))))))))))

(defun cider-log-event-next-line (&optional n)
  "Move N lines forward."
  (interactive "p")
  (let ((n (or n 1)))
    (forward-line n)
    (beginning-of-line)
    (when-let ((framework cider-log-framework)
               (appender cider-log-appender)
               (event (cider-log-event-at-point)))
      (let ((cider-auto-select-error-buffer nil))
        (save-window-excursion
          (when (get-buffer-window cider-error-buffer)
            (cider-log-event--show-stacktrace framework appender event))
          (when (get-buffer-window cider-inspector-buffer)
            (cider-log-event--inspect framework appender event))
          (when (get-buffer-window cider-log-event-buffer)
            (cider-log-event--pretty-print framework appender event)))))))

(defun cider-log-event-previous-line (&optional n)
  "Move N lines backward."
  (interactive "p")
  (cider-log-event-next-line (- (or n 1))))

(defun cider-log--set-filters (filters)
  "Set the filter variables from the NREPL dict FILTERS."
  (when filters
    (setq cider-log--end-time-filter (nrepl-dict-get filters "end-time")
          cider-log--exceptions-filter (nrepl-dict-get filters "exceptions")
          cider-log--level-filter (nrepl-dict-get filters "level")
          cider-log--loggers-filter (nrepl-dict-get filters "loggers")
          cider-log--pattern-filter (nrepl-dict-get filters "pattern")
          cider-log--start-time-filter (nrepl-dict-get filters "start-time")
          cider-log--threads-filter (nrepl-dict-get filters "threads"))))

(defun cider-log--ensure-initialized (framework &optional appender consumer)
  "Ensure that the given FRAMEWORK, APPENDER and CONSUMER are initialized."
  (setq cider-log-framework framework
        cider-log-framework-name (cider-log-framework-name framework))
  (when appender
    (setq cider-log-appender appender
          cider-log-appender-id (cider-log-appender-id appender)
          cider-log-appender-size (cider-log-appender-size appender)
          cider-log-appender-threshold (cider-log-appender-threshold appender))
    (cider-log--set-filters (cider-log-appender-filters appender)))
  (when consumer
    (setq cider-log-consumer consumer)
    (cider-log--set-filters (cider-log-consumer-filters appender)))
  (when (and appender (not cider-log--initialized-once-p))
    (unless (cider-log-appender-reload framework appender)
      (setq cider-log-appender (cider-sync-request:log-add-appender framework appender))
      (setq cider-log--initialized-once-p t))))

(defun cider-log-kill-buffer-hook-handler ()
  "Called from `kill-buffer-hook' to remove the consumer."
  (when (eq 'cider-log-mode major-mode)
    (when-let ((framework cider-log-framework)
               (appender cider-log-appender)
               (consumer cider-log-consumer))
      (cider-log--remove-current-buffer-consumer)
      (message "Removed %s event consumer %s from appender %s."
               (cider-log-framework-display-name framework)
               (cider-log-consumer-display-name consumer)
               (cider-log-appender-display-name appender)))))

(defun cider-log-select-framework ()
  "Select the log framework."
  (let ((frameworks (cider-sync-request:log-frameworks)))
    (cond ((= 1 (length frameworks))
           (car frameworks))
          (t (let ((name (cider-log--read-framework-name)))
               (cider-log-framework-by-name frameworks name))))))

(defun cider-log--current-framework ()
  "Return the log framework by the name bound to `cider-log-framework-name'."
  (when cider-log-framework-name
    (let ((frameworks (cider-sync-request:log-frameworks)))
      (cider-log-framework-by-name frameworks cider-log-framework-name))))

(defun cider-log--framework ()
  "Return the current log framework, or select it."
  (or (cider-log--current-framework) (cider-log-select-framework)))

(defun cider-log--appender ()
  "Return the current log appender."
  (when cider-log-appender-id
    (nrepl-dict "id" cider-log-appender-id
                "filters" (cider-log--filters)
                "size" cider-log-appender-size
                "threshold" cider-log-appender-threshold)))

(defun cider-log--consumer ()
  "Return the current log consumer."
  (let ((consumer (nrepl-dict "filters" (cider-log--filters))))
    (when cider-log-consumer
      (nrepl-dict-put consumer "id" (cider-log-consumer-id cider-log-consumer)))
    consumer))

(defun cider-log--event-options ()
  "Return the current log consumer."
  (nrepl-dict "filters" (cider-log--filters)
              "limit" cider-log-pagination-limit
              "offset" cider-log-pagination-offset))

(defun cider-log--filters ()
  "Return the log event filters."
  (nrepl-dict
   "end-time" cider-log--end-time-filter
   "exceptions" cider-log--exceptions-filter
   "level" cider-log--level-filter
   "loggers" cider-log--loggers-filter
   "pattern" cider-log--pattern-filter
   "start-time" cider-log--start-time-filter
   "threads" cider-log--threads-filter))

(defun cider-log-event-at-point ()
  "Return the log event at point."
  (get-text-property (point) :cider-log-event))

;;;###autoload (autoload 'cider-log-info "cider-log-info" "Show the Cider log current log buffer, framework, appender and consumer." t)
(defun cider-log-info ()
  "Show the current log buffer, framework, appender and consumer."
  (interactive)
  (message "%s"
           (string-join
            (list (when cider-log-buffer
                    (format "Buffer: %s" (cider-log--bold cider-log-buffer)))
                  (when cider-log-framework-name
                    (format "Framework: %s" (cider-log--bold cider-log-framework-name)))
                  (when cider-log-appender-id
                    (format "Appender: %s" (cider-log--bold cider-log-appender-id)))
                  (when-let (id (and cider-log-consumer (cider-log-consumer-id cider-log-consumer)))
                    (format "Consumer: %s" (cider-log--bold id))))
            " ")))

;; Major mode

(defvar cider-log-mode-map
  (let ((map (make-sparse-keymap))
        (parent (if cider-log-use-logview logview-mode-map special-mode-map)))
    (set-keymap-parent map parent)
    (define-key map (kbd "C-c M-l a") #'cider-log-appender)
    (define-key map (kbd "C-c M-l c") #'cider-log-consumer)
    (define-key map (kbd "C-c M-l e") #'cider-log-event)
    (define-key map (kbd "C-c M-l f") #'cider-log-framework)
    (define-key map (kbd "C-c M-l i") #'cider-log-info)
    (define-key map (kbd "C-c M-l l") #'cider-log)
    (define-key map (kbd "E") 'cider-log-show-stacktrace)
    (define-key map (kbd "F") 'cider-log-print-event)
    (define-key map (kbd "I") 'cider-log-inspect-event)
    (define-key map (kbd "RET") 'cider-log-inspect-event)
    (define-key map (kbd "n") 'cider-log-event-next-line)
    (define-key map (kbd "p") 'cider-log-event-previous-line)
    map)
  "The Cider log stream mode key map.")

(defun cider-log--setup-mode ()
  "Setup CIDER log mode."
  (use-local-map cider-log-mode-map)
  (setq-local electric-indent-chars nil)
  (setq-local logview-show-ellipses nil)
  (setq-local sesman-system 'CIDER)
  (setq-local truncate-lines t)
  (when (fboundp 'evil-set-initial-state)
    (evil-set-initial-state 'cider-log-mode 'emacs)))

(defvar cider-log--mode-doc
  "Major mode for inspecting Clojure log events.

CIDER Log Mode allows you to capture, debug, inspect and view log events
emitted by Java logging frameworks.  The captured log events can be
searched, streamed to the client, pretty printed and are integrated with
the CIDER Inspector and the CIDER stacktrace mode.

\\{cider-log-mode-map}")

(if cider-log-use-logview
    (define-derived-mode cider-log-mode logview-mode "Cider Log" cider-log--mode-doc
      (cider-log--setup-mode))
  (define-derived-mode cider-log-mode special-mode "Cider Log" cider-log--mode-doc
      (cider-log--setup-mode)))

;; Transient Lisp Variable

(defclass cider-log--lisp-variable (transient-lisp-variable) ())

(cl-defmethod transient-init-value ((obj cider-log--lisp-variable))
  "Set the initial value of the object OBJ."
  (let* ((prefix-value (oref transient--prefix value))
         (value (cdr (assoc (oref obj variable) prefix-value)))
         (new-value (if (assoc (oref obj variable) prefix-value)
                        value
                      (symbol-value (oref obj variable)))))
    (funcall (oref obj set-value)
             (oref obj variable)
             (oset obj value new-value))))

(cl-defmethod transient-infix-set ((obj cider-log--lisp-variable) value)
  "Set the value of infix object OBJ to VALUE."
  (funcall (oref obj set-value)
           (oref obj variable)
           (oset obj value value)))

(cl-defmethod transient-infix-value ((obj cider-log--lisp-variable))
  "Return the value of the suffix object OBJ."
  (cons (oref obj variable) (oref obj value)))

(cl-defmethod transient-format-value ((obj cider-log--lisp-variable))
  "Format OBJ's value for display and return the result."
  (propertize (prin1-to-string (oref obj value))
              'face 'transient-value))

(cl-defmethod transient-format-value ((obj cider-log--lisp-variable))
  "Format OBJ's value for display and return the result."
  (cider-log--format-value (oref obj value)))

;; Transient options

(transient-define-infix cider-log--appender-size-option ()
  :always-read t
  :argument "--size="
  :class 'cider-log--lisp-variable
  :description "Appender size"
  :key "=s"
  :prompt "Size: "
  :reader #'cider-log--read-number-N+
  :variable 'cider-log-appender-size)

(transient-define-infix cider-log--appender-threshold-option ()
  :always-read t
  :argument "--threshold="
  :class 'cider-log--lisp-variable
  :description "Appender threshold"
  :key "=t"
  :prompt "Threshold: "
  :reader #'cider-log--read-number-N+
  :variable 'cider-log-appender-threshold)

(transient-define-infix cider-log--buffer-option ()
  :always-read t
  :class 'cider-log--lisp-variable
  :description "Buffer"
  :key "=b"
  :prompt "Log buffer: "
  :reader #'cider-log--read-buffer
  :variable 'cider-log-buffer)

(transient-define-infix cider-log--end-time-option ()
  :argument "--end-time="
  :class 'cider-log--lisp-variable
  :description "Filter by end time"
  :key "-e"
  :prompt "End time: "
  :reader #'cider-log--read-time
  :variable 'cider-log--end-time-filter)

(transient-define-infix cider-log--exceptions-option ()
  :argument "--exceptions="
  :class 'cider-log--lisp-variable
  :description "Filter by exceptions"
  :key "-E"
  :multi-value t
  :prompt "Exceptions: "
  :reader #'cider-log--read-exceptions
  :variable 'cider-log--exceptions-filter)

(transient-define-infix cider-log--level-option ()
  :argument "--level="
  :class 'cider-log--lisp-variable
  :description "Filter by level"
  :key "-l"
  :prompt "Log Level: "
  :reader #'cider-log--read-level
  :variable 'cider-log--level-filter)

(transient-define-infix cider-log--limit-option ()
  :always-read t
  :argument "--limit="
  :class 'cider-log--lisp-variable
  :description "Limit"
  :key "=l"
  :prompt "Limit: "
  :reader #'cider-log--read-number-N+
  :variable 'cider-log-pagination-limit)

(transient-define-infix cider-log--logger-option ()
  :argument "--loggers="
  :class 'cider-log--lisp-variable
  :description "Filter by loggers"
  :key "-L"
  :multi-value t
  :prompt "Loggers: "
  :reader #'cider-log--read-loggers
  :variable 'cider-log--loggers-filter)

(transient-define-infix cider-log--offset-option ()
  :always-read t
  :argument "--offset="
  :class 'cider-log--lisp-variable
  :description "Offset"
  :key "=o"
  :prompt "Offset: "
  :reader #'cider-log--read-number-N0
  :variable 'cider-log-pagination-offset)

(transient-define-infix cider-log--pattern-option ()
  :argument "--pattern="
  :class 'cider-log--lisp-variable
  :description "Filter by regex pattern"
  :key "-r"
  :prompt "Regex pattern: "
  :reader #'read-string
  :variable 'cider-log--pattern-filter)

(transient-define-infix cider-log--start-time-option ()
  :argument "--start-time="
  :class 'cider-log--lisp-variable
  :description "Filter by start time"
  :key "-s"
  :prompt "Start time: "
  :reader #'cider-log--read-time
  :variable 'cider-log--start-time-filter)

(transient-define-infix cider-log--threads-option ()
  :argument "--threads="
  :class 'cider-log--lisp-variable
  :description "Filter by threads"
  :key "-t"
  :multi-value t
  :prompt "Threads: "
  :reader #'cider-log--read-threads
  :variable 'cider-log--threads-filter)

;; Framework actions

(transient-define-suffix cider-log-browse-javadocs (framework)
  "Browse the Javadoc of the log FRAMEWORK."
  :description "Browse Java documentation"
  (interactive (list (cider-log--framework)))
  (browse-url (or (cider-log-framework-javadoc-url framework)
                  (user-error (format "The %s framework does not have Javadocs."
                                      (cider-log-framework-name framework))))))

(transient-define-suffix cider-log-browse-website (framework)
  "Browse the website of the log FRAMEWORK."
  :description "Browse website"
  (interactive (list (cider-log--framework)))
  (browse-url (or (cider-log-framework-website-url framework)
                  (user-error (format "The %s framework does not have a website."
                                      (cider-log-framework-name framework))))))

(transient-define-suffix cider-log-set-framework (framework-name)
  "Set the current log framework to FRAMEWORK-NAME."
  :description #'cider-log--description-set-framework
  (interactive (list (cider-log--read-framework-name)))
  (setq cider-log-framework-name framework-name))

(transient-define-suffix cider-log-set-buffer (buffer)
  "Set the current log buffer to BUFFER."
  :description #'cider-log--description-set-buffer
  (interactive (list (cider-log--read-buffer)))
  (setq cider-log-buffer buffer))

;; Appender actions

(transient-define-suffix cider-log-clear-appender (framework appender)
  "Clear the log events of the APPENDER of FRAMEWORK."
  :description "Clear log appender"
  :inapt-if-not #'cider-log-appender-attached-p
  (interactive (list (cider-log--framework) (cider-log--appender)))
  (cider-sync-request:log-clear framework appender)
  (message "Cleared the %s log appender of the %s framework."
           (cider-log-appender-display-name appender)
           (cider-log-framework-display-name framework)))

(transient-define-suffix cider-log-kill-appender (framework appender)
  "Remove the log APPENDER from FRAMEWORK."
  :description "Kill log appender"
  :inapt-if-not #'cider-log-appender-attached-p
  (interactive (list (cider-log--framework) (cider-log--appender)))
  (cider-sync-request:log-remove-appender framework appender)
  (setq-local cider-log-consumer nil)
  (message "Log appender %s removed from the %s framework."
           (cider-log-framework-display-name framework)
           (cider-log-appender-display-name appender)))

(transient-define-suffix cider-log--do-add-appender (framework appender)
  "Add the APPENDER to the log FRAMEWORK."
  :description "Add log appender"
  :inapt-if #'cider-log-appender-attached-p
  (interactive (list (cider-log--framework) (cider-log--appender)))
  (setq cider-log-appender (cider-sync-request:log-add-appender framework appender))
  (message "Log appender %s added to the %s framework."
           (cider-log-appender-display-name appender)
           (cider-log-framework-display-name framework)))

(transient-define-suffix cider-log--do-update-appender (framework appender)
  "Update the APPENDER of the log FRAMEWORK."
  :description "Update log appender"
  :inapt-if-not #'cider-log-appender-attached-p
  (interactive (list (cider-log--framework) (cider-log--appender)))
  (setq cider-log-appender (cider-sync-request:log-update-appender framework appender))
  (message "Updated log appender %s of the %s framework."
           (cider-log-appender-display-name appender)
           (cider-log-framework-display-name framework)))

;; Consumer actions

(transient-define-suffix cider-log--do-add-consumer (framework appender consumer buffer)
  "Add the CONSUMER to the APPENDER of the log FRAMEWORK and write events to BUFFER."
  :description "Add log consumer"
  :inapt-if #'cider-log-consumer-attached-p
  (interactive (list (cider-log--framework)
                     (cider-log--appender)
                     (cider-log--consumer)
                     (current-buffer)))
  (cider-log--consumer-add framework appender consumer buffer))

(transient-define-suffix cider-log-kill-consumer (framework appender consumer)
  "Remove the CONSUMER listening to the APPENDER of the log FRAMEWORK."
  :description "Kill log consumer"
  :inapt-if-not #'cider-log-consumer-attached-p
  (interactive (list (cider-log--framework) (cider-log--appender) (cider-log--consumer)))
  (cider-sync-request:log-remove-consumer framework appender consumer)
  (setq-local cider-log-consumer nil)
  (message "Removed %s log consumer %s for appender %s."
           (cider-log-framework-display-name framework)
           (cider-log-consumer-display-name consumer)
           (cider-log-appender-display-name appender)))

(transient-define-suffix cider-log--do-update-consumer (framework appender consumer)
  "Update the CONSUMER listening to the APPENDER of the log FRAMEWORK."
  :description "Update log consumer"
  :inapt-if-not #'cider-log-consumer-attached-p
  (interactive (list (cider-log--framework)
                     (cider-log--appender)
                     (cider-log--consumer)))
  (setq cider-log-consumer (cider-sync-request:log-update-consumer framework appender consumer))
  (message "Updated %s log consumer %s for appender %s."
           (cider-log-framework-display-name framework)
           (cider-log-consumer-display-name consumer)
           (cider-log-appender-display-name appender)))

;; Event actions

(transient-define-suffix cider-log-show-stacktrace (framework appender event)
  "Show the stacktrace of the log EVENT of FRAMEWORK and APPENDER."
  :description "Show log event stacktrace"
  :if #'cider-log-event-at-point
  :inapt-if-not (lambda ()
                  (when-let (event (cider-log-event-at-point))
                    (cider-log-event-exception event)))
  (interactive (list (cider-log--framework) (cider-log--appender) (cider-log-event-at-point)))
  (cider-log-event--show-stacktrace framework appender event))

(transient-define-suffix cider-log-print-event (framework appender event)
  "Format the log EVENT of FRAMEWORK and APPENDER."
  :description "Pretty print log event at point"
  :if #'cider-log-event-at-point
  (interactive (list (cider-log--framework) (cider-log--appender) (cider-log-event-at-point)))
  (if event
      (cider-log-event--pretty-print framework appender event)
    (user-error "No log event found at point")))

(transient-define-suffix cider-log-inspect-event (framework appender event)
  "Inspect the log EVENT of FRAMEWORK and APPENDER."
  :description "Inspect log event at point"
  :if #'cider-log-event-at-point
  (interactive (list (cider-log--framework) (cider-log--appender) (cider-log-event-at-point)))
  (cider-log-event--inspect framework appender event))

(transient-define-suffix cider-log-clear-event-buffer (buffer)
  "Clear the Cider log events in BUFFER."
  :description #'cider-log--description-clear-events-buffer
  :inapt-if-not #'cider-log-buffer-clear-p
  (interactive (list cider-log-buffer))
  (when (get-buffer buffer)
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)))))

(transient-define-suffix cider-log-switch-to-buffer (buffer)
  "Switch to the Cider log event BUFFER."
  :description "Switch to the log event buffer"
  (interactive (list cider-log-buffer))
  (when (get-buffer-create buffer)
    (switch-to-buffer-other-window buffer)))

(transient-define-suffix cider-log--do-search-events (framework appender filters)
  "Search the log events of FRAMEWORK and APPENDER which match FILTERS."
  :description "Search log events"
  :inapt-if-not #'cider-log-appender-attached-p
  (interactive (list (cider-log--framework) (cider-log--appender) (cider-log--filters)))
  (with-current-buffer (get-buffer-create cider-log-buffer)
    (let ((consumer (nrepl-dict "filters" (cider-log--filters)))
          (inhibit-read-only t))
      (cider-log--remove-current-buffer-consumer)
      (erase-buffer)
      (let ((events (cider-sync-request:log-search
                     framework appender
                     :filters filters
                     :limit cider-log-pagination-limit
                     :offset cider-log-pagination-offset)))
        (seq-doseq (event (nreverse events))
          (insert (cider-log-event--format-logback event)))
        (cider-log-mode)
        (setq-local cider-log-framework framework)
        (setq-local cider-log-appender appender)
        (when (seq-empty-p events)
          (message "No log events found."))
        (cider-log--consumer-add framework appender consumer (current-buffer))))))

;; Log Framework Transient

;;;###autoload (autoload 'cider-log-framework "cider-log" "Show the Cider log framework menu." t)
(transient-define-prefix cider-log-framework (framework)
  "Show the Cider log framework menu."
  [["Cider Log Framework\n\nActions:"
    ("b" cider-log-set-buffer)
    ("j" cider-log-browse-javadocs)
    ("s" cider-log-set-framework)
    ("w" cider-log-browse-website)]]
  (interactive (list (cider-log--framework)))
  (cider-log--ensure-initialized framework)
  (transient-setup 'cider-log-framework))

;; Log Appender Transients

(defun cider-log--appender-interactive-list ()
  "Return the interactive arguments for a appender transient."
  (let ((framework (cider-log--current-framework)))
    (list framework (cider-log-framework-appender framework cider-log-appender-id))))

(transient-define-prefix cider-log-add-appender (framework appender)
  "Show the menu to add a Cider log appender."
  :history-key 'cider-log-appender
  ["Cider Log Appender\n\nSettings:"
   (cider-log--appender-size-option)
   (cider-log--appender-threshold-option)]
  ["Filters:"
   (cider-log--end-time-option)
   (cider-log--exceptions-option)
   (cider-log--level-option)
   (cider-log--logger-option)
   (cider-log--pattern-option)
   (cider-log--start-time-option)
   (cider-log--threads-option)]
  ["Actions"
   ("a" cider-log--do-add-appender)]
  (interactive (cider-log--appender-interactive-list))
  (cider-log--ensure-initialized framework appender)
  (transient-setup 'cider-log-add-appender))

(transient-define-prefix cider-log-update-appender (framework appender)
  "Show the menu to update a Cider log appender."
  :history-key 'cider-log-appender
  ["Cider Log Appender\n\nSettings:"
   (cider-log--appender-size-option)
   (cider-log--appender-threshold-option)]
  ["Filters:"
   (cider-log--end-time-option)
   (cider-log--exceptions-option)
   (cider-log--level-option)
   (cider-log--logger-option)
   (cider-log--pattern-option)
   (cider-log--start-time-option)
   (cider-log--threads-option)]
  ["Actions"
   ("u" cider-log--do-update-appender)]
  (interactive (cider-log--appender-interactive-list))
  (cider-log--ensure-initialized framework appender)
  (transient-setup 'cider-log-update-appender))

;;;###autoload (autoload 'cider-log-appender "cider-log" "Show the Cider log appender menu." t)
(transient-define-prefix cider-log-appender (framework appender)
  "Show the Cider log appender menu."
  :history-key 'cider-log-appender
  ["Cider Log Appender\n\nSettings:"
   (cider-log--appender-size-option)
   (cider-log--appender-threshold-option)]
  ["Filters:"
   (cider-log--end-time-option)
   (cider-log--exceptions-option)
   (cider-log--level-option)
   (cider-log--logger-option)
   (cider-log--pattern-option)
   (cider-log--start-time-option)
   (cider-log--threads-option)]
  ["Actions"
   ("a" cider-log--do-add-appender)
   ("c" cider-log-clear-appender)
   ("k" cider-log-kill-appender)
   ("u" cider-log--do-update-appender)]
  (interactive (cider-log--appender-interactive-list))
  (cider-log--ensure-initialized framework appender)
  (transient-setup 'cider-log-appender))

;; Log Consumer Transient Menu

(defun cider-log--consumer-interactive-list ()
  "Return the interactive arguments for a consumer transient."
  (let* ((framework (cider-log--current-framework))
         (appender (cider-log-framework-appender framework cider-log-appender-id)))
    (list framework appender
          (if (and appender cider-log-consumer)
              (seq-find (lambda (consumer)
                          (equal (cider-log-consumer-id cider-log-consumer)
                                 (cider-log-consumer-id consumer)))
                        (cider-log-appender-consumers appender))
            (nrepl-dict "filters" (cider-log--filters))))))

(transient-define-prefix cider-log-add-consumer (framework appender consumer)
  "Show the menu to add a Cider log consumer."
  :history-key 'cider-log-consumer
  ["Cider Log Consumer\n\nFilters:"
   (cider-log--end-time-option)
   (cider-log--exceptions-option)
   (cider-log--level-option)
   (cider-log--logger-option)
   (cider-log--pattern-option)
   (cider-log--start-time-option)
   (cider-log--threads-option)]
  ["Actions"
   ("a" cider-log--do-add-consumer)]
  (interactive (cider-log--consumer-interactive-list))
  (cider-log--ensure-initialized framework appender consumer)
  (transient-setup 'cider-log-add-consumer))

(transient-define-prefix cider-log-update-consumer (framework appender consumer)
  "Show the menu to update a Cider log consumer."
  :history-key 'cider-log-consumer
  ["Cider Log Consumer\n\nFilters:"
   (cider-log--end-time-option)
   (cider-log--exceptions-option)
   (cider-log--level-option)
   (cider-log--logger-option)
   (cider-log--pattern-option)
   (cider-log--start-time-option)
   (cider-log--threads-option)]
  ["Actions"
   ("u" cider-log--do-update-consumer)]
  (interactive (cider-log--consumer-interactive-list))
  (cider-log--ensure-initialized framework appender consumer)
  (transient-setup 'cider-log-update-consumer))

;;;###autoload (autoload 'cider-log-consumer "cider-log" "Show the Cider log consumer menu." t)
(transient-define-prefix cider-log-consumer (framework appender consumer)
  "Show the Cider log consumer menu."
  :history-key 'cider-log-consumer
  ["Cider Log Consumer\n\nFilters:"
   (cider-log--end-time-option)
   (cider-log--exceptions-option)
   (cider-log--level-option)
   (cider-log--logger-option)
   (cider-log--pattern-option)
   (cider-log--start-time-option)
   (cider-log--threads-option)]
  ["Actions"
   ("a" cider-log--do-add-consumer)
   ("k" cider-log-kill-consumer)
   ("u" cider-log--do-update-consumer)]
  (interactive (cider-log--consumer-interactive-list))
  (cider-log--ensure-initialized framework appender consumer)
  (transient-setup 'cider-log-consumer))

;; Log Event Transient Menu

(transient-define-prefix cider-log-event-search (framework appender)
  "Search the search log events menu."
  :history-key 'cider-log-event
  ["Cider Log Event\n\nPagination:"
   (cider-log--limit-option)
   (cider-log--offset-option)]
  ["Filters:"
   (cider-log--end-time-option)
   (cider-log--exceptions-option)
   (cider-log--level-option)
   (cider-log--logger-option)
   (cider-log--pattern-option)
   (cider-log--start-time-option)
   (cider-log--threads-option)]
  ["Actions"
   ("s" cider-log--do-search-events)]
  (interactive (list (cider-log--framework) (cider-log--appender)))
  (cider-log--ensure-initialized framework appender)
  (transient-setup 'cider-log-event-search))

;;;###autoload (autoload 'cider-log-event "cider-log" "Show the Cider log event menu." t)
(transient-define-prefix cider-log-event (framework appender)
  "Show the Cider log event menu."
  :history-key 'cider-log-event
  ["Cider Log Event\n\nPagination:"
   (cider-log--limit-option)
   (cider-log--offset-option)]
  ["Filters:"
   (cider-log--end-time-option)
   (cider-log--exceptions-option)
   (cider-log--level-option)
   (cider-log--logger-option)
   (cider-log--pattern-option)
   (cider-log--start-time-option)
   (cider-log--threads-option)]
  ["Actions"
   ("c" cider-log-clear-event-buffer)
   ("e" cider-log-show-stacktrace)
   ("i" cider-log-inspect-event)
   ("p" cider-log-print-event)
   ("s" cider-log--do-search-events)]
  (interactive (list (cider-log--framework) (cider-log--appender)))
  (cider-log--ensure-initialized framework appender)
  (transient-setup 'cider-log-event))

;; Main Transient Menu

;;;###autoload (autoload 'cider-log "cider-log" "Show the Cider log menu." t)
(transient-define-prefix cider-log (framework appender)
  "Show the Cider log menu."
  [["Framework Actions"
    ("fs" cider-log-set-framework)
    ("fb" cider-log-set-buffer)
    ("fj" cider-log-browse-javadocs)
    ("fw" cider-log-browse-website)]
   ["Appender Actions"
    ("aa" "Add log appender" cider-log-add-appender
     :inapt-if cider-log-appender-attached-p)
    ("ac" cider-log-clear-appender)
    ("ak" cider-log-kill-appender)
    ("am" "Manage appender" cider-log-appender)
    ("au" "Update log appender" cider-log-update-appender
     :inapt-if-not cider-log-appender-attached-p)]
   ["Consumer Actions"
    ("ca" "Add log consumer" cider-log-add-consumer
     :inapt-if cider-log-consumer-attached-p)
    ("ck" cider-log-kill-consumer)
    ("cm" "Manage consumer" cider-log-consumer)
    ("cu" "Update log consumer" cider-log-update-consumer
     :inapt-if-not cider-log-consumer-attached-p)]
   ["Event Actions"
    ("eb" cider-log-switch-to-buffer)
    ("ec" cider-log-clear-event-buffer)
    ("ee" cider-log-show-stacktrace)
    ("ei" cider-log-inspect-event)
    ("ep" cider-log-print-event)
    ("es" "Search log events" cider-log-event-search
     :inapt-if-not cider-log-appender-attached-p)]]
  (interactive (list (cider-log--framework) (cider-log--appender)))
  (cider-log--ensure-initialized framework appender)
  (transient-setup 'cider-log))

(add-hook 'kill-buffer-hook #'cider-log-kill-buffer-hook-handler)

(provide 'cider-log)

;;; cider-log.el ends here
