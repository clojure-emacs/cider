;;; cider-common-tests.el ---                        -*- lexical-binding: t; -*-

;; Copyright © 2012-2026 Tim King, Bozhidar Batsov

;; Author: Tim King <kingtim@gmail.com>
;;         Bozhidar Batsov <bozhidar@batsov.dev>
;;         Artur Malabarba <bruce.connor.am@gmail.com>

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
(require 'cider-common)
(require 'cider-session)

;; Please, for each `describe', ensure there's an `it' block, so that its execution is visible in CI.

;;; cider-common tests
(describe "cider-abbreviate-ns"
  (it "handles nil input"
    (expect (cider-abbreviate-ns nil) :to-be nil))

  (it "handles empty string input"
    (expect (cider-abbreviate-ns "") :to-equal ""))

  (it "shortens all ns segments but the last"
    (expect (cider-abbreviate-ns "some.test.ns") :to-equal "s.t.ns"))

  (it "handles single-segment namespaces"
    (expect (cider-abbreviate-ns "ns") :to-equal "ns")))

(describe "cider-last-ns-segment"
  (it "handles nil input"
    (expect (cider-last-ns-segment nil) :to-be nil))

  (it "handles empty string input"
    (expect (cider-last-ns-segment "") :to-equal ""))

  (it "drops all ns segments but the last"
    (expect (cider-last-ns-segment "some.test.ns") :to-equal "ns"))

  (it "handles single-segment namespaces"
    (expect (cider-last-ns-segment "ns") :to-equal "ns")))

(describe "cider--kw-to-symbol"
  (it "returns symbol form of the given keyword"
    (expect (cider--kw-to-symbol "symbol") :to-equal "symbol")
    (expect (cider--kw-to-symbol ":clj.core/str") :to-equal "clj.core/str")
    (expect (cider--kw-to-symbol "::keyword") :to-equal "keyword")
    (expect (cider--kw-to-symbol nil) :to-be nil)))

(describe "cider-make-tramp-prefix"
  (it "returns tramp-prefix only"
      ;;; The third parameter is a host.
      (expect (cider-make-tramp-prefix "ssh" "cider-devs" "192.168.50.9#22")
              :to-equal "/ssh:cider-devs@192.168.50.9#22:")
      (expect (cider-make-tramp-prefix "ssh" "cider-devs" "192.168.50.9")
              :to-equal "/ssh:cider-devs@192.168.50.9:")
      (expect (cider-make-tramp-prefix "ssh" "cider-devs" "192.168.50.9" "12345")
              :to-equal "/ssh:cider-devs@192.168.50.9#12345:")
      (expect (cider-make-tramp-prefix "ssh" "cider-devs" "192.168.50.9#12345")
              :to-equal "/ssh:cider-devs@192.168.50.9#12345:")
      ;;; These two cases are for using ssh config alias.
      (expect (cider-make-tramp-prefix "ssh" nil "test.cider.com")
              :to-equal "/ssh:test.cider.com:")
      (expect (cider-make-tramp-prefix "ssh" nil "test.local")
              :to-equal "/ssh:test.local:")))

(defun cider--translate-path-test (translations file direction)
  (let ((cider-path-translations translations))
    (cider--translate-path file direction)))

(defun cider--translate-path-from-nrepl-test (translations file)
  (let ((cider-path-translations translations))
    (cider--translate-path-from-nrepl file)))

(defun cider--translate-path-to-nrepl-test (translations file)
  (let ((cider-path-translations translations))
    (cider--translate-path-to-nrepl file)))

(describe "cider--translate-container-vm"
  ;; Unix absolute paths are not valid absolute paths on MS-Windows (they are
  ;; missing the driver letter). We use `expand-file-name' so as to add the
  ;; drive letter on windows.
  :var ((/docker/src (expand-file-name "/docker/src"))
        (/home/host/project/src (expand-file-name "/home/host/project/src"))
        (/home/host/random/file.clj (expand-file-name "/home/host/random/file.clj"))
        (/src (expand-file-name "/src"))
        (/host (expand-file-name "/host"))
        (/src/project/src/ns.clj (expand-file-name "/src/project/src/ns.clj"))
        (/host/project/src/ns.clj (expand-file-name "/host/project/src/ns.clj"))
        (/host/ (expand-file-name "/host/"))
        (/src/ (expand-file-name "/src/"))
        (/docker (expand-file-name "/docker"))
        (/docker/ns.clj (expand-file-name "/docker/ns.clj"))
        (/host/ns.clj (expand-file-name "/host/ns.clj"))
        (/home/host/project/src/namespace.clj (expand-file-name "/home/host/project/src/namespace.clj"))
        (/docker/src/namespace.clj (expand-file-name "/docker/src/namespace.clj"))
        (/host/project/host/ns.clj (expand-file-name "/host/project/host/ns.clj"))
        (/src/project/host/ns.clj (expand-file-name "/src/project/host/ns.clj")))
  (it "translates file paths from container/vm location to host location"
      (expect (cider--translate-path-test `((,/docker/src . ,/home/host/project/src)) /docker/src/namespace.clj 'from-nrepl)
              :to-equal /home/host/project/src/namespace.clj)
      (expect (cider--translate-path-from-nrepl-test `((,/docker/src . ,/home/host/project/src)) /docker/src/namespace.clj)
              :to-equal /home/host/project/src/namespace.clj))
  (it "returns nil if no prefixes match ('from-nrepl)"
      (expect (cider--translate-path-test `((,/docker/src . ,/home/host/project/src)) /home/host/random/file.clj 'from-nrepl)
              :to-be nil)
      (expect (cider--translate-path-from-nrepl-test `((,/docker/src . ,/home/host/project/src)) /home/host/random/file.clj)
              :to-be nil))
  (it "won't replace a prefix in the middle of the path ('from-nrepl)"
      (expect (cider--translate-path-test `((,/src . ,/host)) /src/project/src/ns.clj 'from-nrepl)
              :to-equal /host/project/src/ns.clj)
      (expect (cider--translate-path-from-nrepl-test `((,/src . ,/host)) /src/project/src/ns.clj)
              :to-equal /host/project/src/ns.clj))
  (it "handles slashes or no slashes in translations ('from-nrepl)"
      (expect (cider--translate-path-test `((,/src . ,/host/)) /src/project/src/ns.clj 'from-nrepl)
              :to-equal /host/project/src/ns.clj)
      (expect (cider--translate-path-test `((,/src/ . ,/host)) /src/project/src/ns.clj 'from-nrepl)
              :to-equal /host/project/src/ns.clj)
      (expect (cider--translate-path-from-nrepl-test `((,/src . ,/host/)) /src/project/src/ns.clj)
              :to-equal /host/project/src/ns.clj)
      (expect (cider--translate-path-from-nrepl-test `((,/src/ . ,/host)) /src/project/src/ns.clj)
              :to-equal /host/project/src/ns.clj))
  (it "expands the destination file paths"
      (expect (cider--translate-path-test `((,/src/ . "~/host")) /src/project/src/ns.clj 'from-nrepl)
              :to-equal (expand-file-name "~/host/project/src/ns.clj"))
      (expect (cider--translate-path-from-nrepl-test `((,/src/ . "~/host")) /src/project/src/ns.clj)
              :to-equal (expand-file-name "~/host/project/src/ns.clj")))
  (it "ensures the prefix has a slash ('from-nrepl)"
      (expect (cider--translate-path-test `((,/docker . ,/host)) /docker/ns.clj 'from-nrepl)
              :to-equal /host/ns.clj)
      (expect (cider--translate-path-from-nrepl-test `((,/docker . ,/host)) /docker/ns.clj)
              :to-equal /host/ns.clj))
  (it "translates file paths from host location to container/vm location"
      (expect (cider--translate-path-test `((,/docker/src . ,/home/host/project/src)) /home/host/project/src/namespace.clj 'to-nrepl)
              :to-equal /docker/src/namespace.clj)
      (expect (cider--translate-path-to-nrepl-test `((,/docker/src . ,/home/host/project/src)) /home/host/project/src/namespace.clj)
              :to-equal /docker/src/namespace.clj))
  (it "returns nil if no prefixes match ('to-nrepl)"
      (expect (cider--translate-path-test `((,/docker/src . ,/home/host/project/src)) /home/host/random/file.clj 'to-nrepl)
              :to-be nil)
      (expect (cider--translate-path-to-nrepl-test `((,/docker/src . ,/home/host/project/src)) /home/host/random/file.clj)
              :to-be nil))
  (it "won't replace a prefix in the middle of the path ('to-nrepl)"
      (expect (cider--translate-path-test `((,/src . ,/host)) /host/project/host/ns.clj 'to-nrepl)
              :to-equal /src/project/host/ns.clj)
      (expect (cider--translate-path-to-nrepl-test `((,/src . ,/host)) /host/project/host/ns.clj)
              :to-equal /src/project/host/ns.clj))
  (it "handles slashes or no slashes in translations ('to-nrepl)"
      (expect (cider--translate-path-test `((,/src . ,/host/)) /host/project/src/ns.clj 'to-nrepl)
              :to-equal  /src/project/src/ns.clj)
      (expect (cider--translate-path-test `((,/src/ . ,/host)) /host/project/src/ns.clj 'to-nrepl)
              :to-equal /src/project/src/ns.clj)
      (expect (cider--translate-path-to-nrepl-test `((,/src . ,/host/)) /host/project/src/ns.clj)
              :to-equal  /src/project/src/ns.clj)
      (expect (cider--translate-path-to-nrepl-test `((,/src/ . ,/host)) /host/project/src/ns.clj)
              :to-equal /src/project/src/ns.clj))
  (it "expands the source file paths"
      (expect (cider--translate-path-test `((,/src/ . "~/host")) "~/host/project/src/ns.clj" 'to-nrepl)
              :to-equal /src/project/src/ns.clj)
      (expect (cider--translate-path-to-nrepl-test `((,/src/ . "~/host")) "~/host/project/src/ns.clj")
              :to-equal /src/project/src/ns.clj))
  (it "ensures the prefix has a slash ('to-nrepl)"
      (expect (cider--translate-path-test `((,/docker . ,/host)) /host/ns.clj 'to-nrepl)
              :to-equal /docker/ns.clj)
      (expect (cider--translate-path-to-nrepl-test `((,/docker . ,/host)) /host/ns.clj)
              :to-equal /docker/ns.clj)))

(describe "cider--pin-repl-if-out-of-project"
  :var (proj-root repl-buf)

  (before-each
    (setq proj-root (file-name-as-directory
                     (file-truename (make-temp-file "cider-pin-test-" t)))
          repl-buf (get-buffer-create "*cider-pin-test-repl*"))
    (with-current-buffer repl-buf
      (setq-local nrepl-project-dir proj-root)))

  (after-each
    (when (buffer-live-p repl-buf)
      (kill-buffer repl-buf))
    (when (and proj-root (file-directory-p proj-root))
      (delete-directory proj-root t)))

  (it "pins the REPL when the buffer's file is outside its project dir"
      (with-temp-buffer
        (setq buffer-file-name (expand-file-name "/some/dep/src/ns.clj"))
        (cider--pin-repl-if-out-of-project repl-buf)
        (expect cider--pinned-repl-buffer :to-be repl-buf)))

  (it "leaves the buffer untouched when its file is inside the project dir"
      (with-temp-buffer
        (setq buffer-file-name (concat proj-root "src/ns.clj"))
        (cider--pin-repl-if-out-of-project repl-buf)
        (expect cider--pinned-repl-buffer :to-be nil)))

  (it "does nothing for a dead REPL buffer"
      (kill-buffer repl-buf)
      (with-temp-buffer
        (setq buffer-file-name (expand-file-name "/some/dep/src/ns.clj"))
        (cider--pin-repl-if-out-of-project repl-buf)
        (expect cider--pinned-repl-buffer :to-be nil)))

  (it "does nothing for a non-file buffer"
      (with-temp-buffer
        (cider--pin-repl-if-out-of-project repl-buf)
        (expect cider--pinned-repl-buffer :to-be nil))))

(describe "cider-auto-select-buffer-p"
  (it "follows cider-auto-select-buffer when the legacy option is `default'"
    (let ((cider-auto-select-buffer t))
      (expect (cider-auto-select-buffer-p 'error 'default) :to-be-truthy))
    (let ((cider-auto-select-buffer nil))
      (expect (cider-auto-select-buffer-p 'error 'default) :to-be nil)))

  (it "supports selecting only the features listed in cider-auto-select-buffer"
    (let ((cider-auto-select-buffer '(error inspector)))
      (expect (cider-auto-select-buffer-p 'error 'default) :to-be-truthy)
      (expect (cider-auto-select-buffer-p 'inspector 'default) :to-be-truthy)
      (expect (cider-auto-select-buffer-p 'test-report 'default) :to-be nil)))

  (it "gives precedence to a customized legacy option"
    (let ((cider-auto-select-buffer t))
      (expect (cider-auto-select-buffer-p 'error nil) :to-be nil))
    (let ((cider-auto-select-buffer nil))
      (expect (cider-auto-select-buffer-p 'error t) :to-be-truthy))))
