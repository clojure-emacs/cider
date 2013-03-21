;; klang - suggestion
(defun nrepl-jump-to-def-for (location)
  ;; ugh; elisp destructuring doesn't work for vectors
  ;;
  (let* ((resource (aref location 0))
        (path (aref location 1))
        (line (aref location 2))
        (user (aref location 3))
        (host (aref location 4))
	(remote (concat tramp-default-method ":" user "@" host ":" resource))
	)
    ;; NOTE: resource might contain jar:file: in addition to the path
    ;; /ssh:user@host:/home/user
    ;; /ssh:user@host:/Users/user
    (message (concat "nrepl-jump-to-def-for: " user ":" host ":" resource))
    (message (concat "nrepl-jump-to-def-for: " remote))
    (cond ((and path (file-exists-p path)) (find-file path))
	  ;; nrepl-find-resource
	  ((string-match "^\\(jar\\|zip\\):file:\\(.+\\)!/\\(.+\\)" resource)
	   (let* ((jar (match-string 2 resource))
		  (path (match-string 3 resource))
		  (buffer-already-open (get-buffer (file-name-nondirectory jar))))
	     (nrepl-find-file (nrepl-maybe-local-m2-resource jar))
	     (goto-char (point-min))
	     (search-forward path)
	     (let ((opened-buffer (current-buffer)))
	       (archive-extract)
	       (when (not buffer-already-open)
               (kill-buffer opened-buffer-buffer)))))
	  ;;((file-remote-p (concat "/ssh:" user "@" host ":" path)) ())
	  (t (nrepl-find-resource resource))
	  )

;; local file exists --> use that
;; otherwise .. find-file on remote
    (if (and path (file-exists-p path))
        (find-file path)
      (nrepl-find-resource resource))

    (goto-char (point-min))
    (forward-line (1- line))
    (search-forward-regexp "(def[^\s]* +" nil t)))

;; klang - suggestion
;; modification returning the user and ip of the host running clojure
;; that information will be used to look up the resources containing 
;; the definition of the var at the point.
(defun nrepl-jump-to-def (var)
  "Jump to the definition of the var at point."
  (let ((form (format "(clojure.core/vec 
                        (clojure.core/concat
                         ((clojure.core/juxt
                           (clojure.core/comp clojure.core/str clojure.java.io/resource :file)
                           (clojure.core/comp clojure.core/str clojure.java.io/file :file) :line)
                          (clojure.core/meta (clojure.core/ns-resolve '%s '%s)))
                         [(System/getProperty \"user.name\")
                          (.getHostAddress (java.net.InetAddress/getLocalHost))]))"
                      (nrepl-current-ns) var)))
    (nrepl-send-string form
                       (nrepl-jump-to-def-handler (current-buffer))
                       nrepl-buffer-ns
                       (nrepl-current-tooling-session))))

