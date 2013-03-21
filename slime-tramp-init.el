(add-to-list 'load-path "~/projects/nrepl.el/")
(require 'slime-tramp)

(push (list "^klang$"
	    (lambda (emacs-filename)
	      (subseq emacs-filename (length "/ssh:192.168.1.6@klang:")))
	    (lambda (lisp-filename)
	      (concat "/ssh:192.168.1.6@klang:" lisp-filename)))
      slime-filename-translations)

