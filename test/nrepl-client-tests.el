(require 'buttercup)
(require 'cider)

(describe "nrepl-connection-buffer-name"
  :var (nrepl-hide-special-buffers nrepl-endpoint)
  (before-all (setq-local nrepl-endpoint '("localhost" 1)))

  (describe "when nrepl-hide-special-buffers is nil"
    (it "returns the name of the connection buffer, which would make it visible in buffer changing commands"
      (expect (nrepl-connection-buffer-name)
              :to-equal "*nrepl-connection localhost*")))

  (describe "when nrepl-hide-special-buffers is t"
    (it "returns the name of the connection buffer, which hides it in buffer changing commands"
      (setq nrepl-hide-special-buffers t)
      (expect (nrepl-connection-buffer-name)
              :to-equal " *nrepl-connection localhost*"))))

(describe "nrepl-server-buffer-name"
  :var (nrepl-hide-special-buffers nrepl-endpoint)
  (before-all (setq-local nrepl-endpoint '("localhost" 1)))

  (describe "when nrepl-hide-special-buffers is nil"
    (it "returns the name of the server buffer, which would make it visible in buffer changing commands"
      (setq nrepl-hide-special-buffers nil)
      (expect (nrepl-server-buffer-name)
              :to-equal "*nrepl-server localhost*")))

  (describe "when nrepl-hide-special-buffers is t"
    (it "returns the name of the server buffer, which hides it in buffer changing commands"
      (setq nrepl-hide-special-buffers t)
      (expect (nrepl-server-buffer-name)
              :to-equal " *nrepl-server localhost*"))))
