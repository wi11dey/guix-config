(define-module (packages securew2)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (ice-9 readline))

(define (prompt-for-run-file)
  (activate-readline)
  (with-readline-completion-function filename-completion-function
				     (lambda _ (readline "Path to SecureW2_JoinNow.run file: "))))

(define-public securew2-joinnow
  (package
   (name "securew2-joinnow")
   (version "1.2.1")
   (source (local-file (or (getenv "GUIX_SECUREW2_JOINNOW_RUN")
			   (prompt-for-run-file))))
   (inputs (list xdg-utils
		 xwininfo
		 openssl
		 python
		 python-dbus))
   (synopsis "Proprietary tool to onboard users onto wireless networks.")
   (description "Proprietary tool to onboard users onto wireless networks. At some organizations, is the only way to get connected to the internet.")
   (license #f)))
