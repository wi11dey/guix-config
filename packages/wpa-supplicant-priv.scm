(define-module (packages wpa-spplicant-priv)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (gnu packages admin))

(define-public wpa-supplicant-priv
  (package
   (inherit wpa-supplicant-minimal)
   (name "wpa-supplicant-priv")
   (arguments
    (substitute-keyword-arguments
     (package-arguments wpa-supplicant-minimal)
     ((#:make-flags flags)
      '(list "CC=gcc"
	     (string-append "BINDIR=" (assoc-ref %outputs "out") "/bin")
	     (string-append "LIBDIR=" (assoc-ref %outputs "out") "/lib")))
     ((#:phases phases)
      `(modify-phases
	,phases
	(add-after 'configure 'configure-for-privsep
		   (lambda _
		     (let ((port (open-file ".config" "al")))
		       (display "
CONFIG_PRIVSEP=y\n" port)
		       (close-port port))
		     #t))
	(add-after 'install 'install-wpa-priv
		   (lambda* (#:key outputs #:allow-other-keys)
			    (install-file "wpa_priv"
					  (string-append (assoc-ref outputs "out")
							 "/sbin"))
			    #t))))))))
