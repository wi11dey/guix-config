;; TODO contribute upstream

(define-module (services startx)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (gnu packages xorg)
  #:use-module (gnu services)
  #:use-module (gnu services xorg)
  #:export (xinit-etc
	    startx-service-type))

(define xinit-etc
  (package
   (inherit xinit)
   (name "xinit-etc")
   (arguments
    `(#:configure-flags '("--with-xinitdir=/etc/X11/xinit")
      #:make-flags (list (string-append "xinitrcdir=" (assoc-ref %outputs "out") "/etc/X11/xinit"))))))

(define* (xinitdir #:optional config)
  (computed-file "xinitdir"
		 (with-imported-modules
		  '((guix build utils))
		  #~(begin
		      (use-modules (guix build utils))

		      (let ((subdir (string-append #$output "/xinit")))
			(mkdir-p subdir)
			(chdir subdir)
			(symlink #$(xorg-start-command config)
				 "xserverrc"))))))

(define startx-service-type
  (service-type
   (name 'startx)
   (description "startx command")
   (extensions (list (service-extension profile-service-type
					(lambda (config)
					  (list xinit-etc
						(xorg-configuration-server config))))
		     (service-extension etc-service-type
					(lambda (config)
					  `(("X11" ,(xinitdir config)))))))
   (default-value (xorg-configuration))))
