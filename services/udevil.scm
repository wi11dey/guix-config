(define-module (services udevil)
  #:use-module (packages udevil)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (gnu services)
  #:use-module (ice-9 match)
  #:export (udevil-configuration
	    udevil-configuration?
	    udevil-configuration-package
	    udevil-configuration-file

	    udevil-service-type))

(define-record-type* <udevil-configuration> udevil-configuration make-udevil-configuration
  udevil-configuration?
  (package udevil-configuration-package
	   (default udevil))
  (config-file udevil-configuration-file
	       (default #f)))

(define (udevil-etc-files config)
  (define config-file
    (or (udevil-configuration-file config)
	(file-append (udevil-configuration-package config)
		     "/etc/udevil/udevil.conf")))

  (define config-directory
    (file-union "udevil-configuration"
		`(("udevil.conf" ,config-file))))
  
  `(("udevil" ,config-directory)))

(define udevil-setuid-programs
  (match-lambda
   (($ <udevil-configuration> package config-file)
    (list (file-append package "/bin/udevil")))))

(define udevil-service-type
  (service-type
   (name 'udevil)
   (extensions (list (service-extension etc-service-type
					udevil-etc-files)
		     (service-extension setuid-program-service-type
					udevil-setuid-programs)))
   (default-value (udevil-configuration))))
