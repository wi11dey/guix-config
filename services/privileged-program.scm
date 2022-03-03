(define-module (services privileged-program)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (gnu services)
  #:use-module (gnu system)
  #:use-module (gnu packages linux)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match)
  #:export (privileged-program
	    privileged-program?
	    privileged-program-file
	    privileged-program-name
	    privileged-program-owner
	    privileged-program-group
	    privileged-program-mode
	    privileged-program-capabilities
	    
	    privileged-program-service-type))

(define %privileged-directory "/run/setuid-programs")

(define-record-type* <privileged-program> privileged-program make-privileged-program
  privileged-program?
  (file privileged-program-file)
  (name privileged-program-name
	(default #f))
  (owner privileged-program-owner
	 (default 0))
  (group privileged-program-group
	 (default 0))
  (mode privileged-program-mode
    (default #o6555))
  (capabilities privileged-program-capabilities
		(default #f)))

(define (activate-privileged-programs programs)
  (define make-privileged-program
    (match-lambda
     (($ <privileged-program> file name owner group mode capabilities)
      #~(let ((target (string-append #$%privileged-directory
				     "/" #$@(if name
						#~(name)
						#~((basename #$file))))))
	  
	  (copy-file #$file target)
	  (chown target #$owner #$group)
	  (chmod target #$mode)
	  #$@(if capabilities
		 #~((system* #$(file-append libcap
					    "/sbin/setcap")
			     #$capabilities
			     target))
		 #~())))
     (file
      (make-privileged-program (privileged-program
				(file file))))))

  (define clean
    #~(begin
	(use-modules (srfi srfi-26)
		     (ice-9 ftw))
	(if (file-exists? #$%privileged-directory)
	    (for-each (compose delete-file
			       (cut string-append #$%privileged-directory "/" <>))
		      (scandir #$%privileged-directory
			       (lambda (file)
				 (not (member file '("." ".."))))
			       string<?))
	    (mkdir-p #$%privileged-directory))))

  #~(begin
      (format #t "setting up privileged programs in '~a'...~%"
	      #$%privileged-directory)
      #$clean
      #$@(map make-privileged-program programs)))

(define privileged-program-service-type
  (service-type
   (name 'privileged-program)
   (extensions (list (service-extension activation-service-type
					(lambda (programs)
					  (activate-privileged-programs programs)))))
   (compose concatenate)
   (extend append)
   (default-value %setuid-programs)
   (description "Takes a list of the form

  ((PROGRAM-FILE-GEXP #:name PROGRAM-NAME #:owner UID #:group GID #:mode MODE #:capabilities CAPABILITIES-STRING)
   ...)

so it can copy the program specified by PROGRAM-FILE-GEXP to /run/setuid-programs/PROGRAM-NAME and apply the attributes specified by keyword arguments during system activation.

PROGRAM-FILE-GEXP is a gexp that must evaluate to the path to the executable to copy at activation time.

PROGRAM-NAME is the name to give the executable once copied to /run/setuid-programs. It defaults to the value of (basename PROGRAM-FILE-GEXP) at activation time.

UID and GID can be gexps, but they must ultimately evaluate to valid UID and GID numbers at runtime. They both default to 0 (root).

MODE must be a decimal mode number. It defaults to #o6555 (setuid and setgid bits set).

CAPABILITIES-STRING must be a capabilities string as described in the man page cap_from_text(3). If it is specified, the resulting activation gexp will depend on the libcap package from (gnu packages linux).")))
