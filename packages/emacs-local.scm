(define-module (packages emacs-local)
  #:use-module (gnu packages base)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages xorg)
  #:use-module (guix packages)
  #:use-module (guix gexp)  
  #:use-module (guix git)
  #:use-module (guix utils))

(define-public emacs-local
  (package
   (inherit emacs-next-pgtk)
   (name "emacs-local")
   (version "latest")
   (source (local-file (string-append (getenv "HOME") "/.emacs.d/src") "emacs-local-checkout"
		       #:recursive? #t))
   (arguments
    (substitute-keyword-arguments
     (package-arguments emacs-next-pgtk)
     ((#:make-flags flags ''())
      `(cons* "NATIVE_FULL_AOT=1" ,flags))
     ((#:configure-flags flags ''())
      `(cons* "--with-native-compilation" ,flags))
     ((#:phases phases)
      `(modify-phases
	,phases
	;; Add build-time library paths for libgccjit:
	(add-before 'configure 'set-libgccjit-path
		    (lambda* (#:key inputs #:allow-other-keys)
			     (let ((libgccjit-libdir (string-append (assoc-ref inputs "libgccjit")
								    "/lib/gcc/" %host-type "/"
								    ,(package-version gcc-11) "/")))
			       (setenv "LIBRARY_PATH" (string-append libgccjit-libdir ":" (getenv "LIBRARY_PATH"))))))
	;; Add runtime library paths for libgccjit:
	(add-after 'unpack 'patch-driver-options
		   (lambda* (#:key inputs #:allow-other-keys)
			    (substitute* "lisp/emacs-lisp/comp.el"
					 (("\\(defcustom native-comp-driver-options nil")
					  (format #f "(defcustom native-comp-driver-options '(~s ~s ~s ~s)"
						  (string-append
						   "-B" (assoc-ref inputs "binutils") "/bin/")
						  (string-append
						   "-B" (assoc-ref inputs "glibc") "/lib/")
						  (string-append
						   "-B" (assoc-ref inputs "libgccjit") "/lib/")
						  (string-append
						   "-B" (assoc-ref inputs "libgccjit") "/lib/gcc/"))))))))
     ;; Keep debug symbols:
     ((#:strip-binaries? _ #f) #f)))
   (native-inputs (modify-inputs (package-native-inputs emacs-next-pgtk)
				 (prepend gcc-11)))
   (inputs (modify-inputs (package-inputs emacs-next-pgtk)
			  (prepend glibc)
			  (prepend (package
				    (inherit libgccjit)
				    (version (package-version gcc-11))
				    (source  (package-source  gcc-11))
				    (arguments
				     (substitute-keyword-arguments
				      (package-arguments libgccjit)
				      ((#:configure-flags flags)
				       `(cons* "--disable-bootstrap"
					       "--disable-libatomic"
					       "--disable-libgomp"
					       "--disable-libquadmath"
					       "--disable-libssp"
					       "--enable-checking=release"
					       ,flags))))
				    (inputs (modify-inputs (package-inputs libgccjit)
							   (delete "libstdc++")))
				    (native-inputs (modify-inputs (package-native-inputs libgccjit)
								  (prepend gcc-11)))))
			  (prepend libxcomposite)))))

emacs-local
