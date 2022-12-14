(use-modules (guix packages)
	     (guix utils)
	     (gnu packages julia))

(package
 (inherit julia)
 (arguments
  (substitute-keyword-arguments
   (package-arguments julia)
   ((#:phases phases)
    `(modify-phases
      ,phases
      ;; Additional packages expected but not correctly symlinked by installation nor `symlink-missing-libraries':
      (add-after 'symlink-missing-libraries 'symlink-missing-libraries-2
		 (lambda* (#:key inputs outputs #:allow-other-keys)
		   (let* ((out (assoc-ref outputs "out"))
			  (link
			   (lambda (pkgname pred)
                             (map (lambda (file)
				    (unless (file-exists?
                                             (string-append out "/lib/julia/"
                                                            (basename file)))
				      (symlink file (string-append out "/lib/julia/"
								   (basename file)))))
				  (find-files (string-append (assoc-ref inputs pkgname)
							     "/lib") pred)))))
		     (link "suitesparse" "libklu\\.so"))))))
   ((#:tests? _ #t) #f))))
