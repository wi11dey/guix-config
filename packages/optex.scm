(use-modules (gnu packages tex)
	     (guix packages)
	     (guix git-download)
	     (guix build-system texlive)
	     (guix licenses))

(define-public optex
  (package
   (name "optex")
   (version "1.05")
   (source (origin
	    (method git-fetch)
	    (uri (git-reference
		  (url "https://github.com/olsak/OpTeX.git")
		  (commit (string-append "v" version))))
	    (file-name (git-file-name name version))
	    (sha256 "1grfligh8r6sxv7aas12c6f40kpwa1mj41wglfjca1lnmcbpnfml")))
   (build-system texlive-build-system)
   (outputs '("out" "doc"))
   (propagated-inputs (list texlive-bin texlive-kpathsea))
   (native-inputs (list texlive-lm))
   (license public-domain)
   (home-page "https://petr.olsak.net/optex/")
   (synopsis "OpTeX is a LuaTeX format based on plain TeX + OPmac macros.")
   (description "OpTeX is a modern plain TeX with power from OPmac (fonts selection system, colors, external graphics, references, hyperlinks...) with Unicode fonts. ")
   (arguments '(#:tex-engine "luatex"
		#:tex-format #f
		#:build-targets '("optex.ini")
		#:tex-directory "../web2c"
		#:phases (modify-phases %standard-phases
					(add-after 'unpack 'chdir
						   (lambda _
						     (chdir "optex/base") #t))
					(add-after 'install 'copy-files
						   (lambda* (#:key outputs inputs #:allow-other-keys)
						     (let ((src (string-append (assoc-ref inputs "source")
									       "/optex/"))
							   (doc (string-append (assoc-ref outputs "doc")
									       "/share/texmf-dist/doc/optex/"))
							   (out (string-append (assoc-ref outputs "out")
									       "/share/texmf-dist/tex/optex/")))
						       (mkdir-p doc)
						       (copy-recursively (string-append src "doc/")
									 doc)
						       (mkdir-p out)
						       (copy-recursively src out)
						       (delete-file-recursively (string-append out "/doc"))
						       #t))))))))

optex
