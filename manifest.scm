(use-modules (gnu packages fonts)
	     (gnu packages fontutils)
	     (guix packages))

(concatenate-manifests
 (list
  (specifications->manifest
   '("aspell"
     "aspell-dict-en"
     "dbus"
     "dosfstools" ; Contains mkfs.fat.
     "emacs-auctex"
     ;; "emacs-djvu3"
     "emacs-next"
     "emacs-pdf-tools"
     "flatpak"
     "font-dejavu"
     "font-google-noto"
     "font-google-noto-sans-cjk"
     "git"
     "icecat" ; TODO remove
     "parted"
     "perl" ; Magit expects Perl to be installed for some operations.
     "scrot"
     "texlive"
     "unzip"))
  (packages->manifest
   (list
    (package
     (inherit font-latin-modern)
     (name (string-append (package-name font-latin-modern) "-ttf"))
     (native-inputs
      (list
       fontforge
       ttfautohint))
     (arguments
      (list
       #:phases
       #~(modify-phases %standard-phases
			(add-before 'install 'convert-to-ttf
				    (lambda _
				      (for-each (lambda (otf)
						  (invoke #+(file-append fontforge "/bin/fontforge")
							  "-lang=ff"
							  "-c" "Open($1); Generate($1:r + \"-unhinted.ttf\")"
							  otf)
						  (delete-file otf))
						(find-files "." "\\.otf$"))))
			(add-before 'install 'ttfautohint
				    (lambda _
				      (for-each (lambda (unhinted-ttf)
						  (invoke #+(file-append ttfautohint "/bin/ttfautohint")
							  unhinted-ttf
							  (string-append
							   (substring unhinted-ttf
								      0 (- (string-length unhinted-ttf)
									   (string-length "-unhinted.ttf")))
							   ".ttf"))
						  (delete-file unhinted-ttf))
						(find-files "." "\\-unhinted.ttf$"))))))))))))
