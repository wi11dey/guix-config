(use-modules (gnu packages emacs))

(concatenate-manifests
 (list
  (specifications->manifest
   '("aspell"
     "aspell-dict-en"
     "dosfstools" ; Contains mkfs.fat.
     "emacs-auctex"
     ;; "emacs-djvu3"
     "emacs-pdf-tools"
     "font-dejavu"
     "font-google-noto"
     "git"
     "icecat" ; TODO remove
     "nyxt"
     "parted"
     "perl" ; Magit expects Perl to be installed for some operations.
     "scrot"
     "texlive" ; TODO texlive-optex only
     "unzip"))
  (packages->manifest
   (list (let ((custom-emacs-package (string-append (getenv "HOME") "/.emacs.d/emacs.scm")))
	   (if (access? custom-emacs-package R_OK)
	       (load custom-emacs-package)
	       emacs-next))))))
