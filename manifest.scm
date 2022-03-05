(add-to-load-path ".")
(use-modules (packages emacs-local)
	     (gnu package))

(concatenate-manifests
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
    "parted"
    "perl" ; Magit expects Perl to be installed for some operations.
    "scrot"
    "texlive" ; TODO texlive-optex only
    "unzip"))
 (packages->manifest
  (list (if (access? (local-file-absolute-file-name (package-source emacs-local)) R_OK)
	    emacs-local
	    emacs-next))))
