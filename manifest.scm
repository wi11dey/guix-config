(specifications->manifest
 '("aspell"
   "aspell-dict-en"
   "dosfstools" ; Contains mkfs.fat.
   "emacs-xwidgets"
   "emacs-auctex"
   ;; "emacs-djvu3"
   "emacs-pdf-tools"
   "font-dejavu"
   "font-google-noto"
   "git"
   "glib-networking" ; For emacs-widgets.
   "gsettings-desktop-schemas" ; For emacs-widgets.
   "icecat" ; TODO remove
   "parted"
   "perl" ; Magit expects Perl to be installed for some operations.
   "scrot"
   "texlive" ; TODO texlive-optex only
   "unzip"))
