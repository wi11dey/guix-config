(use-modules (gnu packages emacs)
             (gnu packages fonts)
             (gnu packages fontutils)
             (gnu packages tex)
             (guix packages)
             (guix utils))

(define (autohint otf-font)
  (package
    (inherit otf-font)
    (name (string-append (package-name otf-font) "-ttf"))
    (native-inputs
     (cons*
      fontforge
      ttfautohint
      (package-native-inputs otf-font)))
    (arguments
     (substitute-keyword-arguments (package-arguments otf-font)
       ((#:phases phases)
        #~(append #$phases
                  ;; Steps from https://tex.stackexchange.com/a/201798:
                  `((ttfautohint . ,(lambda _
                                      (let* ((fonts (string-append #$output "/share/fonts"))
                                             (source (string-append fonts "/opentype"))
                                             (destination (string-append fonts "/truetype")))
                                        (mkdir destination)
                                        (for-each (lambda (otf)
                                                    (let* ((base (string-append "/" (basename otf ".otf")))
                                                           (unhinted (string-append destination base "-unhinted.ttf"))
                                                           (ttf (string-append destination base ".ttf")))
                                                      (invoke #+(file-append fontforge "/bin/fontforge")
                                                              "-lang=ff"
                                                              "-c" "Open($1); Generate($2)"
                                                              otf unhinted)
                                                      (delete-file otf)
                                                      (invoke #+(file-append ttfautohint "/bin/ttfautohint")
                                                              unhinted ttf)
                                                      (delete-file unhinted)))
                                                  (find-files source "\\.otf$"))
                                        (rmdir source)))))))))))

(concatenate-manifests
 (list
  (specifications->manifest
   '("aspell"
     "aspell-dict-en"
     "dbus"
     "dosfstools" ; Contains mkfs.fat.
     "emacs-auctex"
     ;; "emacs-djvu3"
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
     "texlive-optex"
     "unzip"))
  (packages->manifest
   (list
    (emacs->emacs-next emacs-xwidgets)
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
            ;; Steps from https://tex.stackexchange.com/a/201798:
            (add-before 'install 'ttfautohint
              (lambda _
                (for-each (lambda (otf)
                            (let* ((base (string-drop-right otf (string-length ".otf")))
                                   (unhinted (string-append base "-unhinted.ttf"))
                                   (ttf (string-append base ".ttf")))
                              (invoke #+(file-append fontforge "/bin/fontforge")
                                      "-lang=ff"
                                      "-c" "Open($1); Generate($2)"
                                      otf unhinted)
                              (delete-file otf)
                              (invoke #+(file-append ttfautohint "/bin/ttfautohint")
                                      unhinted ttf)
                              (delete-file unhinted)))
                          (find-files "." "\\.otf$"))))))))))))
