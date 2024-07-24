(use-modules
 (guix git)
 (guix packages)
 (guix packages)
 (guix utils)
 (gnu packages emacs)
 (gnu packages fonts)
 (gnu packages fontutils)
 (gnu packages gnome)
 (gnu packages gnome)
 (gnu packages gtk)
 (gnu packages gtk)
 (gnu packages tex))

(define gtk+-linuxfb
  (package/inherit gtk+
    (source (git-checkout
	     (url "https://github.com/wi11dey/gtk.git")
	     (branch "gtk3-linuxfb")
	     (recursive? #t)))
    (name "gtk+-linuxfb")
    (version "3.24.44")
    (description (string-append (package-description gtk+) "

This fork adds a GDK Linux framebuffer backend."))
    (native-inputs (cons*
		    vala
		    (package-native-inputs gtk+)))))

(define gtk+->gtk+-linuxfb
  (package-input-rewriting
   `((,gtk+ ,gtk+-linuxfb))
   (cut string-append <> "-linuxfb")))

(gtk+->gtk+-linuxfb emacs-next-pgtk-xwidgets)

(define (autohint otf-font)
  (package
    (inherit otf-font)
    (name (string-append (package-name otf-font) "-ttf"))
    (native-inputs
     (cons* fontforge
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
    (autohint font-latin-modern)
    (autohint
     (package
       (inherit texlive-newcomputermodern)
       (name "font-newcomputermodern")
       (arguments
        (list
         #:phases
         #~(modify-phases %standard-phases
             (add-after 'link-scripts 'move-fonts
               (lambda _
                 (copy-recursively
                  (string-append #$output:out "/share/texmf-dist/fonts/opentype/public/newcomputermodern")
                  (string-append #$output:out "/share/fonts/opentype"))
                 (delete-file-recursively
                  (string-append #$output:out "/share/texmf-dist"))))
             (add-after 'move-fonts 'remove-regular-weight
               (lambda _
                 (for-each delete-file
                           (find-files #$output:out "NewCM(Math|(|Sans|Uncial)[0-9]{2})(-Regular|-Italic)")))))))))))))
