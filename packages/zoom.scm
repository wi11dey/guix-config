(define-module (packages zoom)
  #:use-module (guix utils)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages elf))

(define-public zoom
  (package
   (name "zoom")
   (version "5.9.1.1380")
   (source (origin
	    (method url-fetch)
	    (uri (string-append "https://cdn.zoom.us/prod/" version "/zoom_x86_64.tar.xz")) ; TODO: match (%current-system)
	    (sha256
	     (base32 "1z7msgqjhnhlw8gfzkxva4928zjrgq7djyak3ks3w7pdk3s377dr"))))
   (build-system gnu-build-system)
   (synopsis "Zoom Client for Linux")
   (description "The Zoom Client for Linux allows you to start or join Zoom meetings on Linux.")
   (home-page "https://zoom.us")
   (license #nil)
   (inputs `(("libX11" ,libx11)
	     ("libxfixes" ,libxfixes)
	     ("libxtst" ,libxtst)
	     ("libxrender" ,libxrender)
	     ("gcc:lib" ,gcc "lib")
	     ("xcb-util-image" ,xcb-util-image)
	     ("xcb-util-keysyms" ,xcb-util-keysyms)
	     ("libxkbcommon" ,libxkbcommon)
	     ("libglvnd" ,libglvnd)
	     ("wayland" ,wayland)
	     ("freetype" ,freetype)
	     ("fontconfig" ,fontconfig)
	     ("glib" ,glib)
	     ("dbus" ,dbus)))
   (native-inputs `(("patchelf" ,patchelf)))))

zoom
