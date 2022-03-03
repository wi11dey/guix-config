(define-module (packages udevil)
  #:use-module (guix utils)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages acl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages pkg-config)
  #:use-module (guix base16))

(define-public udevil
  (package
   (name "udevil")
   (version "0.4.4")
   (source (origin
	    (method url-fetch)
	    (uri (string-append "https://raw.githubusercontent.com/IgnorantGuru/udevil"
				"/pkg/" version "/udevil-" version ".tar.xz"))
	    (sha256 (base16-string->bytevector "ce8c51fd4d589cda7be56e75b42188deeb258c66fc911a9b3a70a3945c157739"))))
   (build-system gnu-build-system)
   (native-inputs
    `(("pkg-config" ,pkg-config)
      ("intltool" ,intltool)))
   (inputs
    `(("glib" ,glib)
      ("eudev" ,eudev)
      ("util-linux" ,util-linux)
      ("acl" ,acl)))
   (arguments
    '(#:configure-flags (list (string-append "--sysconfdir="
					     (assoc-ref %outputs "out")
					     "/etc")
			      (string-append "--with-mount-prog="
					     (assoc-ref %build-inputs "util-linux")
					     "/bin/mount")
			      (string-append "--with-umount-prog="
					     (assoc-ref %build-inputs "util-linux")
					     "/bin/umount")
			      (string-append "--with-losetup-prog="
					     (assoc-ref %build-inputs "util-linux")
					     "/sbin/losetup")
			      (string-append "--with-setfacl-prog="
					     (assoc-ref %build-inputs "acl")
					     "/bin/setfacl"))
      #:make-flags '("CFLAGS=-DSYSCONFDIR='\"/etc\"'")
      #:phases
      (modify-phases
       %standard-phases
       (add-before 'configure 'defer-setuid
		   (lambda _
		     ;; This hook tries to make the udevil executable setuid-root which has to be done by a service.
		     (substitute* '("src/Makefile.in")
				  (("^install-data-hook:")
				   "install-data-hook-disabled-for-guix:"))
		     #t))
       ;; Installation of the executable never happened because it was in noinst_PROGRAMS and was to be installed by the now-disabled install-data-hook:
       (add-after 'install 'install-udevil
		  (lambda* (#:key outputs #:allow-other-keys)
			   (install-file "src/udevil" (string-append (assoc-ref outputs "out") "/bin"))
			   #t)))))
   (synopsis "Mount without password")
   (description "udevil is a command line Linux program which mounts and unmounts removable devices without a password, shows device info, and monitors device changes. It can also mount ISO files, nfs://, smb://, ftp://, ssh:// and WebDAV URLs, and tmpfs/ramfs filesystems.")
   (home-page "https://ignorantguru.github.io/udevil")
   (license license:gpl3+)))
