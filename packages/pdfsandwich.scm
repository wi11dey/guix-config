(define-module (packages pdfsandwich)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages check)
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages imagemagick)
  #:use-module (gnu packages ocr)
  #:use-module (gnu packages pdf)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages sphinx)
  #:use-module (gnu packages video)
  #:use-module (guix build-system meson)
  #:use-module (guix build-system ocaml)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix svn-download)
  #:use-module (guix packages))

(define-public unpaper
  (package
    (name "unpaper")
    (version "7.0.0")
    (source (origin
	      (method git-fetch)
	      (uri (git-reference
		    (url "https://github.com/unpaper/unpaper")
		    (commit (string-append "unpaper-" version))))
	      (sha256
               (base32 "1a9cqiq458si4mv56gpm4rw4qb50f1psszf9m8007m3lrn95f4wa"))))
    (build-system meson-build-system)
    (home-page "https://www.flameeyes.eu/projects/unpaper")
    (synopsis "A post-processing tool for scanned sheets of paper. ")
    (description "unpaper is a post-processing tool for scanned sheets of paper, especially for book pages that have been scanned from previously created photocopies. The main purpose is to make scanned book pages better readable on screen after conversion to PDF. Additionally, unpaper might be useful to enhance the quality of scanned pages before performing optical character recognition (OCR). ")
    (license (list license:gpl2+
		   license:expat
		   license:asl2.0))
    (native-inputs (list pkg-config
			 python-sphinx
			 python-pillow
			 python-pytest))
    (inputs (list ffmpeg-5))))

(define-public pdfsandwich
  (package
    (name "pdfsandwich")
    (version "0.1.7")
    (source (origin
	      (method svn-fetch)
	      (uri (svn-reference
		    (url "svn://svn.code.sf.net/p/pdfsandwich/code/trunk/src")
		    (revision 75)))
	      (sha256
	       (base32 "1420c33divch087xrr61lvyf975bapqkgjqaighl581i69nlzsm6"))))
    (build-system ocaml-build-system)
    (home-page "http://www.tobias-elze.de/pdfsandwich/")
    (synopsis "A tool to make \"sandwich\" OCR pdf files ")
    (description "pdfsandwich generates \"sandwich\" OCR pdf files, i.e. pdf files which contain only images (no text) will be processed by optical character recognition (OCR) and the text will be added to each page invisibly \"behind\" the images. ")
    (license license:gpl2+)
    (native-inputs (list perl))
    (inputs (list ghostscript
		  imagemagick
		  poppler
		  tesseract-ocr
		  unpaper))
    (arguments
     (list
      #:tests? #f ; No tests included.
      #:configure-flags #~(list (string-append "--prefix=" #$output))
      #:modules '((guix build ocaml-build-system)
                  (guix build utils)
		  (srfi srfi-26))
      #:phases
      #~(modify-phases %standard-phases
	  (add-after 'install 'wrap-program
	    (lambda _
	      (wrap-program (string-append #$output "/bin/pdfsandwich")
		`("PATH" ":" prefix
		  ,(map (compose (cut string-append <> "/bin")
				 cadr)
			'#$(package-inputs this-package)))))))))))

pdfsandwich
