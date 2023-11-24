(define-module (packages chromium)
  #:use-module (gnu packages base)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages chromium)
  #:use-module (gnu packages python)
  #:use-module (guix gexp)
  #:use-module (guix packages))

;; ungoogled-chromium-snippet without the ungooglifying
(define chromium-snippet
  #~(let ((chromium-dir (getcwd)))
      (set-path-environment-variable
       "PATH" '("bin")
       (list #+patch #+python-wrapper #+xz))

      ;; Apply patches before running the ungoogled scripts because
      ;; domain substitution may break some of the patches.
      (format #t "Applying assorted build fixes...~%")
      (force-output)
      (for-each (lambda (patch)
                  (invoke "patch" "-p1" "--force" "--input"
                          patch "--no-backup-if-mismatch"))
                '#+(@@ (gnu packages chromium) %patches))

      ;; These patches are "reversed", i.e. their changes should be undone.
      (for-each (lambda (patch)
                  (invoke "patch" "-Rp1" "-F3" "--force" "--input"
                          patch "--no-backup-if-mismatch"))
                '#+(@@ (gnu packages chromium) %reverse-patches))

      (format #t "Removing blacklisted files...~%")
      (force-output)
      (for-each delete-file-recursively '#$(@@ (gnu packages chromium) %blacklisted-files))

      (format #t "Pruning third party files...~%")
      (force-output)
      (apply invoke "python"
             "build/linux/unbundle/remove_bundled_libraries.py"
             "--do-remove" '#$(@@ (gnu packages chromium) %preserved-third-party-files))

      (format #t "Replacing GN files...~%")
      (force-output)
      (substitute* "tools/generate_shim_headers/generate_shim_headers.py"
        ;; The "is_official_build" configure option enables certain
        ;; release optimizations like those used in the commercial
        ;; Chrome browser.  Unfortunately it also requires using the
        ;; bundled libraries: lose that restriction.
        (("#if defined\\(OFFICIAL_BUILD\\)")
         "#if 0"))
      (invoke "python" "build/linux/unbundle/replace_gn_files.py"
              "--system-libraries" "ffmpeg" "flac" "fontconfig" "freetype"
              "harfbuzz-ng" "icu" "jsoncpp" "libdrm" "libevent" "libjpeg"
              "libpng" "libwebp" "libxml" "libxslt" "openh264" "opus" "re2"
              "zlib")))

(define-public chromium
  (package
    (inherit ungoogled-chromium)
    (name "chromium")
    (version (@@ (gnu packages chromium) %chromium-version))
    (source (origin
	      (inherit (package-source ungoogled-chromium))
	      (snippet chromium-snippet)))
    (home-page "https://www.chromium.org/Home/")
    (description
     "Regoogled Chromium")))

chromium
