(define-module (gnu packages julia)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix utils)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (guix gexp)
  #:use-module (gnu packages ruby)
  #:use-module (gnu packages)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages base)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages libevent)
  #:use-module (gnu packages libunwind)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages multiprecision) ; mpfr
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages textutils)
  #:use-module (gnu packages ssh)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages web)
  #:use-module (gnu packages wget)
  #:use-module (ice-9 match))

(define libunwind-julia
  ;; The Julia projects requires their patched version.
  ;; Get from https://github.com/JuliaLang/julia/tree/master/deps/patches
  (package
   (inherit libunwind)
   (name "libunwind-julia")
   (version "1.5.0")
   (source
    (origin
     (method url-fetch)
     (uri (string-append "mirror://savannah/libunwind/libunwind-"
			 version ".tar.gz"))
     (sha256
      (base32
       "05qhzcg1xag3l5m3c805np6k342gc0f3g087b7g16jidv59pccwh"))
     (patches
      (list
       (julia-patch "libunwind-static-arm"
		    "1jk3bmiw61ypcchqkk1fyg5wh8wpggk574wxyfyaic870zh3lhgq")
       (julia-patch "libunwind-prefer-extbl"
		    "0pf3lsq6zxlmqn86lk4fcj1xwdan9gbxyabrwgxcb59p8jjwsl8r")
       (julia-patch "libunwind-dwarf-table"
		    "1vzhzmqdhqdjqvib8f3k5jr5h47src31g77dgijnlf1bxynrjn3h")
       (julia-patch "libunwind-cfa-rsp"
		    "0qs5b1h5lsr5qakkv6sddgy5ghlxpjrn2jiqcvg7bkczy24klr6j")))))
   (arguments
    (substitute-keyword-arguments (package-arguments libunwind)
				  ;; Skip tests on this older and patched version of libunwind.
				  ((#:tests? _ #t) #f)))
   (home-page "https://github.com/JuliaLang/tree/master/deps/")))

(define (julia-patch-url version name)
  (string-append "https://raw.githubusercontent.com/JuliaLang/julia/v" version
                 "/deps/patches/" name ".patch"))

(define-public (julia-patch name sha)
  (let ((version "1.8.2"))
    (origin (method url-fetch)
            (uri (julia-patch-url version name))
            (sha256 (base32 sha))
            (file-name name))))

(define-public libwhich
  (package
   (name "libwhich")
   (version "1.1.0")
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (url "https://github.com/vtjnash/libwhich")
           ;; fixes linux-vdso.so related tests
           (commit "87cffe10080c98e7b5786c5166e420bf1ada1d41")))
     (file-name (string-append name "-" version "-checkout"))
     (sha256
      (base32
       "1bpa0fcqpa3ai3hm8mz0p13bf76fsq53wsfcx5qw302zh22108xr"))))
   (arguments
    `(#:make-flags
      (list (string-append "CC=" ,(cc-for-target)))
      #:phases
      (modify-phases %standard-phases
		     (delete 'configure)
		     (add-before 'check 'set-ld-library-path
				 (lambda* (#:key native-inputs inputs #:allow-other-keys)
				   (setenv "LD_LIBRARY_PATH"
					   (string-append (assoc-ref (or native-inputs inputs) "zlib")
							  "/lib"))))
		     (replace 'install
			      (lambda* (#:key outputs #:allow-other-keys)
				(let ((out (assoc-ref outputs "out")))
				  (install-file "libwhich" (string-append out "/bin")))
				#t)))))
   (native-inputs
    ;; used for tests
    (list zlib))
   (build-system gnu-build-system)
   (home-page "https://github.com/vtjnash/libwhich")
   (synopsis "Like @code{which}, for dynamic libraries")
   (description "@code{libwhich} is like @code{which}, but for dynamic
libraries.  It is also a bit like @code{ldd} and @code{otool -L}.")
   (license license:expat)))

(define-public libblastrampoline
  (package
   (name "libblastrampoline")
   (version "5.2.0")
   (source
    (origin
     (method git-fetch)
     (uri
      (git-reference
       (url "https://github.com/JuliaLinearAlgebra/libblastrampoline")
       (commit
	(string-append "v" version))))
     (sha256
      (base32
       "1ndgwrfbzqiycwx7br34yxs84vfnp8j5l3vackhqbrz7sn2pzpjw"))))
   (arguments
    (list #:make-flags
	  #~(list (string-append "CC=" #$(cc-for-target))
		  (string-append "prefix=" #$output))
	  #:tests? #f ; Tests require Julia 1.7+, which would be a circular dependency.
	  #:phases
	  '(modify-phases %standard-phases
			  (delete 'configure)
			  (add-after 'unpack 'enter-subdirectory
				     (lambda _ (chdir "src"))))))
   (build-system gnu-build-system)
   (home-page "https://github.com/JuliaLinearAlgebra/libblastrampoline")
   (synopsis "Using PLT trampolines to provide a BLAS and LAPACK demuxing library.")
   (description "Using PLT trampolines to provide a BLAS and LAPACK demuxing library.
These BLAS libraries are known to work with libblastrampoline (successfully tested in Julia):
1. OpenBLAS
2. Intel MKL
3. Apple Accelerate
4. BLIS
5. Fujitsu BLAS")
   (license license:expat)))

(define-public libuv-julia
  (let ((commit "e6f0e4900e195c8352f821abe2b3cffc3089547b")
        (revision "4"))
    ;; When upgrading Julia, also upgrade this.  Get the commit from
    ;; https://github.com/JuliaLang/julia/blob/v1.8.2/deps/libuv.version
    (package
     (inherit libuv)
     (name "libuv-julia")
     (version (git-version "2.0.0" revision commit))
     (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/JuliaLang/libuv")
                    (commit commit)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0ib2cprvbyviwrzm0fw6dqvlbm9akf2kj3vjzp82q3gii74cv3c9"))))
     (home-page "https://github.com/JuliaLang/libuv")
     (properties '((hidden? . #t))))))

(define-public llvm-julia
  (package
   (inherit llvm-13)
   ;; No longer need llvm7-symver-jlprefix because of -DLLVM_VERSION_SUFFIX
   (arguments
    (substitute-keyword-arguments
     (package-arguments llvm-13)
     ((#:configure-flags flags)
      `(list
	;; Build a native compiler and the NVPTX backend (NVIDIA) since
	;; Julia insists on it, nothing more.  This reduces build times and
	;; disk usage.
	,(string-append "-DLLVM_TARGETS_TO_BUILD=" (system->llvm-target))
	"-DLLVM_EXPERIMENTAL_TARGETS_TO_BUILD=NVPTX"
	"-DLLVM_INSTALL_UTILS=ON"
	"-DLLVM_BUILD_TESTS=ON"
	"-DLLVM_ENABLE_FFI=ON"
	"-DLLVM_ENABLE_RTTI=ON"
	;; "-DLLVM_HOST_TRIPLE=${stdenv.hostPlatform.config}"
	;; "-DLLVM_DEFAULT_TARGET_TRIPLE=${stdenv.hostPlatform.config}"
	;; "-DLLVM_EXPERIMENTAL_TARGETS_TO_BUILD=WebAssembly"
	"-DLLVM_ENABLE_DUMP=ON"
	"-DLLVM_LINK_LLVM_DYLIB=ON"
	"-DLLVM_VERSION_SUFFIX:STRING=jl"))))))

(define-public utf8proc-2.7.0
  (package
   (inherit utf8proc)
   (name "utf8proc")
   (version "2.7.0")
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (url "https://github.com/JuliaStrings/utf8proc")
           (commit (string-append "v" version))))
     (file-name (git-file-name name version))
     (sha256
      (base32 "1wrsmnaigal94gc3xbzdrrm080zjhihjfdla5admllq2w5dladjj"))))
   (arguments
    (substitute-keyword-arguments (package-arguments utf8proc)
				  ((#:phases phases)
				   `(modify-phases ,phases
						   (replace 'check-data
							    (lambda* (#:key inputs native-inputs #:allow-other-keys)
							      (display native-inputs)
							      (for-each (lambda (i)
									  (copy-file (assoc-ref (or native-inputs inputs) i)
										     (string-append "data/" i)))
									'("NormalizationTest.txt" "GraphemeBreakTest.txt"
									  "DerivedCoreProperties.txt"))))))))
   (native-inputs
    (append
     (package-native-inputs utf8proc)
     (let ((UNICODE_VERSION "14.0.0"))
       `(("DerivedCoreProperties.txt"
          ,(origin
            (method url-fetch)
            (uri (string-append "https://www.unicode.org/Public/"
                                UNICODE_VERSION "/ucd/DerivedCoreProperties.txt"))
            (sha256
             (base32 "1g77s8g9443dd92f82pbkim7rk51s7xdwa3mxpzb1lcw8ryxvvg3"))))
         ;; For tests
         ("ruby" ,ruby)))))))

(define-public julia
  (package
   (name "julia")
   (version "1.8.2")
   (source (origin
            (method url-fetch)
            (uri (string-append
                  "https://github.com/JuliaLang/julia/releases/download/v"
                  version "/julia-" version ".tar.gz"))
            (sha256
             (base32
	      "0pgvn6750wh7g9jwskk63v7hl07wxf7kxa3mggnn7yaxpwsylb1y"))
            ;; (patches
            ;;  (search-patches "julia-SOURCE_DATE_EPOCH-mtime.patch"
            ;;                  "julia-allow-parallel-build.patch"))
	    ))
   (build-system gnu-build-system)
   (arguments
    `(#:test-target "test"
      #:modules ((ice-9 match)
                 (guix build gnu-build-system)
                 (guix build utils))

      ;; The test suite takes many times longer than building and
      ;; can easily fail on smaller machines when they run out of memory.
      #:tests? ,(not (target-aarch64?))

      ;; Do not strip binaries to keep support for full backtraces.
      ;; See https://github.com/JuliaLang/julia/issues/17831
      #:strip-binaries? #f

      ;; The DSOs use $ORIGIN to refer to each other, but (guix build
      ;; gremlin) doesn't support it yet, so skip this phase.
      #:validate-runpath? #f

      #:phases
      (modify-phases %standard-phases
		     (delete 'configure)
		     (add-after 'unpack 'prepare-deps
				(lambda* (#:key inputs #:allow-other-keys)
				  ;; needed by libwhich
				  (setenv "LD_LIBRARY_PATH"
					  (string-join (map (lambda (pkg)
							      (string-append (assoc-ref inputs pkg)
									     "/lib"))
							    '("curl" "dsfmt"
							      "gmp" "lapack"
							      "libssh2" "libnghttp2" "libgit2"
							      "mbedtls" "mpfr"
							      "openblas" "openlibm" "pcre2"
							      "suitesparse" "gfortran:lib"
							      "libblastrampoline"))
						       ":"))))
		     ;; FIXME: Building the documentation requires Julia packages that
		     ;; would be downloaded from the Internet.  We should build them in a
		     ;; separate build phase.
		     (add-after 'unpack 'disable-documentation
				(lambda _
				  (substitute* "Makefile"
					       (("(install: .*) \\$\\(BUILDROOT\\)/doc/_build/html/en/index.html" _ line)
						(string-append line "\n"))
					       (("src ui doc deps")
						"src ui deps"))))
		     (add-after 'unpack 'activate-gnu-source-for-loader
				(lambda _
				  (substitute* "cli/Makefile"
					       (("LOADER_CFLAGS =") "LOADER_CFLAGS = -D_GNU_SOURCE"))))
		     (add-after 'unpack 'change-number-of-precompile-statements
				(lambda _
				  ;; Remove nss-certs drops the number of statements below 1200,
				  ;; causing the build to fail prematurely.
				  (substitute* "contrib/generate_precompile.jl"
					       (("1200") "1100"))))
		     ;; For some reason libquadmath is unavailable on this architecture.
		     ;; https://github.com/JuliaLang/julia/issues/41613
		     ,@(if (target-aarch64?)
			   '((add-after 'unpack 'drop-libquadmath-on-aarch64
					(lambda _
					  (substitute* '("contrib/fixup-libgfortran.sh"
							 "deps/csl.mk"
							 "base/Makefile")
						       ((".*libquadmath.*") ""))
					  (substitute* "Makefile"
						       (("libquadmath ") "")))))
			   '())
		     (add-before 'check 'set-home
				 ;; Some tests require a home directory to be set.
				 (lambda _ (setenv "HOME" "/tmp")))
		     (add-before 'build 'fix-include-and-link-paths
				 (lambda* (#:key inputs #:allow-other-keys)
				   ;; LIBUTF8PROC is a linker flag, not a build target.  It is
				   ;; included in the LIBFILES_* variable which is used as a
				   ;; collection of build targets and a list of libraries to link
				   ;; against.
				   (substitute* "src/flisp/Makefile"
						(("\\$\\(BUILDDIR\\)/\\$\\(EXENAME\\)\\$\\(EXE\\): \\$\\(OBJS\\) \\$\\(LIBFILES_release\\)")
						 "$(BUILDDIR)/$(EXENAME)$(EXE): $(OBJS) $(LLT_release)")
						(("\\$\\(BUILDDIR\\)/\\$\\(EXENAME\\)-debug$(EXE): \\$\\(DOBJS\\) \\$\\(LIBFILES_debug\\)")
						 "$(BUILDDIR)/$(EXENAME)-debug\\$\\(EXE\\): $(DOBJS) $(LLT_debug)"))

				   ;; The REPL must be linked with libuv.
				   (substitute* "cli/Makefile"
						(("JLDFLAGS \\+= ")
						 (string-append "JLDFLAGS += "
								(assoc-ref %build-inputs "libuv")
								"/lib/libuv.so ")))

				   (substitute* "base/Makefile"
						(("\\$\\(build_includedir\\)/uv/errno.h")
						 (search-input-file inputs "/include/uv/errno.h")))))
		     (add-before 'build 'replace-default-shell
				 (lambda _
				   (substitute* "base/client.jl"
						(("/bin/sh") (which "sh")))))
		     (add-before 'build 'shared-objects-paths
				 (lambda* (#:key inputs #:allow-other-keys)
				   (let ((jlpath
					  (lambda (pkgname)
					    (string-append
					     "stdlib/" pkgname "_jll/src/" pkgname "_jll.jl")))
					 (from
					  (lambda (libname)
					    (string-append "const " libname " = .*\\.so")))
					 (to
					  (lambda* (pkg libname #:optional libname_jl)
					    (string-append
					     "const " (or libname_jl libname)  "= \""
					     (assoc-ref inputs pkg) "/lib/" libname ".so"))))
				     (substitute* (jlpath "dSFMT")
						  (((from "libdSFMT")) (to "dsfmt" "libdSFMT")))
				     (substitute* (jlpath "GMP")
						  (((from "libgmp")) (to "gmp" "libgmp"))
						  (((from "libgmpxx")) (to "gmp" "libgmpxx")))
				     (substitute* (jlpath "libLLVM")
						  (((from "libLLVM")) (to "llvm" "libLLVM")))
				     (substitute* (jlpath "LibCURL")
						  (((from "libcurl")) (to "curl" "libcurl")))
				     (substitute* (jlpath "LibGit2")
						  (((from "libgit2")) (to "libgit2" "libgit2")))
				     (substitute* (jlpath "LibSSH2")
						  (((from "libssh2")) (to "libssh2" "libssh2")))
				     (substitute* (jlpath "LibUV")
						  (((from "libuv")) (to "libuv" "libuv")))
				     (substitute* (jlpath "LibUnwind")
						  (((from "libunwind")) (to "libunwind" "libunwind")))
				     (substitute* (jlpath "libblastrampoline")
						  (((from "libblastrampoline")) (to "libblastrampoline" "libblastrampoline")))
				     (substitute* (jlpath "MPFR")
						  (((from "libmpfr")) (to "mpfr" "libmpfr")))
				     (substitute* (jlpath "MbedTLS")
						  ;; For the newer version of mbedtls-apache:
						  (("libmbedcrypto.so.5") "libmbedcrypto.so.6")
						  (((from "libmbedcrypto")) (to "mbedtls" "libmbedcrypto"))
						  (((from "libmbedtls")) (to "mbedtls" "libmbedtls"))
						  (((from "libmbedx509")) (to "mbedtls" "libmbedx509")))
				     (substitute* (jlpath "nghttp2")
						  (((from "libnghttp2")) (to "libnghttp2" "libnghttp2")))
				     (substitute* (jlpath "OpenBLAS")
						  (((from "libopenblas")) (to "openblas" "libopenblas")))
				     (substitute* (jlpath "OpenLibm")
						  (((from "libopenlibm")) (to "openlibm" "libopenlibm")))
				     (substitute* (jlpath "PCRE2")
						  (((from "libpcre2")) (to "pcre2" "libpcre2" "libpcre2_8")))
				     (substitute* (jlpath "SuiteSparse")
						  (((from "libamd")) (to "suitesparse" "libamd"))
						  (((from "libbtf")) (to "suitesparse" "libbtf"))
						  (((from "libcamd")) (to "suitesparse" "libcamd"))
						  (((from "libccolamd")) (to "suitesparse" "libccolamd"))
						  (((from "libcholmod")) (to "suitesparse" "libcholmod"))
						  (((from "libcolamd")) (to "suitesparse" "libcolamd"))
						  (((from "libklu")) (to "suitesparse" "libklu"))
						  (((from "libldl")) (to "suitesparse" "libldl"))
						  (((from "librbio")) (to "suitesparse" "librbio"))
						  (((from "libspqr")) (to "suitesparse" "libspqr"))
						  (((from "libsuitesparse")) (to "suitesparse" "libsuitesparse"))
						  (((from "libsuitesparseconfig"))
						   (to "suitesparse" "libsuitesparseconfig"))
						  (((from "libumfpack")) (to "suitesparse" "libumfpack")))
				     (substitute* (jlpath "Zlib")
						  (((from "libz")) (to "zlib" "libz"))))))
		     (add-after 'unpack 'enable-parallel-tests
				(lambda* (#:key parallel-tests? #:allow-other-keys)
				  (setenv "JULIA_CPU_THREADS" (if parallel-tests?
								  (number->string (parallel-job-count))
								  "1"))
				  (format #t "JULIA_CPU_THREADS environment variable set to ~a~%"
					  (getenv "JULIA_CPU_THREADS"))))
		     (add-after 'unpack 'adjust-test-suite
				(lambda* (#:key inputs #:allow-other-keys)
				  (let ((pcre2 (assoc-ref inputs "pcre2"))
					(mbedtls-apache (assoc-ref inputs "mbedtls"))
					(mpfr (assoc-ref inputs "mpfr"))
					(gmp (assoc-ref inputs "gmp"))
					(nghttp2 (assoc-ref inputs "libnghttp2"))
					(zlib (assoc-ref inputs "zlib"))
					(suitesparse (assoc-ref inputs "suitesparse")))
				    ;; Some tests only check to see if the input is the correct version.
				    (substitute* "stdlib/PCRE2_jll/test/runtests.jl"
						 (("10.40.0") ,(package-version pcre2)))
				    (substitute* "stdlib/MbedTLS_jll/test/runtests.jl"
						 (("2.24.0") ,(package-version mbedtls-apache)))
				    (substitute* "stdlib/MPFR_jll/test/runtests.jl"
						 (("4.1.0") ,(package-version mpfr)))
				    (substitute* "stdlib/GMP_jll/test/runtests.jl"
						 (("6.2.0") ,(package-version gmp)))
				    (substitute* "stdlib/nghttp2_jll/test/runtests.jl"
						 (("1.41.0") ,(package-version nghttp2)))
				    (substitute* "stdlib/Zlib_jll/test/runtests.jl"
						 (("1.2.12") ,(package-version zlib)))
				    (substitute* "stdlib/SuiteSparse_jll/test/runtests.jl"
						 (("5004") ,(string-replace-substring
							     (version-major+minor
							      (package-version suitesparse)) "." "0"))))))
		     (add-before 'check 'disable-broken-tests
				 (lambda _
				   ;; disabling REPL tests because they require a stdin
				   ;; There are some read-only precompile issues in the 1.6 series.
				   ;; https://github.com/JuliaLang/julia/pull/41614
				   ;; https://github.com/JuliaLang/julia/issues/41156
				   (substitute* "test/choosetests.jl"
						(("\"precompile\",") ""))
				   ;; Dates/io tests fail on master when networking is unavailable
				   ;; https://github.com/JuliaLang/julia/issues/34655
				   (substitute* "stdlib/Dates/test/io.jl"
						(("using Dates") "import Dates
using Dates: @dateformat_str, Date, DateTime, DateFormat, Time"))
				   ;; julia embeds a certificate, we are not doing that
				   (substitute* "stdlib/MozillaCACerts_jll/test/runtests.jl"
						(("@test isfile\\(MozillaCACerts_jll.cacert\\)")
						 "@test_broken isfile(MozillaCACerts_jll.cacert)"))
				   ;; This tests for the the OpenBLAS 0.3.13 ex-shift patch working. Depending on OpenBLAS version used as input, this test may or may not pass, and it should really be in the openblas package.
				   (substitute* "stdlib/LinearAlgebra/test/schur.jl"
						;; @test_broken cannot be used because an error should not be raised if OpenBLAS is upgraded and the ex-shift patch starts working.
						(("@testset \"Generalized Schur convergence\" begin.*\nend")
						 " "))
				   ;; LU factorization test uses the wrong integer type for USE_BLAS64=0; patch submitted upstream at https://github.com/JuliaLang/julia/TODO
				   (substitute* "stdlib/LinearAlgebra/test/lu.jl"
						(("\\$Int")
						 "$BlasInt"))
				   ;; since certificate is not present some tests are failing in network option
				   (substitute* "usr/share/julia/stdlib/v1.8/NetworkOptions/test/runtests.jl"
						(("@test isfile\\(bundled_ca_roots\\(\\)\\)")
						 "@test_broken isfile(bundled_ca_roots())")
						(("@test ispath\\(ca_roots_path\\(\\)\\)")
						 "@test_broken ispath(ca_roots_path())")
						(("@test ca_roots_path\\(\\) \\!= bundled_ca_roots\\(\\)")
						 "@test_broken ca_roots_path() != bundled_ca_roots()"))
				   ;; WARNING: failed to select UTF-8 encoding, using ASCII
				   ;; Using 'setlocale' doesn't affect the test failures.
					;(setlocale LC_ALL "en_US.utf8")
					;(setenv "LC_ALL" "en_US.utf8")
				   (substitute* "test/cmdlineargs.jl"
						(("test v\\[3") "test_broken v[3")
						(("test isempty\\(v\\[3") "test_broken isempty(v[3"))
				   ;; These test(s) randomly fails because they depend on CPU.
				   (substitute* "test/math.jl"
						;; @test_broken cannot be used because if the test randomly
						;; passes, then it also raises an error.
						(("@test isinf\\(log1p\\(-one\\(T\\)\\)\\)")
						 " "))))
		     (add-before 'install 'symlink-libraries
				 (lambda* (#:key inputs outputs #:allow-other-keys)
				   (let ((link
					  (lambda (pkgname dir pred)
					    (map (lambda (file)
						   (unless (file-exists?
							    (string-append dir (basename file)))
						     (symlink file (string-append dir (basename file)))))
						 (find-files (string-append (assoc-ref inputs pkgname)
									    "/lib") pred)))))
				     (link "curl" "usr/lib/" "\\.so") ; missing libpthreads libLLVM-11jl
				     (link "suitesparse" "usr/lib/julia/" "libbtf\\.so")
				     (link "suitesparse" "usr/lib/julia/" "libklu\\.so")
				     (link "suitesparse" "usr/lib/julia/" "libldl\\.so")
				     (link "suitesparse" "usr/lib/julia/" "librbio\\.so")
				     (link "gmp" "usr/lib/julia/" "libgmpxx\\.so")
				     (link "libuv" "usr/lib/julia/" "libuv\\.so")
				     (link "zlib" "usr/lib/julia/" "libz\\.so")
				     (link "libunwind" "usr/lib/julia/" "libunwind\\.so")
				     (symlink (string-append (assoc-ref inputs "p7zip") "/bin/7z")
					      "usr/libexec/7z"))))
		     (add-after 'install 'symlink-llvm-utf8proc
				(lambda* (#:key inputs outputs #:allow-other-keys)
				  (let* ((out (assoc-ref outputs "out"))
					 (link
					  (lambda (pkgname pred)
					    (map (lambda (file)
						   (unless (file-exists?
							    (string-append out "/lib/julia/"
									   (basename file)))
						     (symlink file (string-append out "/lib/julia/"
										  (basename file)))))
						 (find-files (string-append (assoc-ref inputs pkgname)
									    "/lib") pred)))))
				    (link "llvm" "libLLVM-13jl\\.so")
				    (link "utf8proc" "libutf8proc\\.so"))))
		     (add-after 'install 'make-wrapper
				(lambda* (#:key inputs outputs #:allow-other-keys)
				  (let* ((out (assoc-ref outputs "out"))
					 (bin (string-append out "/bin"))
					 (program "julia"))
				    (with-directory-excursion bin
							      (wrap-program program
									    `("JULIA_LOAD_PATH" ":" prefix
									      ("" "$JULIA_LOAD_PATH"))
									    `("JULIA_DEPOT_PATH" ":" prefix
									      ("" "$JULIA_DEPOT_PATH"))))))))
      #:make-flags
      (list
       "VERBOSE=1" ;; more helpful logging of what make is doing
       (string-append "prefix=" (assoc-ref %outputs "out"))

       ;; Passing the MARCH or JULIA_CPU_TARGET flag is necessary to build
       ;; binary substitutes for the supported architectures.  See also
       ;; https://docs.julialang.org/en/v1/devdocs/sysimg/#Specifying-multiple-system-image-targets
       ,(match (or (%current-target-system)
                   (%current-system))
          ("x86_64-linux"
           ;; These are the flags that upstream uses for their binaries.
           "JULIA_CPU_TARGET=generic;generic,-cx16,clone_all;sandybridge,-xsaveopt,clone_all;haswell,-rdrnd,base(1)")
          ("i686-linux" "MARCH=pentium4")
          ("armhf-linux" "JULIA_CPU_TARGET=armv7-a,neon")
          ("powerpc64le-linux" "JULIA_CPU_TARGET=pwr8")
          ;; Prevent errors when querying this package on unsupported
          ;; platforms, e.g. when running "guix package --search="
          ;; and also of targeting the builder's architecture.
          (_ "JULIA_CPU_TARGET=generic"))

       "CONFIG_SHELL=bash -x"     ; needed to build bundled libraries
       "USE_BINARYBUILDER=0"
       ;; list (and order!) of "USE_SYSTEM_*" is here:
       ;; https://github.com/JuliaLang/julia/blob/v1.8.2/Make.inc
       "USE_SYSTEM_LIBWHICH=1"
       "USE_SYSTEM_LIBBLASTRAMPOLINE=1"
       "USE_SYSTEM_CSL=1"
       "USE_SYSTEM_LLVM=1"
       "USE_SYSTEM_LIBUNWIND=1"
       "USE_SYSTEM_PCRE=1"
       "USE_SYSTEM_OPENLIBM=1"
       "USE_SYSTEM_DSFMT=1"
       "USE_SYSTEM_BLAS=1"
       "USE_SYSTEM_LAPACK=1"
       "USE_SYSTEM_GMP=1"
       "USE_SYSTEM_MPFR=1"
       "USE_SYSTEM_LIBSUITESPARSE=1"
       "USE_SYSTEM_LIBUV=1"
       "USE_SYSTEM_UTF8PROC=1"
       "USE_SYSTEM_MBEDTLS=1"
       "USE_SYSTEM_LIBSSH2=1"
       "USE_SYSTEM_NGHTTP2=1"
       "USE_SYSTEM_CURL=1"
       "USE_SYSTEM_LIBGIT2=1"
       "USE_SYSTEM_PATCHELF=1"
       "USE_SYSTEM_ZLIB=1"
       "USE_SYSTEM_P7ZIP=1"

       "NO_GIT=1"             ; build from release tarball.
       "USE_BLAS64=0"         ; needed when USE_SYSTEM_BLAS=1
       "LIBBLAS=-lopenblas"
       "LIBBLASNAME=libopenblas"

       (string-append "SUITESPARSE_INC=-I "
                      (assoc-ref %build-inputs "suitesparse")
                      "/include")
       "USE_GPL_LIBS=1"       ; proudly
       (string-append "UTF8PROC_INC="
                      (assoc-ref %build-inputs "utf8proc")
                      "/include")
       "LLVM_VER=11.0.0"

       "USE_LLVM_SHLIB=1"
       (string-append "LIBUV="
                      (assoc-ref %build-inputs "libuv")
                      "/lib/libuv.so")
       (string-append "LIBUV_INC="
                      (assoc-ref %build-inputs "libuv")
                      "/include"))))
   (inputs
    `(("coreutils" ,coreutils) ; for bindings to "mkdir" and the like
      ("curl" ,curl-ssh)
      ("gfortran" ,gfortran)
      ;; required for libgcc_s.so
      ("gfortran:lib" ,gfortran "lib")
      ("gmp" ,gmp)
      ("lapack" ,lapack)
      ("libblastrampoline" ,libblastrampoline)
      ("libgit2" ,libgit2-1.1)
      ("libnghttp2" ,nghttp2 "lib")
      ("libssh2" ,libssh2)
      ("libunwind" ,libunwind-julia)
      ("libuv" ,libuv-julia)
      ("llvm" ,llvm-julia)
      ("mbedtls" ,mbedtls-apache)
      ("mpfr" ,mpfr)
      ("openblas" ,openblas)
      ("openlibm" ,openlibm)
      ("p7zip" ,p7zip)
      ("pcre2" ,pcre2)
      ("suitesparse" ,suitesparse)
      ("utf8proc" ,utf8proc-2.7.0)
      ("wget" ,wget)
      ("which" ,which)
      ("zlib" ,zlib)
      ;; Find dependencies versions here:
      ;; https://raw.githubusercontent.com/JuliaLang/julia/v1.8.2/deps/*.version
      ("dsfmt" ,dsfmt)
      ("libwhich" ,libwhich)))
   (native-inputs
    `(("openssl" ,openssl)
      ("perl" ,perl)
      ("patchelf" ,patchelf)
      ("pkg-config" ,pkg-config)
      ("python" ,python-2)))
   (native-search-paths
    (list (search-path-specification
           (variable "JULIA_LOAD_PATH")
           (files (list "share/julia/loadpath/")))
          (search-path-specification
           (variable "JULIA_DEPOT_PATH")
           (files (list "share/julia/")))))
   ;; Julia is not officially released for ARM and MIPS.
   ;; See https://github.com/JuliaLang/julia/issues/10639
   (supported-systems '("i686-linux" "x86_64-linux" "aarch64-linux"))
   (home-page "https://julialang.org/")
   (synopsis "High-performance dynamic language for technical computing")
   (description
    "Julia is a high-level, high-performance dynamic programming language for
technical computing, with syntax that is familiar to users of other technical
computing environments.  It provides a sophisticated compiler, distributed
parallel execution, numerical accuracy, and an extensive mathematical function
library.")
   (license license:expat)))

julia
