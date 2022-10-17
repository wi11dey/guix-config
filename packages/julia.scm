(use-modules
 (ice-9 match)
 (guix utils)
 (guix packages)
 (guix download)
 (guix git-download)
 (guix gexp)
 (guix build-system gnu)
 ((guix licenses) #:prefix license:)
 (gnu packages)
 (gnu packages julia)
 (gnu packages libunwind)
 (gnu packages libevent)
 (gnu packages llvm))

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

(define (julia-patch-url version name)
  (string-append "https://raw.githubusercontent.com/JuliaLang/julia/v" version
                 "/deps/patches/" name ".patch"))

(define-public julia-1.8
  (package
   (inherit julia)
   (version "1.8.2")
   (source
    (origin
     (method url-fetch)
     (uri
      (string-append
       "https://github.com/JuliaLang/julia/releases/download/v"
       version "/julia-" version ".tar.gz"))
     (sha256
      (base32
       "0pgvn6750wh7g9jwskk63v7hl07wxf7kxa3mggnn7yaxpwsylb1y"))
     ;; (patches
     ;;  (search-patches "julia-SOURCE_DATE_EPOCH-mtime.patch"
     ;;                  "julia-allow-parallel-build.patch"))
     ))
   (arguments
    (substitute-keyword-arguments
     (package-arguments julia)
     ((#:make-flags flags)
      `(cons*
	"USE_SYSTEM_LIBWHICH=1"
	"USE_SYSTEM_LIBSUITESPARSE=1"
	"USE_SYSTEM_LIBBLASTRAMPOLINE=1"
	,flags))
     ((#:phases phases)
      `(modify-phases ,phases
		      (delete 'use-system-libwhich)
		      (add-after 'shared-objects-paths 'libblastrampoline-path
				 (lambda* (#:key inputs #:allow-other-keys)
				   (substitute* "stdlib/libblastrampoline_jll/src/libblastrampoline_jll.jl"
						(("libblastrampoline.so")
						 (string-append
						  (assoc-ref inputs "libblastrampoline")
						  "/lib/libblastrampoline.so")))))
		      (add-after 'prepare-deps 'prepare-libblastrampoline
				 (lambda* (#:key inputs #:allow-other-keys)
				   (setenv "LD_LIBRARY_PATH"
					   (string-append
					    (assoc-ref inputs "libblastrampoline") "/lib:"
					    (getenv "LD_LIBRARY_PATH")))))
		      (replace 'disable-broken-tests
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
					       " "))))))))
   (inputs (modify-inputs (package-inputs julia)
			  (prepend libblastrampoline)
			  (replace "libuv"
				   (let ((commit "e6f0e4900e195c8352f821abe2b3cffc3089547b")
					 (revision "4"))
				     (package
				      (inherit libuv-julia)
				      (version (git-version "2.0.0" revision commit))
				      (source (origin
					       (method git-fetch)
					       (uri
						(git-reference
						 (url "https://github.com/JuliaLang/libuv")
						 (commit commit)))
					       (file-name
						(git-file-name
						 (package-name libuv-julia)
						 version))
					       (sha256
						(base32
						 "0ib2cprvbyviwrzm0fw6dqvlbm9akf2kj3vjzp82q3gii74cv3c9")))))))
			  (replace "libunwind"
				   (package
				    (inherit libunwind)
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
				       (map (match-lambda
					      ((name hash)
					       (origin
						(method url-fetch)
						(uri (julia-patch-url "1.8.2" name))
						(sha256 (base32 hash))
						(file-name name))))
					    '(("libunwind-static-arm"
					       "1jk3bmiw61ypcchqkk1fyg5wh8wpggk574wxyfyaic870zh3lhgq")
					      ("libunwind-prefer-extbl"
					       "0pf3lsq6zxlmqn86lk4fcj1xwdan9gbxyabrwgxcb59p8jjwsl8r")
					      ("libunwind-dwarf-table"
					       "1vzhzmqdhqdjqvib8f3k5jr5h47src31g77dgijnlf1bxynrjn3h")
					      ("libunwind-cfa-rsp"
					       "0qs5b1h5lsr5qakkv6sddgy5ghlxpjrn2jiqcvg7bkczy24klr6j"))))))
				    (arguments
				     (substitute-keyword-arguments (package-arguments libunwind)
								   ;; Skip tests on this older and patched version of libunwind.
								   ((#:tests? _ #t) #f)))
				    (home-page "https://github.com/JuliaLang/tree/master/deps/")))
			  (replace "llvm"
				   (package
				    (inherit llvm-13)
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
					 "-DLLVM_VERSION_SUFFIX:STRING=jl"))))))))))

julia-1.8
