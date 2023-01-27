(load "../operating-system.scm") ; Cannot use modules with add-to-load-path because (current-filename) is #f under guix system reconfigure.

(operating-system
 (inherit %operating-system)
 ;; ...
 )
