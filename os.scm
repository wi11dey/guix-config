(add-to-load-path ".") ; TODO Submit patch to restore (current-filename) behavior.
(use-modules (services startx)
	     (gnu)
	     (gnu system)
	     (gnu system nss)
	     (gnu system keyboard)
	     (gnu services)
	     (gnu services networking)
	     (gnu packages admin)
	     (gnu packages certs)
	     (gnu packages xorg)
	     ;; (gnu packages android)
	     (gnu services xorg)
	     (gnu packages disk)
	     (gnu system setuid)
	     (guix utils)
	     (guix gexp)
	     (guix packages)
	     (ice-9 textual-ports)
	     (rnrs io ports)
	     (srfi srfi-1))

(operating-system
 (host-name "guix")
 (timezone (let ((current (call-with-port (open-input-file "/etc/timezone")
					  (lambda (tz) (get-line tz))))
		 (stdout (standard-output-port))) ; In case (current-output-port) has been set to something different.
	     (put-string stdout (string-append "Timezone"
					       (if (string-null? current)
						   ": "
						   (string-append " [default " current "]: "))))
	     (flush-output-port stdout)
	     (let ((new (get-line (standard-input-port))))
	       (if (string-null? new) current new))))
 (locale "en_US.utf8")
 (keyboard-layout (keyboard-layout "us" #:options '("altwin:ctrl_alt_win"
						    "ctrl:rctrl_ralt"
						    "caps:menu"
						    "numpad:mac")))
 (bootloader (bootloader-configuration
	      (bootloader grub-bootloader)
	      (targets '("/dev/sda"))
	      (keyboard-layout keyboard-layout)))
 (kernel-arguments '("modprobe.blacklist=pcspkr,snd_pcsp"
		     "quiet"))
 (file-systems (cons* (file-system
		       (device (file-system-label "guix"))
		       (mount-point "/")
		       (type "btrfs"))
		      %base-file-systems))
 (swap-devices (list (swap-space
		      (target (file-system-label "swap")))))
 (users (cons* (user-account
		(name "user")
		(comment "User")
		(group "users")
		;; Adding the account to the "wheel" group
		;; makes it a sudoer.  Adding it to "audio"
		;; and "video" allows the user to play sound
		;; and access the webcam.
		(supplementary-groups '("wheel"
					"netdev"
					"audio"
					"video"
					;; "adbusers"
					"input")))
	       %base-user-accounts))
 (packages (cons* wpa-supplicant-minimal
		  nss-certs
		  %base-packages))
 (setuid-programs
  (cons* (setuid-program
	  (program (file-append udevil "/bin/udevil")))
	 %setuid-programs))
 (services (cons* (service startx-service-type
			   (xorg-configuration
			    (keyboard-layout keyboard-layout)
			    (server (package
				     (inherit xorg-server)
				     (name "xorg-server-no-dbus")
				     (arguments
				      (substitute-keyword-arguments
				       (package-arguments xorg-server)
				       ((#:configure-flags flags)
					`(cons* "--disable-systemd-logind" ,flags))))))
			    (server-arguments '("-nolisten" "tcp"
						"-noreset"))
			    (extra-config (list "
Section \"InputClass\"
  Identifier \"touchpad\"
  Driver \"Synaptics\"
  MatchIsTouchpad \"on\"
  Option \"HorizTwoFingerScroll\" \"on\"
  Option \"VertScrollDelta\" \"-75\"
  Option \"HorizScrollDelta\" \"-35\"
  Option \"RightButtonAreaLeft\" \"0\"
  Option \"RightButtonAreaTop\" \"0\"
EndSection
"))))
		  ;; (udev-rules-service 'android android-udev-rules
                  ;;                     #:groups '("adbusers"))
		  (modify-services %base-services
				   (console-font-service-type
				    config => (map (lambda (tty+font)
						     (cons (car tty+font)
							   "Lat2-Terminus16"))
						   config))
				   (guix-service-type
				    config => (guix-configuration
					       (inherit config)
					       (tmpdir "/dev/shm")))))))
