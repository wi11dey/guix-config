(add-to-load-path (dirname (current-filename)))
(use-modules
 (services startx)

 (gnu bootloader)
 (gnu packages admin)
 ;; (gnu packages android)
 (gnu packages certs)
 (gnu packages disk)
 (gnu packages linux)
 (gnu packages xorg)
 (gnu services networking)
 (gnu services shepherd)
 (gnu services xorg)
 (gnu services)
 (gnu system keyboard)
 (gnu system nss)
 (gnu system setuid)
 (gnu system)
 (gnu)
 (guix gexp)
 (guix packages)
 (guix utils)
 (ice-9 pretty-print)
 (ice-9 textual-ports)
 (rnrs io ports)
 (srfi srfi-1))

(define %operating-system
  (operating-system
   (host-name "guix")
   (issue (string-append
	   "\x1b[9;1]" ; 1 minute screen timeout.
	   ;; Solarized light:
	   "\x1b]PB839496" ; S_base00
	   "\x1b]PA93a1a1" ; S_base01
	   "\x1b]P0eee8d5" ; S_base02
	   "\x1b]P62aa198" ; S_cyan
	   "\x1b]P8fdf6e3" ; S_base03
	   "\x1b]P2859900" ; S_green
	   "\x1b]P5d33682" ; S_magenta
	   "\x1b]P1dc322f" ; S_red
	   "\x1b]PC657b83" ; S_base0
	   "\x1b]PE586e75" ; S_base1
	   "\x1b]P9cb4b16" ; S_orange
	   "\x1b]P7073642" ; S_base2
	   "\x1b]P4268bd2" ; S_blue
	   "\x1b]P3b58900" ; S_yellow
	   "\x1b]PF002b36" ; S_base3
	   "\x1b]PD6c71c4" ; S_violet
	   "\x1bc" ; Reset console.
	   ))
   ;; TODO Parse options from (file-append tzdata "/share/zoneinfo/zone.tab") using zonetab->timezones from (gnu installer timezone):
   (timezone (let ((current (call-with-port (open-input-file "/etc/timezone")
			      (lambda (tz) (get-line tz))))
		   (stderr (standard-error-port))) ; In case (current-error-port) has been set to something different.
	       (put-string stderr (string-append "Timezone"
						 (if (string-null? current)
						     ": "
						     (string-append " [default " current "]: "))))
	       (flush-output-port stderr)
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
		       "mem_sleep_default=deep"
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
					  "power"
					  "backlight"
					  "video"
					  ;; "adbusers"
					  "input")))
		 %base-user-accounts))
   (groups (cons* (user-group
		   (name "power")
		   (system? #t))
		  %base-groups))
   (packages (cons* wpa-supplicant-minimal
		    btrfs-progs
		    nss-certs
		    %base-packages))
   (setuid-programs
    (cons* (setuid-program
	    (program (file-append udevil "/bin/udevil")))
	   %setuid-programs))
   (sudoers-file (plain-file "sudoers" "\
root ALL=(ALL) ALL
%wheel ALL=(ALL) ALL
%power ALL=(ALL) NOPASSWD: /run/current-system/profile/sbin/halt,/run/current-system/profile/sbin/shutdown,/run/current-system/profile/sbin/reboot\n"))
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
  Option \"HorizScrollDelta\" \"35\"
  Option \"RightButtonAreaLeft\" \"0\"
  Option \"RightButtonAreaTop\" \"0\"
EndSection
"))))
		    (simple-service 'btrfs-compress-store activation-service-type
				    #~(system* #$(file-append btrfs-progs "/bin/btrfs") "property" "set" "/gnu/store" "compression" "zstd"))
		    (simple-service 'suspend-permission activation-service-type
				    #~(begin
					(chown "/sys/power/state" 0 (group:gid (getgrnam "power")))
					(chmod "/sys/power/state" #o664)))
		    (udev-rules-service 'backlight-permission
					(file->udev-rule "backlight.rules"
							 (mixed-text-file "backlight.rules"
									  "ACTION==\"add\","
									  "SUBSYSTEM==\"backlight\","
									  "RUN+=\"" coreutils "/bin/chgrp backlight $sys$devpath/brightness\","
									  "RUN+=\"" coreutils "/bin/chmod g+w       $sys$devpath/brightness\""))
					#:groups '("backlight"))
		    ;; (udev-rules-service 'android android-udev-rules
                    ;;                     #:groups '("adbusers"))
		    (modify-services %base-services
				     (mingetty-service-type
				      config => (mingetty-configuration
						 (inherit config)
						 (auto-login "user")
						 (login-pause? (not (string=? (mingetty-configuration-tty config)
									      "tty1")))))
				     (console-font-service-type
				      config => (map (lambda (tty+font)
						       (cons (car tty+font)
							     "Lat2-Terminus16"))
						     config)))))))

%operating-system
