(define-module (services wpa-dhcp)
  #:use-module (gnu services)
  #:use-module (gnu packages wpa-supplicant)
  #:use-module (gnu packages networking)
  #:use-module (guix records)
  #:use-module (guix gexp)
  #:export (wpa-dhcp-configuration
	    wpa-dhcp-configuration?
	    wpa-dhcp-configuration-wpa-supplicant
	    wpa-dhcp-configuration-dhcp

	    wpa-dhcp-service))

(define-record-type* <wpa-dhcp-configuration>
  wpa-dhcp-configuration make-wpa-dhcp-configuration
  wpa-dhcp-configuration?
  (wpa-supplicant wpa-dhcp-configuration-wpa-supplicant
		  (default wpa-supplicant))
  (dhcp wpa-dhcp-configuration-dhcp
	(default isc-dhcp)))

(define (wpa-dhcp-service config)
  (shepherd-service
   (documentation "Automatically renew DHCP when wpa_supplicant connects.")
   (provision '(wpa-dhcp))
   (requirement '(networking wpa-supplicant))
   (start #~(make-forkexec-constructor
	     (list #$(file-append (wpa-dhcp-configuration-wpa-supplicant config)
				  "/sbin/wpa_cli")
		   "-a"
		   #$(program-file "wpa-dhcp-action"
				   (let ((dhclient (file-append (wpa-dhcp-configuration-dhcp config)
								"/sbin/dhclient")))
				     #~(if (string=? (cadr (command-line))
						     "CONNECTED")
					   (begin
					     (system* #$dhclient "-r")
					     (system* #$dhclient))))))))
   (stop #~(make-kill-destructor))))
