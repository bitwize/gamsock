(include "srfi-1.scm")

(define generate-temp-file-name
  (let ((rs (make-random-source)))
    (random-source-randomize! rs)
    (let ((g (random-source-make-integers rs)))
      (lambda (prefix)
	(let ((p (open-output-string)))
	  (display prefix p)
	  (display (g 1000000000) p)
	  (get-output-string p))))))

(define-record-type document
  (really-make-document tag content)
  document?
  (tag document-tag)
  (content document-content))

(define (string-document? doc)
  (eq? (document-tag doc) 'string))

(define (newline-document? doc)
  (eq? (document-tag doc) 'newline))

(define (sequence-document? doc)
  (eq? (document-tag doc) 'sequence))

(define (empty-document? doc)
  (eq? (document-tag doc) 'empty))

(define empty-document
  (really-make-document 'empty '()))

(define newline-document
  (really-make-document 'newline '()))


(define (document-output doc port)
  (cond
   ((empty-document? doc) (display "" port))
   ((string-document? doc) (display (document-content doc) port))
   ((newline-document? doc) (newline port))
   ((sequence-document? doc) (let ((dc (document-content doc)))
			       (document-output (car dc) port)
			       (document-output (cdr dc) port)))))

(define (document-catenate doc1 doc2)
  (really-make-document 'sequence (cons doc1 doc2)))

(define (string->document str)
  (really-make-document 'string str))

(define (document-catenate-vertically doc1 doc2)
  (document-catenate doc1 (document-catenate newline-document doc2)))

(define (document-sequence-map f doc)
  (if
   (sequence-document? doc)
   (let ((ds (document-content doc)))
     (document-catenate (document-sequence-map f (car ds)) (document-sequence-map f (cdr ds))))
   (f doc)))

(define (document-indent1 doc nspaces)
  (let ((spaces (string->document (make-string nspaces #\space))))
    (document-sequence-map
     (lambda (doc)
       (if (newline-document? doc) (document-catenate doc spaces) doc))
     doc)))

(define (document-indent doc nspaces)
  (let ((spaces (string->document (make-string nspaces #\space))))
    (document-catenate spaces (document-indent1 doc nspaces))))

(define brace-open (string->document "{"))
(define brace-close (string->document "}"))
(define semicolon (string->document ";"))
(define include-open (string->document "#include <"))
(define include-close (string->document ">"))
(define (ifdef sym)
  (string->document (string-append "#ifdef "  sym)))
(define endif
  (string->document "#endif"))

(define (ifdef-wrap sym doc)
  (document-catenate-vertically
   (ifdef sym)
   (document-catenate-vertically
    doc
    endif)))

(define (c-output-constant const-scheme const-c)
  (string->document
   (string-append "printf(\"(" (symbol->string const-scheme)
		  " \\\"" const-c "\\\")\\n\");")))

(define (header hfile)
  (document-catenate
   include-open
   (document-catenate
    (string->document hfile)
    include-close)))

(define (headers hlist)
  (let loop ((l hlist)
	     (d empty-document))
    (cond
     ((null? l) d)
     (else (loop
	    (cdr l)
	    (document-catenate-vertically
	     d
	     (header (car l))))))))

(define (document-embrace doc)
  (document-catenate-vertically
   brace-open
   (document-catenate-vertically
    (document-indent doc 4)
    brace-close)))

(define (main-function doc)
  (document-catenate-vertically
   (string->document "int main(void)")
   (document-embrace (document-catenate-vertically doc
						   (string->document
						    "return 0;")))))
(define (include-tester-c-file hfile)
  (document-catenate-vertically
   (header hfile)
   (main-function empty-document)))

(define (try-compiling c-doc)
  (let* ((fn (generate-temp-file-name "gamsoc"))
	 (fnc (string-append fn ".c"))
	 (fno (string-append fn ".o"))
	 (oport (open-output-file fnc)))
    (dynamic-wind
	(lambda () #f)
	(lambda ()
	  (document-output c-doc oport)
	  (close-output-port oport)
	  (let* ((result
		  (shell-command
		   (string-append "/bin/sh -c \"gcc -c "
				  fnc
				  " -o "
				  fno
				  ">/dev/null 2>/dev/null\""))))
	    (zero? result)))
	(lambda ()
	  (if (file-exists? fnc) (delete-file fnc))
	  (if (file-exists? fno) (delete-file fno))))))

(define (try-compiling-to-executable c-doc)
  (let* ((fn (generate-temp-file-name "gamsoc"))
	 (fnc (string-append fn ".c"))
	 (oport (open-output-file fnc)))
    (dynamic-wind
	(lambda () #f)
	(lambda ()
	  (document-output c-doc oport)
	  (close-output-port oport)
	  (let* ((result
		  (shell-command
		   (string-append "/bin/sh -c \"gcc "
				  fnc
				  " -o "
				  fn
				  ">/dev/null 2>/dev/null\""))))
	    (if (zero? result) fn #f)))
	(lambda ()
	  (if (file-exists? fnc) (delete-file fnc))))))

(define (try-compiling-include-tester ifile)
  (try-compiling (include-tester-c-file ifile)))

(define (find-includes ilist)
  (let loop ((l1 '())
	     (l2 ilist))
    (if (null? l2)
	(reverse l1)
	(let* ((item (car l2))
	       (result (try-compiling-include-tester item)))
	  (if result
	      (loop (cons item l1) (cdr l2))
	      (loop l1 (cdr l2)))))))

(define socket-headers
  (find-includes
   (list 
    "string.h"
    "stdlib.h"
    "unistd.h"
    "fcntl.h"
    "errno.h"
    "sys/types.h"
    "sys/socket.h"
    "netinet/in.h"
    "sys/un.h")))


(define (symbol-tester-c-file constant)
   (document-catenate-vertically
    (headers socket-headers)
    (main-function (document-catenate
	   (string->document (string-append "(void)" constant))
	   semicolon))))

(define (try-compiling-symbol-tester constant)
  (try-compiling (symbol-tester-c-file constant)))

(define (defined-constants-check const-scheme const-c)
  (ifdef-wrap
   const-c
   (c-output-constant const-scheme const-c)))

(define (defined-constants-tester-c-file constants)
  (document-catenate-vertically
   (headers socket-headers)
   (main-function
    (fold-right
     document-catenate-vertically empty-document
	  (map
	   (lambda (x)
	     (defined-constants-check (car x) (cadr x)))
	   constants)))))

(define (try-compiling-defined-constants-tester constants)
  (try-compiling-to-executable (defined-constants-tester-c-file constants)))

(define (symbol-with-prefix pfx symbol-or-string)
  (string->symbol
   (string-append
    (symbol->string pfx)
    "/"
    (if (string? symbol-or-string)
	symbol-or-string
	(symbol->string symbol-or-string)))))

(define (find-constants const-list)
  (let ((exe (try-compiling-defined-constants-tester
		   const-list)))
    (if (not exe) '()
	(dynamic-wind
	    (lambda () #f)
	    (lambda ()
	      (let ((p (open-process (string-append "./" exe))))
		(let loop ((l '())
			   (datum (read p)))
		  (cond
		   ((eof-object? datum) (reverse l))
		   (else (loop (cons datum l) (read p)))))))
	    (lambda ()
	      (if (file-exists? exe)
		  (delete-file exe)))))))

(define (find-constants-with-prefix pfx const-list)
  (find-constants
   (map
    (lambda (x) (list (symbol-with-prefix pfx (car x)) (cadr x)))
    const-list)))

(define (constant-definition const-pair)
  (list
   'define-c-constant
   (car const-pair)
   'int
   (cadr const-pair)))

(define (constant-set-definition var const-list)
  (list
   'define
   var
   (cons 'list
	 const-list)))

(define (write-constant-definitions const-pairs port)
  (for-each (lambda (x)
	      (write (constant-definition x) port)
	      (newline port))
	    const-pairs))

(define (write-header-declarations hdrs port)
  (for-each (lambda (x)
	      (write `(c-declare ,(string-append "#include <" x ">")) port)
	      (newline port))
	    hdrs))

(define errnos
  (find-constants-with-prefix 
   'errno
   '((perm "EPERM")
    (noent "ENOENT")
    (srch "ESRCH")
    (intr "EINTR")
    (io "EIO")
    (nxio "ENXIO")
    ("2big" "E2BIG")
    (noexec "ENOEXEC")
    (badf "EBADF")
    (child "ECHILD")
    (deadlk "EDEADLK")
    (nomem "ENOMEM")
    (acces "EACCES")
    (fault "EFAULT")
    (notblk "ENOTBLK")
    (busy "EBUSY")
    (exist "EEXIST")
    (xdev "EXDEV")
    (nodev "ENODEV")
    (notdir "ENOTDIR")
    (isdir "EISDIR")
    (inval "EINVAL")
    (nfile "ENFILE")
    (mfile "EMFILE")
    (notty "ENOTTY")
    (txtbsy "ETXTBSY")
    (fbig "EFBIG")
    (nospc "ENOSPC")
    (spipe "ESPIPE")
    (rofs "EROFS")
    (mlink "EMLINK")
    (pipe "EPIPE")
    (dom "EDOM")
    (range "ERANGE")
    (again "EAGAIN")
    (wouldblock "EWOULDBLOCK")
    (inprogress "EINPROGRESS")
    (already "EALREADY")
    (notsock "ENOTSOCK")
    (destaddrreq "EDESTADDRREQ")
    (msgsize "EMSGSIZE")
    (prototype "EPROTOTYPE")
    (noprotoopt "ENOPROTOOPT")
    (protonosupport "EPROTONOSUPPORT")
    (socktnosupport "ESOCKTNOSUPPORT")
    (opnotsupp "EOPNOTSUPP")
    (pfnosupport "EPFNOSUPPORT")
    (afnosupport "EAFNOSUPPORT")
    (addrinuse "EADDRINUSE")
    (addrnotavail "EADDRNOTAVAIL")
    (netdown "ENETDOWN")
    (netunreach "ENETUNREACH")
    (netreset "ENETRESET")
    (connaborted "ECONNABORTED")
    (connreset "ECONNRESET")
    (nobufs "ENOBUFS")
    (isconn "EISCONN")
    (notconn "ENOTCONN")
    (shutdown "ESHUTDOWN")
    (toomanyrefs "ETOOMANYREFS")
    (timedout "ETIMEDOUT")
    (connrefused "ECONNREFUSED")
    (loop "ELOOP")
    (nametoolong "ENAMETOOLONG")
    (hostdown "EHOSTDOWN")
    (hostunreach "EHOSTUNREACH")
    (notempty "ENOTEMPTY")
    (proclim "EPROCLIM")
    (users "EUSERS")
    (dquot "EDQUOT")
    (stale "ESTALE")
    (remote "EREMOTE")
    (badrpc "EBADRPC")
    (rpcmismatch "ERPCMISMATCH")
    (progunavail "EPROGUNAVAIL")
    (progmismatch "EPROGMISMATCH")
    (procunavail "EPROCUNAVAIL")
    (nolck "ENOLCK")
    (nosys "ENOSYS")
    (ftype "EFTYPE")
    (auth "EAUTH")
    (needauth "ENEEDAUTH")
    (ipsec "EIPSEC")
    (noattr "ENOATTR")
    (ilseq "EILSEQ")
    (last "ELAST")
    (nomsg "ENOMSG")
    (idrm "EIDRM")
    (chrng "ECHRNG")
    (l2nsync "EL2NSYNC")
    (l3hlt "EL3HLT")
    (l3rst "EL3RST")
    (lnrng "ELNRNG")
    (unatch "EUNATCH")
    (nocsi "ENOCSI")
    (l2hlt "EL2HLT")
    (bade "EBADE")
    (badr "EBADR")
    (xfull "EXFULL")
    (noano "ENOANO")
    (badrqc "EBADRQC")
    (badslt "EBADSLT")
    (deadlock "EDEADLOCK")
    (bfont "EBFONT")
    (nostr "ENOSTR")
    (nodata "ENODATA")
    (time "ETIME")
    (nosr "ENOSR")
    (nonet "ENONET")
    (nopkg "ENOPKG")
    (nolink "ENOLINK")
    (adv "EADV")
    (srmnt "ESRMNT")
    (comm "ECOMM")
    (proto "EPROTO")
    (multihop "EMULTIHOP")
    (dotdot "EDOTDOT")
    (badmsg "EBADMSG")
    (overflow "EOVERFLOW")
    (notuniq "ENOTUNIQ")
    (badfd "EBADFD")
    (remchg "EREMCHG")
    (libacc "ELIBACC")
    (libbad "ELIBBAD")
    (libscn "ELIBSCN")
    (libmax "ELIBMAX")
    (libexec "ELIBEXEC")
    (restart "ERESTART")
    (strpipe "ESTRPIPE")
    (uclean "EUCLEAN")
    (notnam "ENOTNAM")
    (navail "ENAVAIL")
    (isnam "EISNAM")
    (remoteio "EREMOTEIO")
    (nomedium "ENOMEDIUM")
    (mediumtype "EMEDIUMTYPE")
    (canceled "ECANCELED")
    (nokey "ENOKEY")
    (keyexpired "EKEYEXPIRED")
    (keyrevoked "EKEYREVOKED")
    (keyrejected "EKEYREJECTED"))))

(define socket-types
  (find-constants-with-prefix
   'socket-type
   '((stream "SOCK_STREAM")
     (datagram "SOCK_DGRAM")
     (raw "SOCK_RAW")
     (seq-packet "SOCK_SEQPACKET")
     (rdm "SOCK_RDM"))))

(define message-flags
  (find-constants-with-prefix
   'message
   '((oob "MSG_OOB")
     (dont-route "MSG_DONTROUTE")
     (wait-all "MSG_WAITALL")
     (dont-wait "MSG_DONTWAIT")
     (no-signal "MSG_NOSIGNAL")
     (more "MSG_MORE")
     (confirm "MSG_CONFIRM"))))

(define shutdown-types
  (find-constants-with-prefix
   'shutdown
   '((receives "SHUT_RD")
     (sends "SHUT_WR")
     (sends+receives "SHUT_RDWR"))))


(define address-families
  (find-constants-with-prefix
   'address-family
   '((unspecified "AF_UNSPEC")
     (unix "AF_UNIX")
     (internet "AF_INET")
     (internet6 "AF_INET6"))))

(define protocol-families
  (find-constants-with-prefix
   'protocol-family
   '((unspecified "PF_UNSPEC")
     (unix "PF_UNIX")
     (internet "PF_INET")
     (internet6 "PF_INET6"))))

(define ip-protocols
  (find-constants-with-prefix
   'ip-protocol
   '((ip "IPPROTO_IP")
     (ipv6 "IPPROTO_IPV6")
     (tcp "IPPROTO_TCP")
     (udp "IPPROTO_UDP")
     (icmp "IPPROTO_ICMP")
     (icmpv6 "IPPROTO_ICMPV6")
     (raw "IPPROTO_RAW"))))

(define socket-levels
  (find-constants-with-prefix
   'level
   '((socket "SOL_SOCKET"))))

(define socket-options-with-types
   '((socket/debug "SO_DEBUG" boolean)
     (socket/reuse-address "SO_REUSEADDR" boolean)
     (socket/reuse-port "SO_REUSEPORT" boolean)
     (socket/type "SO_TYPE" integer)
     (socket/error "SO_ERROR" integer)
     (socket/dont-route "SO_DONTROUTE" boolean)
     (socket/broadcast "SO_BROADCAST" boolean)
     (socket/send-buffer "SO_SNDBUF" integer)
     (socket/receive-buffer "SO_RCVBUF" integer)
     (socket/keep-alive "SO_KEEPALIVE" boolean)
     (socket/oob-inline "SO_OOBINLINE" boolean)
     (socket/no-check "SO_NO_CHECK" boolean)
     (socket/priority "SO_PRIORITY" integer)
     (socket/linger "SO_LINGER" linger)
     (socket/pass-credentials "SO_PASSCRED" boolean)
     (socket/peer-credentials "SO_PEERCRED" ucred)
     (socket/receive-low-water "SO_RCVLOWAT" integer)
     (socket/send-low-water "SO_SNDLOWAT" integer)
     (socket/receive-timeout "SO_RCVTIMEO" timeout)
     (socket/send-timeout "SO_SNDTIMEO" timeout)
     
     (socket/security-authentication "SO_SECURITY_AUTHENTICATION" unused)
     (socket/security-encryption-transport "SO_SECURITY_ENCRYPTION_TRANSPORT" unused)
     (socket/security-encryption-network "SO_SECURITY_ENCRYPTION_NETWORK" unused)
     
     (socket/bind-to-device "SO_BINDTODEVICE" string)
     
     (socket/attach-filter "SO_ATTACH_FILTER" filter)
     (socket/detach-filter "SO_DETACH_FILTER" filter)
     
     (socket/peer-name "SO_PEERNAME" string)
     (socket/timestamp "SO_TIMESTAMP" boolean)
     
     (socket/accept-connect "SO_ACCEPTCONN" boolean)
     (socket/pass-security-credentials "SO_PASSSEC" boolean)
     (socket/peer-security-credentials "SO_PEERSEC" integer)
     (socket/jumbo "SO_JUMBO" boolean)
     (ip/type-of-service "IP_TOS" integer)
     (ip/time-to-live "IP_TTL" integer)
     (ip/include-header "IP_HDRINCL" boolean)
     (ip/options "IP_OPTIONS" ip-options)
     (ip/packet-info "IP_PKTINFO" packet-info)
     (ip/receive-options "IP_RECVOPTS" boolean)
     (ip/receive-response-options "IP_RECVRETOPTS" boolean)
     (ip/receive-destination-address "IP_RECVDSTADDR" boolean)
     (ip/response-options "IP_RETOPTS" ip-options)
     (ip/add-membership "IP_ADD_MEMBERSHIP" ip-mreq)
     (ip/drop-membership "IP_ADD_MEMBERSHIP" ip-mreq)
     (ip/port-range "IP_PORTRANGE" integer)
     (ip/multicast-interface "IP_MULTICAST_IF" internet-address)
     (ip/multicast-time-to-live "IP_MULTICAST_TTL" byte)
     (ip/multicast-loopback "IP_MULTICAST_LOOP" boolean-byte)
     (tcp/no-delay "TCP_NODELAY" boolean)
     (tcp/max-segment "TCP_MAXSEG" integer)
     (tcp/cork "TCP_CORK" boolean)
     (tcp/info "TCP_INFO" tcp-info)
     (tcp/defer-accept "TCP_DEFER_ACCEPT" integer)
     (tcp/keep-alive-idle "TCP_KEEPIDLE" integer)
     (tcp/keep-alive-interval "TCP_KEEPINTVL" integer)
     (tcp/syn-count "TCP_SYNCNT" integer)
     (tcp/window-clamp "TCP_WINDOW_CLAMP" integer)
     ))

(define socket-options
  (find-constants
   (map (lambda (x) (list (car x) (cadr x)))
	socket-options-with-types)))

(define option-types
  (map cdr socket-options-with-types))

(define integer-socket-options
  (map car
       (filter (lambda (x)
		 (cond
		  ((assoc (cadr x) option-types) =>
		   (lambda (y) (eq? (cadr y) 'integer)))
		  (else #f)))
	       socket-options)))

(define boolean-socket-options
  (map car
       (filter (lambda (x)
		 (cond
		  ((assoc (cadr x) option-types) =>
		   (lambda (y) (eq? (cadr y) 'boolean)))
		  (else #f)))
	       socket-options)))

(define timeout-socket-options
  (map car
       (filter (lambda (x)
		 (cond
		  ((assoc (cadr x) option-types) =>
		   (lambda (y) (eq? (cadr y) 'timeout)))
		  (else #f)))
	       socket-options)))

(define linger-socket-options
  (map car
       (filter (lambda (x)
		 (cond
		  ((assoc (cadr x) option-types) =>
		   (lambda (y) (eq? (cadr y) 'linger)))
		  (else #f)))
	       socket-options)))

(define string-socket-options
  (map car
       (filter (lambda (x)
		 (cond
		  ((assoc (cadr x) option-types) =>
		   (lambda (y) (eq? (cadr y) 'string)))
		  (else #f)))
	       socket-options)))


(let ((oport (open-output-file "gamsock-headers.scm")))
  (write-header-declarations socket-headers oport)
  (close-output-port oport))

(let ((oport (open-output-file "gamsock-constants.scm")))
  (write-constant-definitions errnos oport)
  (write-constant-definitions socket-types oport)
  (write-constant-definitions address-families oport)
  (write-constant-definitions protocol-families oport)
  (write-constant-definitions shutdown-types oport)
  (write-constant-definitions message-flags oport)
  (write-constant-definitions socket-levels oport)
  (write-constant-definitions ip-protocols oport)
  (write-constant-definitions socket-options oport)
  (write (constant-set-definition 'integer-socket-options
				  integer-socket-options)
	 oport)
  (newline oport)
  (write (constant-set-definition 'boolean-socket-options
				  boolean-socket-options)
	 oport)
  (newline oport)
  (write (constant-set-definition 'timeout-socket-options
				  timeout-socket-options)
	 oport)
  (newline oport)
  (write (constant-set-definition 'linger-socket-options
				  linger-socket-options)
	 oport)
  (newline oport)
  (close-output-port oport))