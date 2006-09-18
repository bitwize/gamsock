(c-declare "#include <string.h>")
(c-declare "#include <stdlib.h>")
(c-declare "#include <unistd.h>")
(c-declare "#include <sys/types.h>")
(c-declare "#include <sys/socket.h>")
(c-declare "#include <netinet/in.h>")
(c-declare "#include <sys/un.h>")
(c-declare "#include <errno.h>")

(define-macro (define-c-constant var type . const)
  (let* ((const (if (not (null? const)) (car const) (symbol->string var)))
	 (str (string-append "___result = " const ";")))
    `(define ,var ((c-lambda () ,type ,str)))))

(define-macro (define-int-c-constants . var-list)
  `(begin
     ,@(map (lambda (x) `(define-c-constant ,x int)) var-list)))

; Address families.

(define-int-c-constants AF_UNSPEC AF_LOCAL AF_UNIX AF_INET AF_INET6)

; Protocol families.

(define-int-c-constants PF_LOCAL PF_UNIX PF_INET PF_INET6 PF_PACKET)

; Socket types.

(define-int-c-constants SOCK_STREAM SOCK_DGRAM SOCK_RAW SOCK_SEQPACKET)

; Possible error conditions.

(define-int-c-constants EACCES EADDRINUSE EAGAIN EBADF EINVAL ENOTSOCK EFAULT ELOOP ENAMETOOLONG
  ENOENT ENOMEM ENOTDIR EROFS EISCONN EINPROGRESS EINTR ENETUNREACH ETIMEDOUT EALREADY
  EAFNOSUPPORT ENOTCONN EMSGSIZE ECONNRESET EDESTADDRREQ EWOULDBLOCK EOPNOTSUPP EPIPE
  EPROTONOSUPPORT ENOBUFS ECONNREFUSED EMFILE ENFILE ECONNABORTED EPROTO EPERM)

; Send and recv flags.

(define-int-c-constants MSG_ERRQUEUE MSG_OOB MSG_PEEK MSG_TRUNC MSG_WAITALL MSG_DONTWAIT
  MSG_DONTROUTE MSG_EOR MSG_MORE MSG_NOSIGNAL MSG_CONFIRM)

; Socket address sizes.

(define-c-constant *sockaddr-un-len* int "sizeof(struct sockaddr_un)")
(define-c-constant *sockaddr-in-len* int "sizeof(struct sockaddr_in)")
(define-c-constant *sockaddr-in6-len* int "sizeof(struct sockaddr_in6)")

(define-type socket
  id: 98e94265-558a-d985-b3fe-e67f32458c35
  type-exhibitor: macro-type-socket
  constructor: macro-make-socket
  implementer: implement-type-socket
  opaque:
  macros:
  prefix: macro-
  predicate: macro-socket?
  (fd unprintable:)
  (will unprintable:))

(implement-type-socket)

(define-type sockaddr
  id: ce56103c-c098-5996-21e1-7d200e7e4e6f
  type-exhibitor: macro-type-sockaddr
  constructor: macro-make-sockaddr
  implementer: implement-type-sockaddr
  opaque:
  macros:
  prefix: macro-
  predicate: macro-sockaddr?
  (family unprintable:)
  (address unprintable:))

(implement-type-sockaddr)

(c-declare #<<c-declare-end

#define ___SOCKADDR_FAM(x) ___UNCHECKEDSTRUCTUREREF(x,___FIX(1),___SUB(0),___FAL)
#define ___SOCKADDR_DATA(x) ___UNCHECKEDSTRUCTUREREF(x,___FIX(2),___SUB(0),___FAL)

int build_c_sockaddr(___SCMOBJ theaddr,struct sockaddr *myaddr)
{
  size_t len = ___CAST(size_t,___INT(___U8VECTORLENGTH(___SOCKADDR_DATA(theaddr))));
  myaddr->sa_family = ___CAST(sa_family_t,___INT(___SOCKADDR_FAM(theaddr)));
  memcpy(myaddr->sa_data,___CAST(unsigned char *,___BODY_AS(___SOCKADDR_DATA(theaddr),___tSUBTYPED)),len);  
  return ___NO_ERR;
}

size_t c_sockaddr_size(___SCMOBJ theaddr) {
  return ___CAST(size_t,___INT(___U8VECTORLENGTH(___SOCKADDR_DATA(theaddr))) + sizeof(sa_family_t));
}

int build_scheme_sockaddr(struct sockaddr *myaddr,___SCMOBJ theaddr,int addr_size)
{
  ___SCMOBJ thevec;
  thevec = ___EXT(___alloc_scmobj)(___sU8VECTOR,addr_size - sizeof(sa_family_t),___STILL);
  if(___FIXNUMP(thevec)) {return thevec;}
  memcpy(___CAST(unsigned char *,___BODY_AS(thevec,___tSUBTYPED)),myaddr->sa_data,addr_size - sizeof(sa_family_t));
  ___UNCHECKEDSTRUCTURESET(theaddr,___FIX(myaddr->sa_family),___FIX(1),___SUB(0),___FAL);
  ___UNCHECKEDSTRUCTURESET(theaddr,thevec,___FIX(2),___SUB(0),___FAL);
  return ___NO_ERR;
}

c-declare-end
)



(define-record-type invalid-sockaddr-exception
  (make-invalid-sockaddr-exception n expected-family proc args)
  invalid-sockaddr-exception?
  (n invalid-sockaddr-argument-number)
  (expected-family invalid-sockaddr-exception-expected-family)
  (proc invalid-sockaddr-exception-procedure)
  (args invalid-sockaddr-exception-arguments))

(define (sockaddr? obj) (macro-sockaddr? obj))
(define (socket? obj) (macro-socket? obj))

(define (sockaddr-family obj) (macro-sockaddr-family obj))

(define (check-sockaddr obj fam n proc args)
  (if (not (sockaddr? obj))
      (##raise-type-exception n 'sockaddr proc args)
      )
  (if (not (= (macro-sockaddr-family obj) fam))
      (raise (make-invalid-sockaddr-exception n fam proc args))))

(define-macro (define-sockaddr-family-pred name family)
  (define (sym-concat sym1 sym2)
    (string->symbol (string-append (symbol->string sym1) (symbol->string sym2))))
  `(define (,name a)
     (and (sockaddr? a)
	  (= (macro-sockaddr-family a) ,family))))

; build_scheme_sockaddr will put the error code in place of the address data vector
; if there is a heap overflow error when allocating the vector.

(define (sockaddr-alloc-error? a)
   (integer? (macro-sockaddr-address a)))

(define (raise-if-sockaddr-alloc-error a)
  (if (sockaddr-alloc-error? a)
      (##raise-heap-overflow-exception)
      a))

(define (make-local-sockaddr fn)
  (let* (
	 (unix-path-max (- *sockaddr-un-len* 2))
	 (l (min (string-length fn) (- unix-path-max 1)))
	 (v (make-u8vector unix-path-max 0))
	 )
    (let loop ((i 0))
      (cond
       ((>= i l) #f)
       (else (u8vector-set! v i (remainder (char->integer (string-ref fn i)) 255))
	     (loop (+ i 1)))))
    (macro-make-sockaddr AF_LOCAL v)))

(define (local-sockaddr? a)
  (and
   (sockaddr? a)
   (= (macro-sockaddr-family a) AF_LOCAL)))

(define (local-sockaddr-path a)
  (check-sockaddr a AF_LOCAL 0 local-sockaddr-path (list a))
  (let* ((v (macro-sockaddr-address a))
	(l (u8vector-length v))
	(s (make-string l #\nul)))
    (let loop ((i 0))
      (if (and (< i l)
	       (not (zero? (u8vector-ref v i))))
	  (begin (string-set! s i (integer->char (u8vector-ref v i)))
		 (loop (+ i 1)))
	  (substring s 0 i)))))

(define (integer->network-order-vector-16 n)
  (u8vector
   (bitwise-and (arithmetic-shift n -8) 255)
   (bitwise-and n 255)))

(define (integer->network-order-vector-32 n)
  (u8vector
   (bitwise-and (arithmetic-shift n -24) 255)
   (bitwise-and (arithmetic-shift n -16) 255)
   (bitwise-and (arithmetic-shift n -8) 255)
   (bitwise-and n 255)))

(define (network-order-vector->integer-16 v)
  (bitwise-ior
   (arithmetic-shift (u8vector-ref v 0) 8)
   (u8vector-ref v 1)))

(define (network-order-vector->integer-32 v)
  (bitwise-ior
   (arithmetic-shift (u8vector-ref v 0) 24)
   (arithmetic-shift (u8vector-ref v 1) 16)
   (arithmetic-shift (u8vector-ref v 2) 8)
   (u8vector-ref v 3)))

(define (raise-not-an-ip-address)
  (error "not an ip address"))

(define (consume-ip4-address-component str ptr)
  (let ((l (string-length str)))
    (let loop
	((p ptr)
	 (s '()))
      (if (>= p l)
	  (values (list->string (reverse s)) #f)
	  (let ((c (string-ref str p)))
	    (cond
	     ((char-numeric? c) (loop (+ p 1) (cons c s)))
	     ((char=? c #\.) (values (list->string (reverse s)) p))
	     (else (raise-not-an-ip-address))))))))

(define (string->ip4-address str)
  (let* ((ccheck
	  (lambda (x)
	    (let* ((r (string->number x)))
	      (if (or (not r) (not (integer? r)) (not (exact? r))
		      (> r 255) (< r 0))
		  (raise-not-an-ip-address)
		  r)))))
    (let loop
	((ptr 0)
	 (parts '())
	 (count 0))
      (call-with-values (lambda () (consume-ip4-address-component str ptr))
	(lambda (next nptr)
	  (cond
	   ((and nptr (or (>= (+ nptr 1) (string-length str)) 
			  (not (char-numeric? (string-ref str (+ nptr 1))))))
	    (raise-not-an-ip-address))
	   ((and (< count 3) nptr)
	    (loop (+ nptr 1) (cons (ccheck next) parts) (+ count 1)))
	   ((and (= count 3) (not nptr))
	    (apply u8vector (reverse (cons (ccheck next) parts))))
	   (else (raise-not-an-ip-address))))))))

(define (check-ip4-address v)
  (let* ((e raise-not-an-ip-address))
    (if (not (and (u8vector? v) (= (u8vector-length v) 4))) (e)
	(let loop ((i 0))
	  (cond ((< i (u8vector-length v))
		 (let ((r (u8vector-ref v i)))
		    (if (or (not (integer? r)) (not (exact? r)) (> r 255) (< r 0))
			(e)
			(loop (+ i 1))))))))))

(define *ip4-address-any* (u8vector 0 0 0 0))
(define *ip4-address-loopback* (u8vector 127 0 0 1))

(define (make-inet-sockaddr host port)
  (let* ((ip4a (cond
		((u8vector? host) host)
		((string? host) (string->ip4-address host))))
	 (pv (integer->network-order-vector-16 port)))
	 
    (check-ip4-address ip4a)
    (macro-make-sockaddr AF_INET (u8vector-append
				  pv ip4a
				  (make-u8vector
				   (- (- *sockaddr-in-len* 2)
				      (+ (u8vector-length pv)
					    (u8vector-length ip4a)))
				   0)))))

(define-sockaddr-family-pred inet-sockaddr? AF_INET)

(define (inet-sockaddr-ip-address a)
  (check-sockaddr a AF_INET 0 inet-sockaddr-ip-address (list a))
  (subu8vector (macro-sockaddr-address a) 2 6))

(define (inet-sockaddr-port a)
  (check-sockaddr a AF_INET 0 inet-sockaddr-port (list a))
  (network-order-vector->integer-16 (subu8vector (macro-sockaddr-address a) 0 2)))

(define (make-unspecified-sockaddr)
  (macro-make-sockaddr AF_UNSPEC (make-u8vector 14 0)))

(define-sockaddr-family-pred unspecified-sockaddr? AF_UNSPEC)

(define (close sock)
  (let* ( (c-close (c-lambda (int) int
			    "___result = close(___arg1);")))
    (c-close (macro-socket-fd sock))))

(define errno (c-lambda () int "___result = errno;"))


(define (make-flags lst fl)
  (let loop ((l lst)
	     (n 0))
      (cond
       ((null? l) n)
       (else
	(let* ((c (assoc (car l) fl)))
	  (cond
	   ((not c) (loop (cdr l) n))
	   (else (loop (cdr l) (bitwise-ior n (cadr c))))))))))

(define (make-enum int-or-sym en)
  (if (integer? int-or-sym)
      int-or-sym
      (let ((a (assoc int-or-sym en)))
	(if a
	    (cadr a)
	    (error "invalid enum symbol")))))



(define (raise-socket-exception-if-error thunk proc . args)
  (let ((b (thunk)))
    (if (< b 0)
	(let* ((e (errno)))
	  (apply
	   ##raise-os-exception
	   (append
	    (list
	     #f
	     e
	     proc
	     )
	   args
	   )))
	b)))

(define-macro (macro-really-make-socket fd)
  `(let* (
	  (sockobj (macro-make-socket ,fd #f)))
    (macro-socket-will-set! sockobj 
			    (make-will sockobj (lambda (s) (close s))))
    sockobj))

(define (socket domain type protocol)
  (let* (
	 (domain (make-enum domain `((unix ,PF_UNIX) (local ,PF_LOCAL) (inet ,PF_INET) 
				     (inet6 ,PF_INET6) (packet ,PF_PACKET))))
	 (type (make-enum type `((stream ,SOCK_STREAM) (dgram ,SOCK_DGRAM) (raw ,SOCK_RAW)
				 (seqpacket ,SOCK_SEQPACKET))))
	 (c-socket (c-lambda (int int int) int
			     "___result = socket(___arg1,___arg2,___arg3);"))
	 (c-close (c-lambda (int) int
			    "___result = close(___arg1);"))
	 (sockobj (macro-really-make-socket (raise-socket-exception-if-error
				      (lambda () (c-socket domain type protocol)) socket))))
    sockobj))


(define (bind sock addr)
  (let* ((c-bind (c-lambda (scheme-object scheme-object) int
"
size_t mysize = c_sockaddr_size(___arg2);
struct sockaddr *myaddr = (struct sockaddr *)(malloc(mysize));
if(myaddr != NULL) {
  build_c_sockaddr(___arg2,myaddr);
  ___result = bind(___CAST(int,___INT(___UNCHECKEDSTRUCTUREREF(___arg1,___FIX(1),___SUB(0),___FAL))),myaddr,mysize);
}
else {
  ___result = -1;
}
#define ___AT_END if(myaddr != NULL) { free(myaddr); }
"
)))
    (if (not (socket? sock))
	(##raise-type-exception 0 'socket bind (list sock addr))
	(if (not (sockaddr? addr))
	    (##raise-type-exception 1 'sockaddr bind (list sock addr))
	    (raise-socket-exception-if-error (lambda () (c-bind sock addr)) bind)))))

(define (connect sock addr)
  (let* ((c-connect (c-lambda (scheme-object scheme-object) int
"
size_t mysize = c_sockaddr_size(___arg2);
struct sockaddr *myaddr = (struct sockaddr *)(malloc(mysize));
if(myaddr != NULL) {
  build_c_sockaddr(___arg2,myaddr);
  ___result = connect(___CAST(int,___INT(___UNCHECKEDSTRUCTUREREF(___arg1,___FIX(1),___SUB(0),___FAL))),myaddr,mysize);
}
else {
  ___result = -1;
}
#define ___AT_END if(myaddr != NULL) { free(myaddr); }
"
)))
    (if (not (socket? sock))
	(##raise-type-exception 0 'socket connect (list sock addr))
	(if (not (sockaddr? addr))
	    (##raise-type-exception 1 'sockaddr connect (list sock addr))
	    (raise-socket-exception-if-error (lambda () (c-connect sock addr)) connect)))))



(define (send sock vec flags)
  (let* ((nf (if (number? flags)
		 flags
		 (make-flags flags `((confirm ,MSG_CONFIRM)
				 (dont-route ,MSG_DONTROUTE)
				 (dont-wait ,MSG_DONTWAIT)
				 (end-of-record ,MSG_EOR)
				 (more-data ,MSG_MORE)
				 (no-signal ,MSG_NOSIGNAL)
				 (oob ,MSG_OOB)))))
	 (c-send
	  (c-lambda (scheme-object scheme-object int) int
		    "
int soc = ___CAST(int,___INT(___UNCHECKEDSTRUCTUREREF(___arg1,___FIX(1),___SUB(0),___FAL)));
void *buf = ___CAST(void *,___BODY_AS(___arg2,___tSUBTYPED));
size_t bufsiz = ___CAST(size_t,___INT(___U8VECTORLENGTH(___arg2)));
int fl = ___CAST(int,___INT(___arg3));
___result = send(soc,buf,bufsiz,fl);
")))
    (if (not (socket? sock))
	(##raise-type-exception 0 'socket send (list sock vec flags)))
    (if (not (u8vector? vec))
	(##raise-type-exception 1 'u8vector send (list sock vec flags)))
    (raise-socket-exception-if-error (lambda () (c-send sock vec nf)) send)))


(define (recv sock len flags)
  (let* ((nf (if (number? flags) 
		 flags
		 (make-flags flags `((err-queue ,MSG_ERRQUEUE)
				     (oob ,MSG_OOB)
				     (peek ,MSG_PEEK)
				     (trunc ,MSG_TRUNC)
				     (waitall ,MSG_WAITALL)))))
	 (vec (make-u8vector len 0))
	 (c-recv
	  (c-lambda (scheme-object scheme-object int) int
		    "
int soc = ___CAST(int,___INT(___UNCHECKEDSTRUCTUREREF(___arg1,___FIX(1),___SUB(0),___FAL)));
void *buf = ___CAST(void *,___BODY_AS(___arg2,___tSUBTYPED));
size_t bufsiz = ___CAST(size_t,___INT(___U8VECTORLENGTH(___arg2)));
int fl = ___CAST(int,___INT(___arg3));
___result = recv(soc,buf,bufsiz,fl);
")))
    (if (not (socket? sock))
	(##raise-type-exception 0 'socket recv (list sock len flags)))
    (let* ((size-actually-recvd
	    (raise-socket-exception-if-error (lambda () (c-recv sock vec nf)) recv)))
      (subu8vector vec 0 size-actually-recvd))))

(define (listen sock backlog)
  (let* ((c-listen
	  (c-lambda (scheme-object int) int
		    "
int soc = ___CAST(int,___INT(___UNCHECKEDSTRUCTUREREF(___arg1,___FIX(1),___SUB(0),___FAL)));
___result = listen(soc,___arg2);
")))
    (if (not (socket? sock))
	(##raise-type-exception 0 'socket listen (list sock backlog)))
    (raise-socket-exception-if-error (lambda () (c-listen sock backlog)) listen)
    ))

(define (getsockname sock) 
  (let* (
	 (dummy-sockaddr (macro-make-sockaddr 0 #f))
	 (c-getsockname
	  (c-lambda (scheme-object scheme-object) int
		    "
struct sockaddr_storage ss;
int sslen = sizeof(struct sockaddr_storage);
int soc = ___CAST(int,___INT(___UNCHECKEDSTRUCTUREREF(___arg1,___FIX(1),___SUB(0),___FAL)));
int r = getsockname(soc,(struct sockaddr *)&ss,&sslen);
if(r<0) {
   ___result = r;
}
else {
   build_scheme_sockaddr((struct sockaddr *)&ss,___arg2,sslen);
   ___result = r;
}
")))
    (if (not (socket? sock))
	(##raise-type-exception 0 'socket getsockname (list sock)))
    (raise-socket-exception-if-error (lambda () 
				       (c-getsockname sock dummy-sockaddr)) getsockname)
    (raise-if-sockaddr-alloc-error dummy-sockaddr)))

(define (getpeername sock) 
  (let* (
	 (dummy-sockaddr (macro-make-sockaddr 0 #f))
	 (c-getpeername
	  (c-lambda (scheme-object scheme-object) int
		    "
struct sockaddr_storage ss;
int sslen = sizeof(struct sockaddr_storage);
int soc = ___CAST(int,___INT(___UNCHECKEDSTRUCTUREREF(___arg1,___FIX(1),___SUB(0),___FAL)));
int r = getpeername(soc,(struct sockaddr *)&ss,&sslen);
if(r<0) {
   ___result = r;
}
else {
   build_scheme_sockaddr((struct sockaddr *)&ss,___arg2,sslen);
   ___result = r;
}
")))
    (if (not (socket? sock))
	(##raise-type-exception 0 'socket getpeername (list sock)))
    (raise-socket-exception-if-error (lambda () 
				       (c-getpeername sock dummy-sockaddr)) getpeername)
    (raise-if-sockaddr-alloc-error dummy-sockaddr)))

(define (accept sock) 
  (let* (
	 (dummy-sockaddr (macro-make-sockaddr 0 #f))
	 (c-accept
	  (c-lambda (scheme-object scheme-object) int
		    "
struct sockaddr_storage ss;
int sslen = sizeof(struct sockaddr_storage);
int soc = ___CAST(int,___INT(___UNCHECKEDSTRUCTUREREF(___arg1,___FIX(1),___SUB(0),___FAL)));
int r = accept(soc,(struct sockaddr *)&ss,&sslen);
if(r < 0) {
   ___result = r;
}
else {
   build_scheme_sockaddr((struct sockaddr *)&ss,___arg2,sslen);
   ___result = r;
}
")))
    (if (not (socket? sock))
	(##raise-type-exception 0 'socket getpeername (list sock)))
    (let* ((s2 
	    (raise-socket-exception-if-error (lambda () (c-accept sock dummy-sockaddr)) accept)))
      (raise-if-sockaddr-alloc-error dummy-sockaddr)
      (values (macro-really-make-socket s2) dummy-sockaddr))))

