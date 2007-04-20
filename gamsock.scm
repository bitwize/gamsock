; Gamsock -- an enhanced socket library for Gambit built around the Scsh
; socket API.

; Copyright (c) 2006-2007 by Jeffrey T. Read.
; Scsh constant files copyright (c) 1993-1994 by Olin Shivers and Brian D.
; Carlstrom.

; For redistribution conditions, please see the file COPYING.
(include "gamsock-headers.scm")

(define-macro (define-c-constant var type . const)
  (let* ((const (if (not (null? const)) (car const) (symbol->string var)))
	 (str (string-append "___result = " const ";")))
    `(define ,var ((c-lambda () ,type ,str)))))

(define-macro (define-int-c-constants prefix . constants)
  (let* ((base (cond
		((string? prefix) prefix)
		((symbol? prefix) (symbol->string prefix))
		(else (error "Symbol or string required for define-enum-constants prefix")))))
    `(begin
       ,@(map (lambda (x)
		`(define-c-constant
		   ,(string->symbol
		     (string-append base "/" (symbol->string (car x))))
		   int ,(cadr x))) constants))))

(define-macro (define-enum-constants prefix . constants)
  (let* ((base (cond
		  ((string? prefix) prefix)
		  ((symbol? prefix) (symbol->string prefix))
		  (else (error "Symbol or string required for define-enum-constants prefix")))))
    `(begin
       ,@(map (lambda (x)
	       `(define ,(string->symbol 
			  (string-append base "/" (symbol->string (car x))))
		  ,(cadr x)))
	     constants))))


(include "gamsock-constants.scm")

; This is the definition of the socket type. It should be treated as opaque.

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

; This is the definition of the socket address type.

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

; This is the definition of an internal type which holds
; all of the data for an IPv6 address.

(define-type sockaddr-inet6-info
  id: 74065378-a567-ba71-0047-22b413ad9797
  type-exhibitor: macro-type-sockaddr-inet6-info
  constructor: macro-make-sockaddr-inet6-info
  implementer: implement-type-sockaddr-inet6-info
  opaque:
  macros:
  prefix: macro-
  predicate: macro-sockaddr-inet6-info?
  (host unprintable:)
  (port unprintable:)
  (flowinfo unprintable:)
  (scope-id unprintable:))

(implement-type-sockaddr-inet6-info)

(c-define (make-empty-sockaddr-inet6-info) () 
	  scheme-object
	  "make_empty_sockaddr_inet6_info"
	  "static"
	  (macro-make-sockaddr-inet6-info #f #f #f #f))

; Conversion between Scheme sockaddr records and C sockaddr structures of the appropriate
; type are done with these C helper functions:
; * build_c_sockaddr(___SCMOBJ theaddr,struct sockaddr *myaddr) -- populates a C struct
;   sockaddr_un, sockaddr_in, etc. in the area pointed to by `myaddr' with the family
;   and address data from the Scheme sockaddr referenced by `theaddr'.
; * c_sockaddr_size(struct sockaddr *myaddr) -- returns the size of the sockaddr struct.
;   This is determined by the sockaddr's family.
; * build_scheme_sockaddr(struct sockaddr *myaddr,___SCMOBJ theaddr) -- populates the
;   Scheme sockaddr record referenced by `theaddr' with family and address data from the
;   C sockaddr struct pointed to by `myaddr'.

(c-declare #<<c-declare-end

#define ___SOCKADDR_FAM(x) ___UNCHECKEDSTRUCTUREREF(x,___FIX(1L),___SUB(0),___FAL)
#define ___SOCKADDR_DATA(x) ___UNCHECKEDSTRUCTUREREF(x,___FIX(2L),___SUB(0),___FAL)

#define ___INET6_INFO_HOST(x) ___UNCHECKEDSTRUCTUREREF(x,___FIX(1L),___SUB(0),___FAL)
#define ___INET6_INFO_PORT(x) ___UNCHECKEDSTRUCTUREREF(x,___FIX(2L),___SUB(0),___FAL)
#define ___INET6_INFO_FLOWINFO(x) ___UNCHECKEDSTRUCTUREREF(x,___FIX(3L),___SUB(0),___FAL)
#define ___INET6_INFO_SCOPEID(x) ___UNCHECKEDSTRUCTUREREF(x,___FIX(4L),___SUB(0),___FAL)


int build_c_sockaddr(___SCMOBJ theaddr,struct sockaddr *myaddr)
{
  size_t len = ___CAST(size_t,___INT(___U8VECTORLENGTH(___SOCKADDR_DATA(theaddr))));
  myaddr->sa_family = ___CAST(sa_family_t,___INT(___SOCKADDR_FAM(theaddr)));
  switch(myaddr->sa_family) {
    case AF_UNSPEC:
      break;
    case AF_UNIX:
      {
        struct sockaddr_un *su = (struct sockaddr_un *)myaddr;
        ___SCMOBJ thevec = ___SOCKADDR_DATA(theaddr);
        int len = ___INT(___U8VECTORLENGTH(thevec));
        int maxlen = sizeof(su->sun_path);
        len  = (len >= maxlen ? maxlen - 1: len);
        memcpy((void *)su->sun_path,___CAST(void *,___BODY_AS(thevec,___tSUBTYPED)),len);
        su->sun_path[len] = 0;
      }
      break;
    case AF_INET:
      {
        struct sockaddr_in *si = (struct sockaddr_in *)myaddr;
        ___SCMOBJ thepr = ___SOCKADDR_DATA(theaddr);
        si->sin_port = htons(___INT(___PAIR_CAR(thepr)));
        memcpy((void *)&(si->sin_addr),___CAST(void *,___BODY_AS(___PAIR_CDR(thepr),___tSUBTYPED)),4);
      }
      break;
    case AF_INET6:
      {
        struct sockaddr_in6 *si = (struct sockaddr_in6 *)myaddr;
        ___SCMOBJ thedata = ___SOCKADDR_DATA(theaddr);
        unsigned short port = ___INT(___INET6_INFO_PORT(thedata));
        unsigned int ___temp;
        si->sin6_port = htons(port);
        si->sin6_flowinfo = htonl(___INT(___INET6_INFO_FLOWINFO(thedata)));
        si->sin6_scope_id = htonl(___U32UNBOX(___INET6_INFO_SCOPEID(thedata)));
        memcpy((void *)&(si->sin6_addr),___CAST(void *,___BODY_AS(___INET6_INFO_HOST(thedata),___tSUBTYPED)),sizeof(struct in6_addr));
      }
      break; 
  }
  return ___NO_ERR;
}

int c_sockaddr_size(struct sockaddr *myaddr) {
  switch(myaddr->sa_family) {
    case AF_UNSPEC:
      return sizeof(struct sockaddr);
    case AF_UNIX:
      return sizeof(struct sockaddr_un);
    case AF_INET:
      return sizeof(struct sockaddr_in);
    case AF_INET6:
      return sizeof(struct sockaddr_in6);
    default:
      return sizeof(struct sockaddr);
  }
}

int build_scheme_sockaddr(struct sockaddr *myaddr,___SCMOBJ theaddr)
{
  ___SCMOBJ thedata;
  switch(myaddr->sa_family)
  {
    case AF_UNIX:
      {
        struct sockaddr_un *su = (struct sockaddr_un *)myaddr;
        int len = strlen(su->sun_path);
        thedata = ___EXT(___alloc_scmobj)(___sU8VECTOR,len,___STILL);
        if(___FIXNUMP(thedata)) {
          return thedata;
        }
        memcpy(___CAST(unsigned char *,___BODY_AS(thedata,___tSUBTYPED)),myaddr->sa_data,len);
      }
      break;
    case AF_INET:
      {
        struct sockaddr_in *si = (struct sockaddr_in *)myaddr;
        thedata = ___EXT(___make_pair)(___FIX(ntohs(si->sin_port)),___EXT(___alloc_scmobj)(___sU8VECTOR,4,___STILL),___STILL);
        if(___FIXNUMP(thedata)) {
          return thedata;
        }
        memcpy(___CAST(unsigned char *,___BODY_AS(___PAIR_CDR(thedata),___tSUBTYPED)),&(si->sin_addr),4);
      }
      break;
    case AF_INET6:
      {
        struct sockaddr_in6 *si = (struct sockaddr_in6 *)myaddr;
        ___SCMOBJ thevec = ___EXT(___alloc_scmobj)(___sU8VECTOR,sizeof(struct
          in6_addr),___STILL);
        unsigned int ___temp;
         
        thedata = make_empty_sockaddr_inet6_info();
	___UNCHECKEDSTRUCTURESET(thedata,thevec,___FIX(1),___SUB(0),___FAL);
	___UNCHECKEDSTRUCTURESET(thedata,___FIX(ntohs(si->sin6_port)),___FIX(2),___SUB(0),___FAL);
	___UNCHECKEDSTRUCTURESET(thedata,___FIX(ntohl(si->sin6_flowinfo)),___FIX(3),___SUB(0),___FAL);
        ___UNCHECKEDSTRUCTURESET(thedata,___U32BOX(ntohl(si->sin6_scope_id)),___FIX(4),___SUB(0),___FAL);
        memcpy(___CAST(unsigned char *,___BODY_AS(thevec,___tSUBTYPED)),&(si->sin6_addr),sizeof(struct in6_addr));
      }
      break;
    default:
      thedata = ___NUL;
      break;
  }
  ___UNCHECKEDSTRUCTURESET(theaddr,___FIX(myaddr->sa_family),___FIX(1),___SUB(0),___FAL);
  ___UNCHECKEDSTRUCTURESET(theaddr,thedata,___FIX(2),___SUB(0),___FAL);
  return ___NO_ERR;
}

c-declare-end
)

; An exception that is raised when you try to access address information
; from a socket address of the wrong family (e.g., trying to get the IP
; address of a UNIX socket).

(define-record-type invalid-sockaddr-exception
  (make-invalid-sockaddr-exception n expected-family proc args)
  invalid-sockaddr-exception?
  (n invalid-sockaddr-argument-number)
  (expected-family invalid-sockaddr-exception-expected-family)
  (proc invalid-sockaddr-exception-procedure)
  (args invalid-sockaddr-exception-arguments))

; Socket and socket-address type predicates.

(define (socket-address? obj) (macro-sockaddr? obj))
(define (socket? obj) (macro-socket? obj))

; Returns the address family of a socket address.

(define (socket-address-family obj)
  (if (socket-address? obj)
      (macro-sockaddr-family obj)
      (##raise-type-exception 0 (macro-type-sockaddr) socket-address-family (list obj))))

(define (check-sockaddr obj fam n proc args)
  (if (not (socket-address? obj))
      (##raise-type-exception n (macro-type-sockaddr) proc args))
  (if (not (= (macro-sockaddr-family obj) fam))
      (raise (make-invalid-sockaddr-exception n fam proc args))))

(define-macro (define-sockaddr-family-pred name family)
  (define (sym-concat sym1 sym2)
    (string->symbol (string-append (symbol->string sym1) (symbol->string sym2))))
  `(define (,name a)
     (and (socket-address? a)
	  (= (macro-sockaddr-family a) ,family))))

; build_scheme_sockaddr will put the error code in place of the address data vector
; if there is a heap overflow error when allocating the vector.

(define (sockaddr-alloc-error? a)
   (integer? (macro-sockaddr-address a)))

(define (raise-if-sockaddr-alloc-error a)
  (if (sockaddr-alloc-error? a)
      (##raise-heap-overflow-exception)
      a))
; Converts a "UNIX address" or file name (Shivers named the procedure,
; not me!) to a socket address for a UNIX socket.

; This does not yet work with character encodings except ASCII/Latin-1.

(define (unix-address->socket-address fn)
  (let* (
	 (unix-path-max 108)
	 (l (string-length fn))
	 (v (make-u8vector unix-path-max 0))
	 )
    (if (>= l unix-path-max)
	(error "unix-address->socket-address: path too long" fn)
	(begin
	  (let loop ((i 0))
	    (cond
	     ((>= i l) #f)
	     (else (u8vector-set!
		    v i
		    (remainder (char->integer (string-ref fn i)) 255))
		   (loop (+ i 1)))))
	  (macro-make-sockaddr address-family/unix v)))))

; Predicate for UNIX-socket-address-ness.

(define (unix-socket-address? a)
  (and
   (socket-address? a)
   (= (macro-sockaddr-family a) address-family/unix)))

; Given a UNIX socket address, returns the "address" (file name) for the
; socket.

(define (socket-address->unix-address a)
  (check-sockaddr a address-family/unix 0 socket-address->unix-address (list a))
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

(define (check-ip4-address v)
  (let* ((e raise-not-an-ip-address))
    (if (not (and (u8vector? v) (= (u8vector-length v) 4))) (e))))

(define (check-ip6-address v)
  (let* ((e raise-not-an-ip-address))
    (if (not (and (u8vector? v) (= (u8vector-length v) 16))) (e))))

; Some important IPv4 address and port constants.

(define ip-address/any (u8vector 0 0 0 0))
(define ip-address/loopback (u8vector 127 0 0 1))
(define ip-address/broadcast (u8vector 255 255 255 255))

(define port/any 0)

; Creates a new IPv4 socket address from a host IP address
; and port number.

(define (internet-address->socket-address host port)
  (let* ((ip4a host)
	 (pv (integer->network-order-vector-16 port)))
	 
    (check-ip4-address ip4a)
    (macro-make-sockaddr address-family/internet (cons port ip4a))))

; IPv4 socket-address predicate.

(define-sockaddr-family-pred internet-socket-address? address-family/internet)

; Returns the address (host and port number as 2 values) of
; an IPv4 socket address.

(define (socket-address->internet-address a)
  (check-sockaddr a address-family/internet 0 socket-address->internet-address (list a))
  (values 
   (cdr (macro-sockaddr-address a))
   (car (macro-sockaddr-address a))))

; Creates a new IPv6 socket address from a host IP address,
; port number, flow info and scope ID.

(define (internet6-address->socket-address host port flowinfo scope-id)
  (check-ip6-address host)
  (macro-make-sockaddr
   address-family/internet6
   (macro-make-sockaddr-inet6-info host port flowinfo scope-id)))

; IPv6 socket-address predicate.

(define-sockaddr-family-pred internet6-socket-address? address-family/internet6)

; Returns the IPv6 address info associated with an IPv6
; socket address.

(define (socket-address->internet6-address a)
  (check-sockaddr a address-family/internet6 0 socket-address->internet-address (list a))
  (let* ((b (macro-sockaddr-address a)))
    (values 
     (macro-sockaddr-inet6-info-host b)
     (macro-sockaddr-inet6-info-port b)
     (macro-sockaddr-inet6-info-flowinfo b)
     (macro-sockaddr-inet6-info-scope-id b))))

; Creates a new unspecified socket address.

(define (make-unspecified-socket-address)
  (macro-make-sockaddr address-family/unspecified '()))

; Predicate to test for an unspecified socket address.

(define-sockaddr-family-pred unspecified-socket-address? address-family/unspecified)

; All socket related procedures propagate errors from the operating system
; by raising a Gambit os-exception with the errno as the exception code.
; The exceptions are EAGAIN, EWOULDBLOCK, and EINTR; all of which
; simply retry the operation until it's successful or raises another error.

(define errno (c-lambda () int "___result = errno;"))

(define (raise-socket-exception-if-error thunk proc . args)
  (let loop
      ((b (thunk)))
    (if (< b 0)
	(let* ((e (errno)))
	  (if (or
	       (= e errno/again)
	       (= e errno/wouldblock)
	       (= e errno/intr))
	      (begin
		(thread-yield!) ; to avoid tying up the CPU
		(loop (thunk)))
	      (apply
	       ##raise-os-exception
	       (append
		(list
		 #f
		 e
		 proc
		 )
		args
		))))
	  b)))

(define-macro (macro-really-make-socket fd)
  `(let* (
	  (sockobj (macro-make-socket ,fd #f)))
    (macro-socket-will-set! sockobj 
			    (make-will sockobj (lambda (s) (close-socket s))))
    sockobj))

; These are C wrappers for the socket-related system calls exposed by gamsock.
; They are kept in a private namespace "gamsock-c#". MESSING WITH THEM IS BAD
; JUJU as their exact interfaces are likely to change. Gamsock's external
; interface is likely to be stable, leaving our internals to be AS MESSY AS
; WE WANNA BE.

(namespace ("gamsock-c#" c-socket c-bind c-connect c-send c-sendto c-recvfrom
	    c-listen c-accept c-do-boolean-socket-option
	    c-do-integer-socket-option c-do-timeout-socket-option
	    c-do-boolean-set-socket-option c-do-integer-set-socket-option
	    c-do-timeout-set-socket-option c-close))
(define 
  c-socket
  (c-lambda (int int int) int #<<C-END
int s = socket(___arg1,___arg2,___arg3);
int fl = fcntl(s,F_GETFL);
fcntl(s,F_SETFL,fl | O_NONBLOCK);
___result = s;
C-END
))

(define c-bind
  (c-lambda (scheme-object scheme-object) int #<<C-END
int mysize;
struct sockaddr_storage myaddr;
build_c_sockaddr(___arg2,(struct sockaddr *)&myaddr);
mysize = c_sockaddr_size((struct sockaddr *)&myaddr);
___result = bind(___CAST(int,___INT(___UNCHECKEDSTRUCTUREREF(___arg1,___FIX(1),___SUB(0),___FAL))),(struct sockaddr *)&myaddr,mysize);
C-END
))

(define c-connect (c-lambda (scheme-object scheme-object) int #<<C-END
int mysize;
struct sockaddr_storage myaddr;
build_c_sockaddr(___arg2,(struct sockaddr *)&myaddr);
mysize = c_sockaddr_size((struct sockaddr *)&myaddr);
___result = connect(___CAST(int,
                            ___INT(___UNCHECKEDSTRUCTUREREF(___arg1,___FIX(1),
                                                            ___SUB(0),___FAL))),
                    (struct sockaddr *)&myaddr,mysize);
C-END
))
(define c-send
  (c-lambda (scheme-object scheme-object int) int #<<C-END
int soc = ___CAST(int,___INT(___UNCHECKEDSTRUCTUREREF(___arg1,___FIX(1),___SUB(0),___FAL)));
void *buf = ___CAST(void *,___BODY_AS(___arg2,___tSUBTYPED));
size_t bufsiz = ___CAST(size_t,___INT(___U8VECTORLENGTH(___arg2)));
int fl = ___CAST(int,___INT(___arg3));
___result = send(soc,buf,bufsiz,fl);
C-END
))
(define c-sendto
  (c-lambda (scheme-object scheme-object int scheme-object) int #<<C-END
struct sockaddr_storage sa;
int sa_size;
int soc = ___CAST(int,___INT(___UNCHECKEDSTRUCTUREREF(___arg1,___FIX(1),
                                                      ___SUB(0),___FAL)));
void *buf = ___CAST(void *,___BODY_AS(___arg2,___tSUBTYPED));
size_t bufsiz = ___CAST(size_t,___INT(___U8VECTORLENGTH(___arg2)));
int fl = ___CAST(int,___INT(___arg3));
build_c_sockaddr(___arg4,(struct sockaddr *)&sa);
sa_size = c_sockaddr_size((struct sockaddr *)&sa);
___result = sendto(soc,buf,bufsiz,fl,(struct sockaddr *)&sa,sa_size);
C-END
))

(define c-recvfrom
  (c-lambda (scheme-object scheme-object int scheme-object) int #<<C-END
struct sockaddr_storage sa;
socklen_t sa_size;
int soc = ___CAST(int,___INT(___UNCHECKEDSTRUCTUREREF(___arg1,___FIX(1),
                                                      ___SUB(0),___FAL)));
void *buf = ___CAST(void *,___BODY_AS(___arg2,___tSUBTYPED));
size_t bufsiz = ___CAST(size_t,___INT(___U8VECTORLENGTH(___arg2)));
int fl = ___CAST(int,___INT(___arg3));
___result = recvfrom(soc,buf,bufsiz,fl,(struct sockaddr *)&sa,&sa_size);
if(sa_size > 0) {
  build_scheme_sockaddr((struct sockaddr *)&sa,___arg4);
}
C-END
))
(define c-listen
  (c-lambda (scheme-object int) int #<<C-END
int soc = ___CAST(int,___INT(___UNCHECKEDSTRUCTUREREF(___arg1,___FIX(1),
                                                      ___SUB(0),___FAL)));
___result = listen(soc,___arg2);
C-END
))

(define c-accept
  (c-lambda (scheme-object scheme-object) int #<<C-END
struct sockaddr_storage ss;
socklen_t sslen = sizeof(struct sockaddr_storage);
int soc = ___CAST(int,___INT(___UNCHECKEDSTRUCTUREREF(___arg1,___FIX(1),___SUB(0),___FAL)));
int r = accept(soc,(struct sockaddr *)&ss,&sslen);
if(r < 0) {
   ___result = r;
}
else {
   build_scheme_sockaddr((struct sockaddr *)&ss,___arg2);
   int fl = fcntl(r,F_GETFL);
   fcntl(r,F_SETFL,fl | O_NONBLOCK);
   ___result = r;
}
C-END
))


(define c-do-boolean-socket-option
  (c-lambda (scheme-object int int scheme-object) int #<<C-END
int optval = 0;
socklen_t optlen = sizeof(optval);
int r;
int soc = ___CAST(int,___INT(___UNCHECKEDSTRUCTUREREF(___arg1,___FIX(1),___SUB(0),___FAL)));
r = getsockopt(soc,___arg2,___arg3,&optval,&optlen);
___VECTORSET(___arg4,___FIX(0L),___FIX(optval));
___result = r;
C-END
))
(define c-do-integer-socket-option
  (c-lambda (scheme-object int int scheme-object) int #<<C-END
int optval = 0;
socklen_t optlen = sizeof(optval);
int r;
int soc = ___CAST(int,___INT(___UNCHECKEDSTRUCTUREREF(___arg1,___FIX(1),___SUB(0),___FAL)));
r = getsockopt(soc,___arg2,___arg3,&optval,&optlen);
___VECTORSET(___arg4,___FIX(0L),___FIX(optval));
___result = r;
C-END
))

(define c-do-timeout-socket-option
 (c-lambda (scheme-object int int scheme-object) int #<<C-END
struct timeval optval;
socklen_t optlen = sizeof(optval);
int r;
int soc = ___CAST(int,___INT(___UNCHECKEDSTRUCTUREREF(___arg1,___FIX(1),___SUB(0),___FAL)));
r = getsockopt(soc,___arg2,___arg3,&optval,&optlen);
___VECTORSET(___arg4,___FIX(0L),___FIX(optval.tv_sec));
___VECTORSET(___arg4,___FIX(1L),___FIX(optval.tv_usec));
___result = r;
C-END
))

(define c-do-boolean-set-socket-option
 (c-lambda (scheme-object int int scheme-object) int #<<C-END
int optval = 0;
socklen_t optlen = sizeof(optval);
int r;
int soc = ___CAST(int,___INT(___UNCHECKEDSTRUCTUREREF(___arg1,___FIX(1),___SUB(0),___FAL)));
if(___arg4 != ___FAL)
{
  optval = 1;
}
else
{
 optval = 0;
}
r = setsockopt(soc,___arg2,___arg3,&optval,optlen);
___result = r;
C-END
))

(define c-do-integer-set-socket-option
  (c-lambda (scheme-object int int int) int #<<C-END
int optval = ___arg4;
socklen_t optlen = sizeof(optval);
int r;
int soc = ___CAST(int,___INT(___UNCHECKEDSTRUCTUREREF(___arg1,___FIX(1),___SUB(0),___FAL)));
r = setsockopt(soc,___arg2,___arg3,&optval,optlen);
___result = r;
C-END
))

(define c-do-timeout-set-socket-option
  (c-lambda (scheme-object int int int int) int #<<C-END
struct timeval optval;
socklen_t optlen = sizeof(optval);
int r;
int soc = ___CAST(int,___INT(___UNCHECKEDSTRUCTUREREF(___arg1,___FIX(1),___SUB(0),___FAL)));
optval.tv_sec = ___arg4;
optval.tv_usec = ___arg5;
r = setsockopt(soc,___arg2,___arg3,&optval,optlen);
___result = r;
C-END
))

(define c-close
  (c-lambda (int) int "___result = close(___arg1);"))


; GAMSOCK API begins here.
; Closes an open socket.

(define (close-socket sock)
    (c-close (macro-socket-fd sock)))

; Creates a new socket of the specified domain (protocol family),
; type (e.g., stream, datagram), and optional protocol.

(define (create-socket domain type #!optional (protocol 0))
  (macro-really-make-socket
   (raise-socket-exception-if-error
    (lambda () (c-socket domain type protocol))
    create-socket)))

; Binds a socket to a local address.

(define (bind-socket sock addr)
    (if (not (socket? sock))
	(##raise-type-exception 
	 0 (macro-type-socket) bind-socket (list sock addr))
	(if (not (socket-address? addr))
	    (##raise-type-exception
	     1 (macro-type-sockaddr) bind-socket (list sock addr))
	    (raise-socket-exception-if-error
	     (lambda () (c-bind sock addr)) bind-socket)))
    (if #f #f))

; Connects a socket to a remote address.

(define (connect-socket sock addr)

    (if (not (socket? sock))
	(##raise-type-exception 0 (macro-type-socket) connect-socket (list sock addr))
	(if (not (socket-address? addr))
	    (##raise-type-exception 1 (macro-type-sockaddr) connect-socket (list sock addr))
	    (raise-socket-exception-if-error (lambda () (c-connect sock addr)) connect-socket)))
    (if #f #f))

; Sends a message on a socket. The message must be a u8vector or, if
; start and end parameters are given, a slice of the u8vector bound by
; the start and end params.

; Optional flags and a destination address may also be specified; the latter
; is only useful for connectionless sockets (e.g., UDP/IP).

(define (send-message sock vec #!optional (start 0) (end #f) (flags 0)
		      (addr #f))
  (let ((svec (if (and (= start 0) (not end)) vec
		   (subu8vector vec
				start 
				(if (not end) (u8vector-length vec) end)))))

    (if (not (socket? sock))
	(##raise-type-exception 0 (macro-type-socket) send-message (list sock vec start end flags addr)))
    (if (not (u8vector? vec))
	(##raise-type-exception 1 'u8vector send-message (list sock vec start end flags addr)))
    (if (not addr)
	(raise-socket-exception-if-error (lambda () (c-send sock svec flags)) send-message)
	(if (not (socket-address? addr))
	    (##raise-type-exception 
	     3 (macro-type-sockaddr) send-message
	     (list sock vec start end flags addr))
	    (raise-socket-exception-if-error
	     (lambda () (c-sendto sock svec flags addr)) send-message)))))

; Receives a message from a socket of a given length and returns it as a
; u8vector. Optional flags may be specified. This procedure actually returns
; two values: the received message and the source address.

(define (receive-message sock len #!optional (flags 0))
  (let ((addr (make-unspecified-socket-address))
	 (vec (make-u8vector len 0)))
    (if (not (socket? sock))
	(##raise-type-exception 
	 0 (macro-type-socket) receive-message (list sock len flags)))
    (let* ((size-actually-recvd
	    (raise-socket-exception-if-error
	     (lambda () (c-recvfrom sock vec flags addr)) receive-message)))
      (values
       (subu8vector vec 0 size-actually-recvd)
       addr))))

; Sets up a socket to listen for incoming connections, with the specified number
; of backlogged connections allowed.

(define (listen-socket sock backlog)
  (if (not (socket? sock))
      (##raise-type-exception
       0 (macro-type-socket) listen-socket (list sock backlog)))
  (raise-socket-exception-if-error
   (lambda () (c-listen sock backlog)) listen-socket)
  (if #f #f)
  )

; Returns the local socket address of the socket.

(define (socket-local-address sock) 
  (let* (
	 (dummy-sockaddr (macro-make-sockaddr 0 #f))
	 (c-getsockname
	  (c-lambda (scheme-object scheme-object) int
		    "
struct sockaddr_storage ss;
socklen_t sslen = sizeof(struct sockaddr_storage);
int soc = ___CAST(int,___INT(___UNCHECKEDSTRUCTUREREF(___arg1,___FIX(1),___SUB(0),___FAL)));
int r = getsockname(soc,(struct sockaddr *)&ss,&sslen);
if(r<0) {
   ___result = r;
}
else {
   build_scheme_sockaddr((struct sockaddr *)&ss,___arg2);
   ___result = r;
}
")))
    (if (not (socket? sock))
	(##raise-type-exception 0 (macro-type-socket) socket-local-address (list sock)))
    (raise-socket-exception-if-error (lambda () 
				       (c-getsockname sock dummy-sockaddr)) socket-local-address)
    (raise-if-sockaddr-alloc-error dummy-sockaddr)))

; Returns the remote socket address of a socket.

(define (socket-remote-address sock) 
  (let* (
	 (dummy-sockaddr (macro-make-sockaddr 0 #f))
	 (c-getpeername
	  (c-lambda (scheme-object scheme-object) int
		    "
struct sockaddr_storage ss;
socklen_t sslen = sizeof(struct sockaddr_storage);
int soc = ___CAST(int,___INT(___UNCHECKEDSTRUCTUREREF(___arg1,___FIX(1),___SUB(0),___FAL)));
int r = getpeername(soc,(struct sockaddr *)&ss,&sslen);
if(r<0) {
   ___result = r;
}
else {
   build_scheme_sockaddr((struct sockaddr *)&ss,___arg2);
   ___result = r;
}
")))
    (if (not (socket? sock))
	(##raise-type-exception 0 (macro-type-socket) socket-remote-address (list sock)))
    (raise-socket-exception-if-error (lambda () 
				       (c-getpeername sock dummy-sockaddr)) socket-remote-address)
    (raise-if-sockaddr-alloc-error dummy-sockaddr)))

; Accepts a connection on a socket. Returns two values: a new socket corresponding to
; the connection, and the address of the other side of the connection.

(define (accept-connection sock) 
  (let ((dummy-sockaddr (macro-make-sockaddr 0 #f)))
    (if (not (socket? sock))
	(##raise-type-exception
	 0 (macro-type-socket) accept-connection (list sock)))
    (let ((s2 
	   (raise-socket-exception-if-error
	    (lambda () (c-accept sock dummy-sockaddr)) accept-connection)))
      (raise-if-sockaddr-alloc-error dummy-sockaddr)
      (values (macro-really-make-socket s2) dummy-sockaddr))))

(define (boolean-socket-option? optname)
  (member optname boolean-socket-options))
(define (integer-socket-option? optname)
  (member optname integer-socket-options))
(define (timeout-socket-option? optname)
  (member optname timeout-socket-options))

; ### Socket Option Getters ###

(define (do-boolean-socket-option socket level optname)
  (let ((v (make-vector 1)))
    (if (not (socket? socket))
	(##raise-type-exception 0
				(macro-type-socket)
				socket-option
				(list socket level optname)))
    (if (not (integer? level))
	(##raise-type-exception 1
				'integer
				socket-option
				(list socket level optname)))
    (if (not (integer? optname))
	(##raise-type-exception 2
				'integer
				socket-option
				(list socket level optname)))
    (raise-socket-exception-if-error
     (lambda () (c-do-boolean-socket-option socket
					    level
					    optname
					    v)) socket-option)
    (not (zero? (vector-ref v 0)))))

(define (do-integer-socket-option socket level optname)
  (let ((v (make-vector 1)))
    (if (not (socket? socket))
	(##raise-type-exception 0
				(macro-type-socket)
				socket-option
				(list socket level optname)))
    (if (not (integer? level))
	(##raise-type-exception 1
				'integer
				socket-option
				(list socket level optname)))
    (if (not (integer? optname))
	(##raise-type-exception 2
				'integer
				socket-option
				(list socket level optname)))
    (raise-socket-exception-if-error
     (lambda () (c-do-integer-socket-option socket
					    level
					    optname
					    v)) socket-option)
    (vector-ref v 0)))

(define (do-timeout-socket-option socket level optname)
  (let ((v (make-vector 2)))
    (if (not (socket? socket))
	(##raise-type-exception 0
				(macro-type-socket)
				socket-option
				(list socket level optname)))
    (if (not (integer? level))
	(##raise-type-exception 1
				'integer
				socket-option
				(list socket level optname)))
    (if (not (integer? optname))
	(##raise-type-exception 2
				'integer
				socket-option
				(list socket level optname)))
    (raise-socket-exception-if-error
     (lambda () (c-do-timeout-socket-option socket
					    level
					    optname
					    v)) socket-option)
    (+
     (vector-ref v 0)
     (/ (vector-ref v 1) 1000000.0))))

(define (socket-option socket level optname)
  (cond
   ((boolean-socket-option? optname)
    (do-boolean-socket-option socket level optname))
   ((integer-socket-option? optname)
    (do-integer-socket-option socket level optname))
   ((timeout-socket-option? optname)
    (do-timeout-socket-option socket level optname))
   (else
    (error "unsupported socket option"))))

; ### Socket option setters ###
(define (do-boolean-set-socket-option socket level optname optval)
    (if (not (socket? socket))
	(##raise-type-exception 0
				(macro-type-socket)
				socket-option
				(list socket level optname optval)))
    (if (not (integer? level))
	(##raise-type-exception 1
				'integer
				socket-option
				(list socket level optname optval)))
    (if (not (integer? optname))
	(##raise-type-exception 2
				'integer
				socket-option
				(list socket level optname optval)))
    (raise-socket-exception-if-error
     (lambda () (c-do-boolean-set-socket-option socket
					    level
					    optname
					    optval)) socket-option)
    #!void)

(define (do-integer-set-socket-option socket level optname optval)
    (if (not (socket? socket))
	(##raise-type-exception 0
				(macro-type-socket)
				socket-option
				(list socket level optname optval)))
    (if (not (integer? level))
	(##raise-type-exception 1
				'integer
				socket-option
				(list socket level optname optval)))
    (if (not (integer? optname))
	(##raise-type-exception 2
				'integer
				socket-option
				(list socket level optname optval)))
    (if (not (integer? optval))
	(##raise-type-exception 3
				'integer
				socket-option
				(list socket level optname optval)))
    (raise-socket-exception-if-error
     (lambda () (c-do-integer-set-socket-option socket
					    level
					    optname
					    optval)) socket-option)
    #!void)

(define (do-timeout-set-socket-option socket level optname optval)
    (if (not (socket? socket))
	(##raise-type-exception 0
				(macro-type-socket)
				socket-option
				(list socket level optname optval)))
    (if (not (integer? level))
	(##raise-type-exception 1
				'integer
				socket-option
				(list socket level optname optval)))
    (if (not (integer? optname))
	(##raise-type-exception 2
				'integer
				socket-option
				(list socket level optname optval)))
    (if (not (real? optval))
	(##raise-type-exception 3
				'integer
				socket-option
				(list socket level optname optval)))
    (let* ((sec (inexact->exact (truncate optval)))
	   (usec (inexact->exact (truncate (* (- optval sec) 1000000.)))))
      (raise-socket-exception-if-error
       (lambda () (c-do-timeout-set-socket-option socket
						  level
						  optname
						  sec
					    usec)) socket-option))
    #!void)

(define (set-socket-option socket level optname optval)
  (cond
   ((boolean-socket-option? optname)
    (do-boolean-set-socket-option socket level optname optval))
   ((integer-socket-option? optname)
    (do-integer-set-socket-option socket level optname optval))
   ((timeout-socket-option? optname)
    (do-timeout-set-socket-option socket level optname optval))
   (else
    (error "unsupported socket option"))))

(namespace (""))