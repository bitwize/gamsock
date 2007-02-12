#!/bin/sh
OSNAME=`uname`

write_config() {
  echo "(include \"$1-errno.scm\")" >constants.scm
  echo "(include \"$1-netconst.scm\")" >>constants.scm

}

case $OSNAME in
Linux) write_config linux;;
NetBSD) write_config bsd;;
OpenBSD) write_config bsd;;
FreeBSD) write_config bsd; echo "(set! address-family/internet6 28) (set! protocol-family/internet6 address-family/internet6)" >>constants.scm ;;
esac
