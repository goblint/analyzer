#
# Autoconf configure file for the goblin.
# 
# Usage:
#	./configure [CIL=<path to cil>]
#		    [XML=<path to xml-light>]
#		    [FINDPATH=<path to ocamlfind> |
#		     OUNIT=<path to ounit>]
#
#

######
##
#  Chapter 0: About us
##
######


AC_INIT(goblint, 0.9.2)

m4_include([scripts/ax_compare_version.m4])

LINKLIBS="unix str nums cil xml-light"
DEFAULTCIL="../cil/"
DEFAULTXML="../xml-light/"
DEFAULTOUNIT="../ounit/"
DEFAULTDOC="ocamldoc.opt"


# Is it something for you?
if test configure -ot configure.ac; then
  AC_MSG_ERROR(configure is older than configure.in; you forgot to run autoconf)
fi


######
##
#  Chapter 1: Where are we?
##
######


AC_CANONICAL_SYSTEM

case "$target" in
  # linux
  *86*linux*|*86*freebsd*|*86*openbsd*)
    AC_MSG_RESULT(configuring for linux/x86)
    ARCHOS=x86_LINUX
    ;;
  # os x x86
  *86*darwin*)
    AC_MSG_RESULT(configuring for osx/x86)
    ARCHOS=x86_DARWIN
    ;;

  # Mac OS X
  *powerpc*darwin*)
    AC_MSG_RESULT(configuring for powerpc/darwin, which we treat like linux/x86)
    ARCHOS=ppc_DARWIN
    ;;

  # cygwin
  *86*cygwin*)
    AC_MSG_RESULT(configuring for Cygwin on win32/x86)
    ARCHOS=x86_WIN32
    ;;

  # Solaris
  *sparc*solaris*)
    AC_MSG_RESULT(configuring for SPARC/Solaris)
    ARCHOS=sparc_SOLARIS
    ;;

  *)
    AC_MSG_ERROR([
      Unsupported platform $target -- sorry.
      ./configure supports these platforms:
         on x86:     Linux, Win32(with Cygwin), freeBSD, openBSD, and Mac OS X
         on PowerPC: Mac OS X
         on SPARC:   Solaris
    ])
    ;;
esac


######
##
#  Chapter 2: Finding Cil
##
######

# Do we know where to look?
if test -z $CIL
then 
     # No ... Maybe in ../cil ?
     CIL=$DEFAULTCIL
fi


# CIL is that you?
AC_CHECK_FILE($CIL/obj/$ARCHOS/cil.cma,ok=true,ok="")

if test -z $ok 
then	
    # YOU'RE A PHONEY!!! 
    AC_MSG_ERROR([Can't find CIL! Specify "./configure CIL=<Path>"])
else
    AC_MSG_RESULT(using cil from $CIL)
fi

# test cil version
CIL_VERSION=`$CIL/bin/cilly --version | head -n 1 | cut -d " " -f3`
AX_COMPARE_VERSION([echo $CIL_VERSION],[lt],[1.3.6])

if `$ax_compare_version` 
then
    AC_MSG_ERROR([CIL is too old, use at least version 1.3.6 !])
fi

AC_SUBST(CIL)
AC_SUBST(CILOBJ,/obj/$ARCHOS)


######
##
#  Chapter 3: oCamlfind
##
######


if test -z "$FINDPATH"
then
  AC_PATH_PROG(OCAMLFIND, ocamlfind)
else
  OCAMLFIND=$FINDPATH/ocamlfind
fi

AC_CHECK_FILE($OCAMLFIND,ok=true,ok="")
if test -z "$ok"
then
  # Oh, well ...
  AC_MSG_WARN([Can't find ocamlfind! Specify by running "./configure FINDPATH=<Path>"])
  AC_SUBST(CAMLDIR,"")
  NO_FIND=1
fi

if test ! $NO_FIND
then
  AC_SUBST(CAMLDIR, $OCAMLFIND)
  AC_SUBST(OCAMLDOC,"$OCAMLFIND ocamldoc")
else
  AC_SUBST(OCAMLDOC,$DEFAULTDOC)
fi 

######
##
#  Chapter 4: The Libraries
##
######

#ounit
if test -z "$OUNIT" 
then 
  if test ! "$NO_FIND"   
  then
    OUNIT=`$OCAMLFIND query oUnit`
    USE_FIND=true
  fi
fi 


if test -z $OUNIT
then
  OUNIT=$DEFAULTOUNIT
fi

# Is oUnit built
AC_CHECK_FILE($OUNIT/oUnit.cma,ok=true,ok="")
AC_CHECK_FILE($OUNIT/oUnit.cmxa,      ,ok="")

if test -z $ok
then
  AC_MSG_WARN([
  Can't find oUnit! Unittests disabled!
  Specify by running "./configure OUNIT=<Path>"])
  OUNIT=""
else
  # Now we are ready for the ride.
  if test $USE_FIND
  then
    COMPILEFLAGS="$COMPILEFLAGS -package oUnit"
    USE_FIND=""
  else
    COMPILEFLAGS="$COMPILEFLAGS -I $OUNIT/"
  fi
fi	


# Do we know where to look?


if test -z $XML 
then 
  if test ! $NO_FIND
  then
    XML=`$OCAMLFIND query xml-light`
    USE_FIND=true
  fi
fi 


if test -z $XML
then 
     # No ... Maybe in ../xml-light ?
     XML=$DEFAULTXML
     USE_FIND=""
fi


AC_CHECK_FILE($XML/xml-light.cma,ok=true,ok="")
AC_CHECK_FILE($XML/xml-light.cmxa,      ,ok="")

if test -z $ok 
then	
    # Oh, well ...
    AC_MSG_ERROR([Can't find Xml-light! Specify by running "./configure XML=<Path>"])
else
  AC_MSG_RESULT(using xml-light from $XML)
  
  if test $USE_FIND
  then
    COMPILEFLAGS="$COMPILEFLAGS -package xml-light"
    USE_FIND=""
  else
    COMPILEFLAGS="$COMPILEFLAGS -I $XML/ -ccopt -L$XML"
  fi
  
fi

AC_SUBST(LINKLIBS)
AC_SUBST(COMPILEFLAGS)
AC_SUBST(DOCFLAGS)
AC_SUBST(OUNIT)
AC_SUBST(XML)
AC_SUBST(JSON)


AC_OUTPUT(Makefile)
