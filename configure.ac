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
#  Chapter 0: About the author
##
######


AC_INIT(goblint, 0.9.2)

LINKLIBS="str cil xml-light"
DEFAULTCIL="../cil"
DEFAULTXML="../xml-light"
DEFAULTOUNIT="../ounit"


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

AC_SUBST(CIL)
AC_SUBST(CILOBJ,/obj/$ARCHOS)



######
##
#  Chapter 3: The Great o Camlfind
##
######


# Can we use ocamlfind?
AC_PATH_PROG(OCAMLFIND, ocamlfind)

if test -z $OCAMLFIND
then
    # No? Really?    

    AC_CHECK_FILE($FINDPATH/ocamlfind,ok=true,ok="")
    if test -z $ok 
    then	
        # Oh, well ...
	AC_MSG_WARN([Can't find ocamlfind! Specify by running "./configure FINDPATH=<Path>"])
	AC_SUBST(CAMLDIR,"")
	NO_FIND=1
	LINKLIBS="unix $LINKLIBS"
    else
	AC_SUBST(CAMLDIR,$FINDPATH/ocamlfind )
	COMPILEFLAGS=["-package oUnit,xml-light -linkpkg"]
    fi
else
    # Finding a camel will be easy...
    AC_SUBST(CAMLDIR,$OCAMLFIND )
    COMPILEFLAGS=["-package oUnit,xml-light -linkpkg"]
fi



######
##
#  Chapter 4: The Libraries
##
######

# Do we need them?
if test $NO_FIND
then
    # The hard way then ...

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
		Can't find ocamlfind nor oUnit! Unittests disabled!
		Specify by running "./configure OUNIT=<Path>"
	                       	or "./configure FINDPATH=<Path>"])
	OUNIT=""
    else
	# Now we are ready for the ride.
	COMPILEFLAGS="$COMPILEFLAGS -I $OUNIT/"
    fi	
    # Do we know where to look?
    if test -z $XML
    then 
         # No ... Maybe in ../xml-light ?
         XML=$DEFAULTXML
    fi


    # Are mommy and daddy home?
    AC_CHECK_FILE($XML/xml-light.cma,ok=true,ok="")
    AC_CHECK_FILE($XML/xml-light.cmxa,      ,ok="")

    if test -z $ok 
    then	
        # Oh, well ...
        AC_MSG_ERROR([Can't find Xml-light! Specify by running "./configure XML=<Path>"])
    else
        AC_MSG_RESULT(using xml-light from $XML)
    fi
fi

AC_SUBST(LINKLIBS)
AC_SUBST(COMPILEFLAGS)
AC_SUBST(OUNIT)
AC_SUBST(XML)


######
##
#  Chapter 4: Now we only need a camel.
##
######


AC_OUTPUT(Makefile)
