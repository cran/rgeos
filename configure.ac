# dnl Process this file with autoconf to produce a configure script.
define([pkgversion], esyscmd([sh -c "grep Version: DESCRIPTION | cut -d' ' -f2 | tr -d '\n'"]))dnl
AC_INIT(rgeos, [pkgversion], Roger.Bivand@nhh.no)
AC_CONFIG_SRCDIR(src/rgeos.c)

# find R home and set correct compiler + flags
: ${R_HOME=`R RHOME`}
if test -z "${R_HOME}"; then
  AC_MSG_ERROR([cannot determine R_HOME. Make sure you use R CMD INSTALL!])
fi
RBIN="${R_HOME}/bin/R"

# pick all flags for testing from R
: ${CC=`"${RBIN}" CMD config CC`}
: ${CXX=`"${RBIN}" CMD config CXX`}
: ${CPP=`"${RBIN}" CMD config CPP`}
: ${CFLAGS=`"${RBIN}" CMD config CFLAGS`}
: ${CPPFLAGS=`"${RBIN}" CMD config CPPFLAGS`}
: ${LDFLAGS=`"${RBIN}" CMD config LDFLAGS`}
# AC_SUBST([CC],["clang"])
# AC_SUBST([CXX],["clang++"])
AC_MSG_NOTICE([CC: ${CC}])
AC_MSG_NOTICE([CXX: ${CXX}])

AC_MSG_NOTICE([${PACKAGE_NAME}: ${PACKAGE_VERSION}])

SVN_VERSION=`svnversion -n '.'`
if test "${?}" && test "${SVN_VERSION}" != "exported"; then
  echo "${SVN_VERSION}" > inst/SVN_VERSION
else
  SVN_VERSION=`cat inst/SVN_VERSION | tr -d '\n'`
fi
AC_MSG_NOTICE([svn revision: ${SVN_VERSION}])

GEOS_CONFIG="geos-config"

AC_ARG_WITH([geos-config],
    AC_HELP_STRING([--with-geos-config=GEOS_CONFIG],
           [the location of geos-config]),
           [geos_config=$withval])
if test [ -n "$geos_config" ] ; then
    AC_SUBST([GEOS_CONFIG],["${geos_config}"])
fi


AC_MSG_CHECKING(geos-config usability)
if test `${GEOS_CONFIG} --version`;
then

        GEOS_VER=`${GEOS_CONFIG} --version`
        GEOS_VER_DOT=`echo $GEOS_VER | tr -d "."`
	GEOS_CPPFLAGS=`${GEOS_CONFIG} --cflags`
	GEOS_LIBS=`${GEOS_CONFIG} --libs`
        AC_MSG_RESULT(yes)

else

        AC_MSG_RESULT(no)
        AC_MSG_ERROR([${GEOS_CONFIG} not usable])
fi

AC_MSG_NOTICE([geos-config: ${GEOS_CONFIG}])
AC_MSG_NOTICE([GEOS version: ${GEOS_VER}])
AC_MSG_CHECKING([geos version over 3.2.1])
if test ${GEOS_VER_DOT} -lt 322 ; then
        AC_MSG_RESULT(no)
        AC_MSG_ERROR([Upgrade GEOS to at least 3.2.2])
else
        AC_MSG_RESULT(yes)
fi

AC_MSG_CHECKING(geos-config clibs)
if test ${GEOS_VER_DOT} -lt 332 ; then
        AC_MSG_RESULT(no)
        GEOS_CLIBS="-lgeos_c"
else        
        GEOS_CLIBS=`${GEOS_CONFIG} --clibs`
        AC_MSG_RESULT(yes)
fi


AC_SUBST([PKG_CPPFLAGS],["${PKG_CPPFLAGS} ${GEOS_CPPFLAGS}"])
AC_SUBST([PKG_LIBS],["${PKG_LIBS} ${GEOS_LIBS} ${GEOS_CLIBS}"])

# honor PKG_xx overrides
# for CPPFLAGS we will superfluously double R's flags
# since we'll set PKG_CPPFLAGS with this, but that shouldn't hurt
CPPFLAGS="${CPPFLAGS} ${PKG_CPPFLAGS}"
LIBS="${LIBS} ${PKG_LIBS}"

geosok=yes
AC_CHECK_HEADERS(geos_c.h,,geosok=no)
if test "${geosok}" = no; then
   AC_MSG_ERROR([geos_c.h not found in given locations.])
fi

#AC_CHECK_LIB(geos_c,initGEOS_r,,geosok=no)

[cat > geos_test.cc <<_EOCONF
#include <geos_c.h>
#ifdef __cplusplus
extern "C" {
#endif
static void __errorHandler(const char *fmt, ...) {
    return;
}
static void __warningHandler(const char *fmt, ...) {
    return;
}
int main() {
    GEOSContextHandle_t r = initGEOS_r((GEOSMessageHandler) __warningHandler, (GEOSMessageHandler) __errorHandler);
    finishGEOS_r(r);
}
#ifdef __cplusplus
}
#endif
_EOCONF]

#echo "${CXX} ${CPPFLAGS} -o geos_test geos_test.cc ${LIBS}"
AC_MSG_CHECKING(geos: linking with libgeos_c)
${CXX} ${CPPFLAGS} -o geos_test geos_test.cc ${LIBS} 2> errors.txt
if test `echo $?` -ne 0 ; then
geosok=no
AC_MSG_RESULT(no)
else
AC_MSG_RESULT(yes)
fi

if test "${geosok}" = no; then
   cat errors.txt
   AC_MSG_NOTICE([Install failure: compilation and/or linkage problems.])
   AC_MSG_ERROR([initGEOS_r not found in libgeos_c.])
fi

rm -f geos_test errors.txt geos_test.cc


if test "${geosok}" = no; then
   AC_MSG_ERROR([libgeos_c not found in given locations.])
fi

AC_MSG_NOTICE([PKG_CPPFLAGS: ${PKG_CPPFLAGS}])
AC_SUBST(PKG_CPPFLAGS)
AC_MSG_NOTICE([PKG_LIBS: ${PKG_LIBS}])
AC_SUBST(PKG_LIBS)

AC_CONFIG_FILES(src/Makevars)
AC_OUTPUT

