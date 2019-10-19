dnl Ada compiler handling
dnl Contributed by Samuel Tardieu <sam@inf.enst.fr>

dnl Usage: AM_PROG_ADA
dnl Look for an Ada compiler (ADA environment variable, then gcc, then $CC)

AC_DEFUN([AM_PROG_ADA],
[AC_BEFORE([$0], [AM_TRY_ADA])
AC_REQUIRE([AC_PROG_CC])
AC_CHECK_PROGS(ADA, gnatmake)
if test -z "$ADA"; then
  AC_MSG_RESULT([  Tentatively using $CC as an Ada compiler])
  ADA="$CC"
fi])

dnl Usage: AM_PROG_GNATCHOP
dnl Look for GNATCHOP program

AC_DEFUN([AM_PROG_GNATCHOP],
[AC_CHECK_PROG(GNATCHOP, gnatchop, gnatchop)])

dnl Usage: AM_TRY_ADA(gnatmake, filename, content, pragmas, success, failure)
dnl Compile, bind and link an Ada program and report its success or failure

AC_DEFUN([AM_TRY_ADA],
[mkdir conftest
cat > conftest/src.ada <<EOF
[$3]
EOF
cat > conftest/gnat.adc <<EOF
[$4]
EOF
ac_try="cd conftest && $GNATCHOP -q src.ada && $1 $2 > /dev/null 2>../conftest.out"
if AC_TRY_EVAL(ac_try); then
  ifelse([$5], , :, [rm -rf conftest*
  $5])
else
  ifelse([$6], , :, [ rm -rf conftest*
  $6])
fi
rm -f conftest*])

dnl Usage: AM_TRY_ADA_CONFPRAGMA(pragma, success, failure)
dnl Check whether a given configuration pragma is supported.

AC_DEFUN([AM_TRY_ADA_CONFPRAGMA],
[AC_REQUIRE([AM_CROSS_PROG_GNATMAKE])
AM_TRY_ADA($GNATMAKE_FOR_TARGET,[check.adb],
[procedure Check is begin null; end Check;],[$1],[$2],[$3])])

dnl Usage: AM_PROG_WORKING_ADA
dnl Try to compile a simple Ada program to test the compiler installation
dnl (especially the standard libraries such as Ada.Text_IO)

AC_DEFUN([AM_PROG_WORKING_ADA],
[AC_REQUIRE([AM_PROG_ADA])
AC_REQUIRE([AM_PROG_GNATCHOP])
AC_MSG_CHECKING([if the$crossflagmsg Ada compiler works])
AM_TRY_ADA([$ADA -c],[check.adb],
[with Ada.Text_IO;
procedure Check is
begin
   null;
end Check;
], [], [AC_MSG_RESULT(yes)],
[AC_MSG_RESULT(no)
AC_MSG_ERROR([Ada compiler is not working])])])

dnl Usage: AM_ADA_PREREQ(date, version)
dnl Check that GNAT is at least as recent as date (YYMMDD)

AC_DEFUN([AM_ADA_PREREQ],
[AC_REQUIRE([AM_PROG_WORKING_ADA])
AC_CHECK_PROG(GNATLS, gnatls, gnatls)
AC_CHECK_PROG(SED, sed, sed)
AC_MSG_CHECKING([if the Ada compiler is recent enough])
am_gnatls_date=`$GNATLS -v | $SED -ne 's/^GNATLS .*(\(.*\)).*$/\1/p'`
if test "$1" -le "$am_gnatls_date"; then
  AC_MSG_RESULT(yes)
else
  AC_MSG_RESULT(no)
  am_gnatls_version=`$GNATLS -v | $SED -ne 's/^GNATLS \(.*\) (.*.*$/\1/p'`
  AC_MSG_ERROR([Please get a version of GNAT no older than [$2 ($1)]
(it looks like you only have GNAT [$am_gnatls_version ($am_gnatls_date)])])
fi])

dnl Usage: AM_CROSS_PROG_ADA
dnl Look for an Ada compiler for the target (same as the host one if host and
dnl target are equal)

AC_DEFUN([AM_CROSS_PROG_ADA],
[AC_BEFORE([$0], [AM_TRY_CROSS_ADA])
AC_REQUIRE([AM_PROG_WORKING_ADA])
 if test $host = $target; then
   ADA_FOR_TARGET=$ADA
   AC_SUBST(ADA_FOR_TARGET)
 else
   AC_CHECK_PROGS(ADA_FOR_TARGET, [$target_alias-$ADA $target-$ADA])
 fi
])

dnl Usage: AM_CROSS_PROG_WORKING_ADA
dnl Try to use Ada compiler for the target if it is different from the host

AC_DEFUN([AM_CROSS_PROG_WORKING_ADA],
[AC_REQUIRE([AM_CROSS_PROG_ADA])
 if test $host != $target; then
   OLDADA=$ADA
   ADA=$ADA_FOR_TARGET
   crossflagmsg=" cross"
   AM_PROG_WORKING_ADA
   crossflagmsg=""
   ADA=$OLDADA
 fi
])

dnl Usage: AM_PROG_GNATMAKE
dnl Look for an Ada make

AC_DEFUN([AM_PROG_GNATMAKE],
[AC_REQUIRE([AC_PROG_CC])
AC_CHECK_PROGS(GNATMAKE, gnatmake gnatgcc adagcc gcc)
if test -z "$GNATMAKE"; then
  AC_MSG_RESULT([  Tentatively using $ADA as a make])
  GNATMAKE="$ADA"
fi])

dnl Usage: AM_CROSS_PROG_GNATMAKE
dnl Look for gnatmake for the target (same as the host one if host and
dnl target are equal)

AC_DEFUN([AM_CROSS_PROG_GNATMAKE],
[AC_REQUIRE([AM_PROG_WORKING_ADA])
 if test $host = $target; then
   GNATMAKE_FOR_TARGET=$GNATMAKE
   AC_SUBST(GNAMAKE_FOR_TARGET)
 else
   AC_CHECK_PROGS(GNATMAKE_FOR_TARGET, [$target_alias-$GNATMAKE $target-$GNATMAKE])
 fi
])

dnl Usage: AM_CROSS_PROG_CC
dnl Look for CC for the target (same as the host one if host and
dnl target are equal)

AC_DEFUN([AM_CROSS_PROG_CC],
[AC_REQUIRE([AC_PROG_CC])
 if test $host = $target; then
   CC_FOR_TARGET=$CC
   AC_SUBST(CC_FOR_TARGET)
 else
   AC_CHECK_PROGS(CC_FOR_TARGET, [$target_alias-$CC $target-$CC])
 fi
])

dnl Usage: AM_HAS_GNAT_SOCKETS_COPY
dnl Determine whether GNAT.Sockets has a Copy operation.

AC_DEFUN([AM_HAS_GNAT_SOCKETS_COPY],
[AC_REQUIRE([AM_CROSS_PROG_GNATMAKE])
AC_BEFORE([AM_HAS_GNAT_SOCKETS_COPY])
AC_BEFORE([AM_HAS_GNAT_OS_LIB_CLOSE_WITH_STATUS])
AC_BEFORE([AM_HAS_PRAGMA_PROFILE_RAVENSCAR])
AC_BEFORE([AM_HAS_PRAGMA_PROFILE_WARNINGS])
AC_MSG_CHECKING([whether you have GNAT.Sockets.Copy])
AM_TRY_ADA($GNATMAKE_FOR_TARGET,[check.adb],
[with GNAT.Sockets;
procedure Check is
   S1, S2 : GNAT.Sockets.Socket_Set_Type;
begin
   GNAT.Sockets.Copy (S1, S2);
end Check;
], [], [AC_MSG_RESULT(yes)
MISS_GNAT_SOCKETS_COPY="--  "],
[AC_MSG_RESULT(no)
HAVE_GNAT_SOCKETS_COPY="--  "])
AC_SUBST(MISS_GNAT_SOCKETS_COPY)dnl
AC_SUBST(HAVE_GNAT_SOCKETS_COPY)])

dnl Usage: AM_HAS_GNAT_OS_LIB_CLOSE_WITH_STATUS
dnl Determine whether GNAT.OS_Lib has a Close operation with status report.

AC_DEFUN([AM_HAS_GNAT_OS_LIB_CLOSE_WITH_STATUS],
[AC_REQUIRE([AM_CROSS_PROG_GNATMAKE])
AC_MSG_CHECKING([whether you have GNAT.OS_Lib.Close (FD : File_Descriptor; Status : out Boolean)])
AM_TRY_ADA($GNATMAKE_FOR_TARGET,[check.adb],
[with GNAT.OS_Lib;
procedure Check is
   FD : GNAT.OS_Lib.File_Descriptor;
   Status : boolean;
begin
   GNAT.OS_Lib.Close (FD, Status);
end Check;
], [], [AC_MSG_RESULT(yes)
MISS_GNAT_OS_LIB_CLOSE_WITH_STATUS="--  "],
[AC_MSG_RESULT(no)
HAVE_GNAT_OS_LIB_CLOSE_WITH_STATUS="--  "])
AC_SUBST(MISS_GNAT_OS_LIB_CLOSE_WITH_STATUS)dnl
AC_SUBST(HAVE_GNAT_OS_LIB_CLOSE_WITH_STATUS)])

dnl Usage: AM_HAS_GNAT_PERFECT_HASH_GENERATORS
dnl Determine whether GNAT.Perfect_Hash_Generators exists

AC_DEFUN([AM_HAS_GNAT_PERFECT_HASH_GENERATORS],
[AC_REQUIRE([AM_CROSS_PROG_GNATMAKE])
AC_MSG_CHECKING([whether you have GNAT.Perfect_Hash_Generators])
AM_TRY_ADA($GNATMAKE_FOR_TARGET,[check.adb],
[with GNAT.Perfect_Hash_Generators;
procedure Check is begin null; end Check;
], [], [AC_MSG_RESULT(yes)
GNAT_PERFECT_HASH_GENERATORS="GNAT.Perfect_Hash_Generators"],
[AC_MSG_RESULT(no)
GNAT_PERFECT_HASH_GENERATORS="GNAT.Perfect_Hash.Generators"])
AC_SUBST(GNAT_PERFECT_HASH_GENERATORS)])

dnl AM_HAS_PRAGMA_PROFILE_RAVENSCAR

AC_DEFUN([AM_HAS_PRAGMA_PROFILE_RAVENSCAR],
[AC_REQUIRE([AM_CROSS_PROG_GNATMAKE])
AC_MSG_CHECKING([whether pragma Profile (Ravenscar) is supported])
AM_TRY_ADA_CONFPRAGMA([pragma Profile (Ravenscar);],
[AC_MSG_RESULT(yes)
PRAGMA_PROFILE_RAVENSCAR="pragma Profile (Ravenscar);"],
[AC_MSG_RESULT(no)
PRAGMA_PROFILE_RAVENSCAR="pragma Ravenscar;"])
AC_SUBST(PRAGMA_PROFILE_RAVENSCAR)])

AC_DEFUN([AM_HAS_PRAGMA_PROFILE_WARNINGS],
[AC_REQUIRE([AM_CROSS_PROG_GNATMAKE])
AC_MSG_CHECKING([whether pragma Profile_Warnings (Ravenscar) is supported])
AM_TRY_ADA_CONFPRAGMA([pragma Profile_Warnings (Ravenscar);],
[AC_MSG_RESULT(yes)
DISABLE_PROFILE_WARNINGS=""],
[AC_MSG_RESULT(no)
DISABLE_PROFILE_WARNINGS="--  "])
AC_SUBST(DISABLE_PROFILE_WARNINGS)])

AC_DEFUN([AM_HAS_APDIR],
[AC_REQUIRE([AM_CROSS_PROG_GNATMAKE])
AC_MSG_CHECKING([whether $GNATMAKE_FOR_TARGET supports -aPdir])
if AC_TRY_COMMAND([$GNATMAKE_FOR_TARGET 2>&1 | grep " -aPdir" > /dev/null])
then
  HAVE_GNATMAKE_APDIR=yes
else
  HAVE_GNATMAKE_APDIR=no
fi
AC_MSG_RESULT($HAVE_GNATMAKE_APDIR)
AM_CONDITIONAL(HAS_GNATMAKE_APDIR, [test "x$HAVE_GNATMAKE_APDIR" = "xyes"])
AC_SUBST(HAVE_GNATMAKE_APDIR)
])

dnl AM_GPRBUILD
dnl adapted from GtkAda

AC_DEFUN(AM_GPRBUILD,
[
   AC_PATH_PROG(GPRBUILD, gprbuild, no)
   AC_PATH_PROG(GPRINSTALL, gprinstall, no)

   if test x$GPRBUILD = xno ; then
      AC_MSG_ERROR(I could not find gprbuild. .)
   fi

   AC_MSG_CHECKING(that your gnat compiler works with a simple example)

   rm -f conftest.adb
   cat << EOF > conftest.adb
with Ada.Text_IO;

procedure Conftest is
   Conftest_Ok : Ada.Text_IO.File_Type;
begin
   Ada.Text_IO.Create (File => Conftest_Ok,
                       Name => "$conftest_ok");
   Ada.Text_IO.Close (Conftest_Ok);
end Conftest;
EOF
   cat <<EOF > conftest.gpr
project Conftest is
   for Main use ("conftest.adb");
   for Source_Files use ("conftest.adb");
end Conftest;
EOF

   $GPRBUILD -q -P conftest.gpr > /dev/null
   rm -f auto.cgpr b__conftest.*

   if ( test ! -x conftest ) then
      AC_MSG_RESULT(no)
      AC_MSG_ERROR($GPRBUILD test failed at compile time! Check your configuration.)
   fi

   ./conftest

   if ( test ! -f $conftest_ok ) then
      AC_MSG_RESULT(no)
      AC_MSG_ERROR($GPRBUILD test failed at run time! Check your configuration.)
   fi

   AC_MSG_RESULT(yes)
])
