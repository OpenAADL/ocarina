# Automake macro for finding GNU make

AC_DEFUN([AM_GNU_MAKE],
[
AC_CHECK_PROGS(GNU_MAKE, [gmake gnumake make], no)

AC_MSG_CHECKING(whether ${GNU_MAKE} is a GNU make)
if test ! "x${GNU_MAKE}" = "xno"; then
   if ${GNU_MAKE} --version | head -1 | grep GNU >/dev/null 2>&1; then
      AC_MSG_RESULT(yes.)
   else
      GNU_MAKE="no"
   fi
fi

if test "x$GNU_MAKE" = "xno"; then
   AC_MSG_RESULT(no.)
   AC_MSG_ERROR([GNU Make has not been found on your system])
fi
])