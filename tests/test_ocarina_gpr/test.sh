#!/bin/sh

gpr=`ocarina-config --prefix`/lib/gnat

ADA_PROJECT_PATH="$gpr:$ADA_PROJECT_PATH" gnatmake -Ptest_000.gpr &&
ADA_PROJECT_PATH="$gpr:$ADA_PROJECT_PATH" gnatclean -Ptest_000.gpr

