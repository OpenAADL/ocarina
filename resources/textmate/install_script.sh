#!/bin/sh

#This script could be used to checkout TextMate bundle from Ocarina repository

LC_CTYPE=en_US.UTF-8
SVN=`which svn`

echo Changing to Bundles directory...
mkdir -p /Library/Application\ Support/TextMate/Bundles
cd /Library/Application\ Support/TextMate/Bundles

if [ -d /Library/Application\ Support/TextMate/Bundles/AADL.tmbundle ]; then
   echo AADL bundle already exists - updating...
   $SVN up AADL.tmbundle
else
   echo Checking out AADL bundle...
   $SVN co https://eve.enst.fr/svn/aadl/software/ocarina/resources/textmate/AADL.tmbundle
fi

echo Reloading bundles in TextMate...
osascript -e 'tell app "TextMate" to reload bundles'

