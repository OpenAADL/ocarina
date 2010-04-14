#!/bin/sh

gnatmake test_000.adb `ocarina-config` &&
gnatclean test_000.adb
