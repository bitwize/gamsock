#!/bin/sh
GAMBIT_PATH=$HOME/local/Gambit-C
$GAMBIT_PATH/bin/gsi config-gamsock.scm
$GAMBIT_PATH/bin/gsc -keep-c gamsock.scm
