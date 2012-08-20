#!/bin/sh
GAMBIT_PATH=$HOME/local/Gambit-C
$GAMBIT_PATH/current/bin/gsi config-gamsock.scm
$GAMBIT_PATH/current/bin/gsc gamsock.scm
