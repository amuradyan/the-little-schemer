#!/usr/bin/env bash

while inotifywait -e modify code.scm; do
    echo "Compiling" &&
    csc code.scm &&
    echo "Compiling done" &&
    echo "Runing" &&
    ./code
done
