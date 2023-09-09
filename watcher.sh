#!/bin/sh

while inotifywait -e modify code.scm; do
    echo "\nCompiling" &&
    csc code.scm &&
    echo "\nCompiling done" &&
    echo "\nRuning\n" &&
    ./code &&
    rm code
done
