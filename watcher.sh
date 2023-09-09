#!/bin/sh

while inotifywait -e modify code.scm; do
  csc code.scm
done
