#! /bin/bash

sbt "aocJVM/testOnly aoc.Day${DAY}Spec -- -z Part${PART}"
