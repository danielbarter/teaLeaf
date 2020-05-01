#!/bin/bash

clang -g generateTeaLeaf.c -o generateTeaLeaf.elf  -lfftw3 -lm
mv generateTeaLeaf.elf ../server/generateTeaLeaf.elf
