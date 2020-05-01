#!/bin/bash

clang -g -Wno-absolute-value generateTeaLeaf.c libbmp/libbmp.c -o generateTeaLeaf.elf -lfftw3 -lm
