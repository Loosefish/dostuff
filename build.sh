#! /bin/sh -e
gcc -Wall -std=gnu99 -O3 -c dostuff.c -o dostuff.o
gcc -lmagic dostuff.o -o dostuff
