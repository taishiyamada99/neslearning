#!/bin/bash

ca65 $1.asm -o $1.o -t nes
ld65 $1.o -o $1.nes -t nes