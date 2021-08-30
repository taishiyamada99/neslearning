#!/bin/bash

: > ./tmp/stdout.log
: > ./tmp/stderr.log

LOG_OUT=./tmp/stdout.log
LOG_ERR=./tmp/stderr.log

exec 1>>$LOG_OUT
exec 2>>$LOG_ERR

ca65 $1.asm -o $1.o -t nes
ld65 $1.o -o $1.nes -t nes --dbgfile $1.dbg
