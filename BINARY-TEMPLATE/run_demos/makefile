all:	
	cd ..; make

clib:	CLIB
CLIB:
	cd ..; make clib

mlib:	MLIB
MLIB:
	cd ..; make mlib

util:	UTIL
UTIL:
	cd ..; make util

protos:	PROTOS
PROTOS:
	cd ..; make protos

demos:	DEMOS
DEMOS:
	cd ..; make demos

your_prog:	your_prog.o ../clib/libisis1.a  ../clib/libisis2.a  ../mlib/libisism.a 
	cc -o your_prog your_prog.o ../clib/libisis1.a  ../clib/libisis2.a  ../mlib/libisism.a 

your_prog.o:	your_prog.c
	cc -c -I../../include your_prog.c
