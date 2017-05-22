# ============================================================================
# Name        : Makefile
# Author      : Mecanica Aplicada
# Version     :
# Copyright   : This is CopyRighted to Mecanica Aplicada
# Description : Makefile for program WAVES
# ============================================================================

.PHONY: all clean

.SUFFIXES: .for

FC=gfortran -g3 -ggdb -fbounds-check -O3 -fopenmp
#LIBS=-lpthread
FLAGS=
FLAGS+=$(INCS) $(LIBS)

SOURCES=mtxutil.for gaussin.for matutil.for femutil.for uelutil.for asemutil.for postproc.for preproc.for incodrm.for WAVES.for 

OBJECTS=$(SOURCES:.for=.o)

all: $(SOURCES) WAVES.out

WAVES.out: $(OBJECTS)
	$(FC) $(FLAGS) -o $@ $(OBJECTS)


.for.o:
	$(FC) $(INCS) -c $< -o $@

clean:
	rm -f WAVES.out *.mod *.o

echo:
	echo $(OBJS)
