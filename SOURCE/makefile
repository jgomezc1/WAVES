# ============================================================================
# Name        : Makefile
# Author      : Mechanic Aplicada
# Version     :
# Copyright   : This is CopyRighted to Ricardo Serrano
# Description : Makefile for Hello MPI World in Fortran
# ============================================================================

.PHONY: all clean

.SUFFIXES: .for

FC=gfortran -g3 -ggdb -fbounds-check -O3 -fopenmp
#LIBS=-lpthread
FLAGS=
FLAGS+=$(INCS) $(LIBS)

SOURCES=mtxutil.for gaussin.for matutil.for femutil.for uelutil.for asemutil.for postproc.for preproc.for incodrm.for adamiant.for 

OBJECTS=$(SOURCES:.for=.o)

all: $(SOURCES) damiane.out

damiane.out: $(OBJECTS)
	$(FC) $(FLAGS) -o $@ $(OBJECTS)


.for.o:
	$(FC) $(INCS) -c $< -o $@

clean:
	rm -f damiane.out *.mod *.o

echo:
	echo $(OBJS)
