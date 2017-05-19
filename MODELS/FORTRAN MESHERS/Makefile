OBJECTS = Malla.o
FFLAGS= -O3
FLAGPARALLEL= -fbounds-check
FC= gfortran
.PHONY: clean

malla.out: $(OBJECTS)
	$(FC) $(OBJECTS) $(FFLAGS) -o malla.out


Malla.o: Malla.f90	
	$(FC) -c Malla.f90

clean:
	rm -f $(OBJECTS) malla.out

