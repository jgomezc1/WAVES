OBJECTS = Malla9PaperReg.o
FFLAGS= -O3
FLAGPARALLEL= -fbounds-check
FC= gfortran
.PHONY: clean

malla.out: $(OBJECTS)
	$(FC) $(OBJECTS) $(FFLAGS) -o malla.out


Malla9PaperReg.o: Malla9PaperReg.f90	
	$(FC) -c Malla9PaperReg.f90

clean:
	rm -f $(OBJECTS) malla.out

