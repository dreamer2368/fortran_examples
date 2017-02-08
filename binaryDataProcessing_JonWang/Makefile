FCOMP = gfortran
FFLAGS = -O2 -fdefault-real-8 -fbounds-check -fimplicit-none

FSRC = exampleIO.f90
FOBJ = exampleIO.o
EXE = exec

$(EXE): $(FOBJ)
	$(FCOMP) $(FFLAGS) -o $(EXE) $(FOBJ) 

%.o: %.f90
	$(FCOMP) $(FFLAGS) -c $<

clean:
	rm *.o *.mod $(EXE)

echo:
	echo $(FSRC)
	echo $(FOBJ)

.PHONY: all run clean


