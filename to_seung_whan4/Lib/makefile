FC = gfortran 
FCFLAGS = -fdefault-real-8 -fbounds-check
EXE = exec
OBJECTS = main.o commonlyUsed.o
F90FILES = cat.f90 mean.f90 mesh.f90 misc.f90 printArray.f90 quadrature.f90 tecplotAscii.f90

# First target is the executable
all: $(EXE)

run: $(EXE)
	./$(EXE)

# Dependencies
main.exe: main.o
main.o: commonlyUsed.o
commonlyUsed.o: $(F90FILES)

# Link all object files to executable
$(EXE): $(OBJECTS)
	$(FC) $(FCFLAGS) -o $(EXE) $(OBJECTS)

# All .o files depend on the corresponding .f90 file
%.o: %.f90
	$(FC) $(FCFLAGS) -c $<

clean:
	rm $(OBJECTS) $(EXE)

.PHONY: all run clean
