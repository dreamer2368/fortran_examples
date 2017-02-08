### Compilers & flags
F90=gfortran

FFTWLIBS=~/bin/FFTW/lib/libfftw3.a
VECLIBSMACOSX=
LAPACKLIB=-L/opt/local/lib/lapack-3.5.0 -llapack -lblas
BLASLIB=/opt/local/lib/lapack-3.5.0/librefblas.a
PNETCDFLIBS=

LIBS    = $(LAPACKLIB) $(FFTWLIBS)


EXE = exec
F90SRC = main.f90 FastPoisson.f90 FFTPoisson.f90 constants.f90
F90OBJ = main.o FastPoisson.o FFTPoisson.o constants.o

### Targets
all: $(EXE)
run: $(EXE) 
	./$(EXE)

# Link object files to executables
$(EXE): $(F90OBJ)
	$(F90) -o $(EXE) $(F90OBJ) $(LIBS)

# All .o files depend on the corresponding .f90 file
%.o: %.f90
	$(F90) -c $<

# Dependencies
FastPoisson.o : constants.o
FFTPoisson.o : constants.o
main.o: FastPoisson.o FFTPoisson.o

clean:
	rm *.o *.mod $(EXE)

.PHONY: all run clean


