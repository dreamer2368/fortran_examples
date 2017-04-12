F90 = gfortran
F90FLAGS = -lstdc++ -fdefault-real-8 -fbounds-check -Wconversion -fopenmp

EXE = exec
SRC = Prog.f90 demo.f90
OBJ = Prog.o demo.o

all: $(EXE)
run: $(EXE) 
	./$(EXE)

Prog.o: demo.o

$(EXE): $(OBJ)
	$(F90) $(F90FLAGS) -o $(EXE) $(OBJ)

%.o: %.f90
	$(F90) $(F90FLAGS) -c $<

clean:
	rm *.o *.mod $(EXE)

.PHONY: all clean
