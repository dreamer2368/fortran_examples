F90 = mpifort
F90FLAGS =

EXE = exec
SRC = main.f90
OBJ = $(subst .f90,.o,$(SRC)) 

all: $(EXE)

#Prog.o: demo.o

$(EXE): $(OBJ)
	$(F90) $(F90FLAGS) -o $(EXE) $(OBJ)

%.o: %.f90
	$(F90) $(F90FLAGS) -c $<

clean:
	rm *.o *.mod $(EXE)

.PHONY: all clean
