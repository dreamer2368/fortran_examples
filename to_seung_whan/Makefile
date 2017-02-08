include /home/jmwang2/usr/include/cantera/Cantera.mak

### Compilers & flags
F90=gfortran
CPP=g++
F90FLAGS=-lstdc++ -fdefault-real-8 -fbounds-check -fopenmp
CPPFLAGS=-std=c++0x $(CANTERA_INCLUDES)
LDLIBS=$(CANTERA_LIBS)

### Files
ModLib = ModLib/*.f90

EXE1 = execInit
F90SRC1 = ProgInit.f90 ModLib.f90 ModConstants.f90
F90OBJ1=$(subst .f90,.o,$(F90SRC1))

EXE2 = execData
F90SRC2 = ProgData.f90 ModLib.f90 ModReadInputs.f90 ModConstants.f90 ModModel.f90 \
	ModParameters.f90 ModRk4.f90 ModFluid.f90
F90OBJ2 = $(subst .f90,.o,$(F90SRC2))

EXE3 = execModel
F90SRC3 = ProgModel.f90 ModLib.f90 ModReadInputs.f90 ModConstants.f90 ModModel.f90 \
	ModParameters.f90 ModRk4.f90 ModFluid.f90 ModCalibrator.f90 ModMcmc.f90
F90OBJ3 = $(subst .f90,.o,$(F90SRC3))

EXE4 = execStar
F90SRC4 = ProgStar.f90 ModLib.f90 ModReadInputs.f90 ModConstants.f90
F90OBJ4 = $(subst .f90,.o,$(F90SRC4))

EXE5 = execQgrid
F90SRC5 = ProgQgrid.f90 ModReadInputs.f90 ModLib.f90 ModConstants.f90 ModModel.f90 \
	ModParameters.f90 ModRk4.f90 ModFluid.f90
F90OBJ5 = $(subst .f90,.o,$(F90SRC5))

CPPSRC = canteraKinetics.cpp
CPPOBJ = $(subst .cpp,.o,$(CPPSRC))

EXE = $(EXE1) $(EXE2) $(EXE3) $(EXE4) $(EXE5)

### Targets
all: $(EXE)
runInit: $(EXE1) 
	./$(EXE1)
runData: $(EXE2)
	./$(EXE2)
runModel: $(EXE3)
	./$(EXE3)
runStar: $(EXE4)
	./$(EXE4)
runQgrid: $(EXE5)
	./$(EXE5)

### Dependencies
ProgInit.o: ModLib.o ModConstants.o
ProgData.o: ModLib.o ModConstants.o ModReadInputs.o ModModel.o
ProgModel.o: ModCalibrator.o
ProgStar.o: ModLib.o ModReadInputs.o ModConstants.o
ProgQgrid.o: ModLib.o ModReadInputs.o ModModel.o ModParameters.o
ModCalibrator.o: ModLib.o ModReadInputs.o ModMcmc.o ModModel.o ModParameters.o
ModModel.o: ModParameters.o ModConstants.o ModFluid.o ModLib.o ModRk4.o canteraKinetics.o
ModFluid.o: ModLib.o ModConstants.o
ModParameters.o: ModLib.o ModConstants.o
ModReadInputs.o: ModLib.o
ModRK4.o: ModFluid.o ModParameters.o
ModMcmc.o: ModLib.o

# Link object files to executables
$(EXE1): $(F90OBJ1)
	$(F90) $(F90FLAGS) -o $(EXE1) $(F90OBJ1)

$(EXE2): $(F90OBJ2) $(CPPOBJ)
	$(F90) $(F90FLAGS) -o $(EXE2) $(F90OBJ2) $(CPPOBJ) $(LDLIBS)

$(EXE3): $(F90OBJ3) $(CPPOBJ)
	$(F90) $(F90FLAGS) -o $(EXE3) $(F90OBJ3) $(CPPOBJ) $(LDLIBS)

$(EXE4): $(F90OBJ4)
	$(F90) $(F90FLAGS) -o $(EXE4) $(F90OBJ4)

$(EXE5): $(F90OBJ5) $(CPPOBJ)
	$(F90) $(F90FLAGS) -o $(EXE5) $(F90OBJ5) $(CPPOBJ) $(LDLIBS)

# All .o files depend on the corresponding .f90 file
%.o: %.f90
	$(F90) $(F90FLAGS) -c $<

# All .o files depend on the corresponding .cpp file
$(CPPOBJ): $(CPPSRC)
	$(CPP) $(CPPFLAGS) -c $<

ModLib.o: Lib/*.f90
	$(F90) $(F90FLAGS) -c ModLib.f90

echo:
	echo $(EXE1)
	echo $(EXE2)
	echo $(EXE3)
	echo $(EXE4)
	echo $(F90OBJ1)
	echo $(F90OBJ2)
	echo $(F90OBJ3)
	echo $(F90OBJ4)
	echo $(CPPOBJ)
	echo $(ModLib)
	echo $(ModLibInclude)
	echo $(CANTERA_INCLUDES)
	echo $(CANTERA_LIBS)

clean:
	rm *.o *.mod $(EXE)

.PHONY: all run clean


