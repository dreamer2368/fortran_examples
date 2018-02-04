cd src/;
gfortran -c example.f90 example_wrapper.f90;
cd ../;
python hsetup.py build_ext --inplace;
