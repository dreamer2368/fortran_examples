cd src/;
gfortran -c mesh.f90 fmesh_wrapper.f90;
cd ../;
python setup.py build_ext --inplace;
