import numpy as np
cimport numpy as np
   
ctypedef np.float64_t DOUBLE
   
cdef extern:
  void c_plus_in_fortran(double *a, double *b, double *aplusb)
   
def plus_in_python(self, double a, double b):
    cdef double aplusb
    c_plus_in_fortran(&a,&b,&aplusb)
    return aplusb
