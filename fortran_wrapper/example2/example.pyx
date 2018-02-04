import numpy as np
cimport numpy as np
   
ctypedef np.float64_t DOUBLE
   
cdef extern:
    void c_product_in_fortran(double *a, double *b, double *aprodb)
    void c_plus_in_fortran(double *a, double *b, double *aplusb)
    
def plus_in_python(double a, double b):
    cdef double aplusb
    c_plus_in_fortran(&a,&b,&aplusb)
    return aplusb

def product_in_python(double a, double b):
    cdef double aprodb
    c_product_in_fortran(&a,&b,&aprodb)
    return aprodb

def plus_and_product(double a, double b, double c):
    cdef double temp, result
    c_product_in_fortran(&b,&c,&temp)
    c_plus_in_fortran(&a,&temp,&result)
    return result
