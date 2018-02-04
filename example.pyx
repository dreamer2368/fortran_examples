import numpy as np
cimport numpy as np
   
ctypedef np.float64_t DOUBLE
   
cdef extern:
  void c_example1(double *a, double *b, double *aplusb)
   
   
class example(object):
  
      dtype = np.double
  
  
  
      def example1(self, double a, double b):
          cdef double aplusb
          c_example1(&a,&b,&aplusb)
          return aplusb
