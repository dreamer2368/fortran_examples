import numpy as np
cimport numpy as np

ctypedef np.float64_t DOUBLE

cdef extern:
    void c_ptc2pdf(int *Ng, double *dx, double *dy, int *N, double *pg,
                    double *x, double *y, double *spwt_input)

class mesh(object):

    dtype = np.double
    
    def __init__(self,Ng,Lx,Ly):
        self.Ng = Ng
        self.Lx = Lx
        self.Ly = Ly
        self.dx = Lx/Ng
        self.dy = Ly/Ng
        self.xg = np.linspace(-Lx,Lx,2*Ng+1)
        self.yg = np.linspace(-Ly,Ly,2*Ng+1)

    def ptc2pdf(self,
                double[:] x, double[:] y,
                double[:] spwt=None):
        cdef int N, Ng
        N = len(x)
        Ng = self.Ng
        cdef double dx,dy
        dx,dy = self.dx, self.dy
        if spwt is None:
            spwt = 1./N*np.ones(N)
        cdef double[:] sc=np.ascontiguousarray(spwt,dtype=np.double)
        cdef double[:] xc=np.ascontiguousarray(x,dtype=np.double)
        cdef double[:] yc=np.ascontiguousarray(y,dtype=np.double)
        pg = np.zeros((2*self.Ng+1,2*self.Ng+1))
        cdef double[:,:] pgc = np.ascontiguousarray(pg,dtype=np.double)
       
        c_ptc2pdf(&Ng,&dx,&dy,&N,&pgc[0,0],&xc[0],&yc[0],&sc[0])
        return pgc