module fmesh_wrapper

	use iso_c_binding, only: c_double, c_int
	use mesh

	implicit none

contains

	subroutine c_ptc2pdf(Ng,dx,dy,N,pg,x,y,spwt) bind(c)
		integer(c_int), intent(in) :: Ng, N
		real(c_double), intent(in) :: dx, dy, x(N), y(N)
		real(c_double), intent(in) :: spwt(N)
		real(c_double), dimension(0:2*Ng,0:2*Ng), intent(out) :: pg
		call ptc2pdf(Ng,dx,dy,N,pg,x,y,spwt)
	end subroutine

end module