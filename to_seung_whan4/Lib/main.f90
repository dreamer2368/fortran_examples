program main

	use commonlyUsed

	real, allocatable :: x(:)
	real, allocatable :: y(:)
	real, allocatable :: z(:)
	real, allocatable :: f(:)
	real, allocatable :: xx(:,:)
	real, allocatable :: yy(:,:)
	real, allocatable :: zz(:,:)
	real, allocatable :: ff(:,:)
	real, allocatable :: xxx(:,:,:)
	real, allocatable :: yyy(:,:,:)
	real, allocatable :: zzz(:,:,:)
	real, allocatable :: fff(:,:,:)
	real :: trap1d
	real :: simp1d
	real :: trap2d
	real :: simp2d
	real :: trap3d
	real :: simp3d
	real :: dx
	real :: dy
	real :: dz
	integer :: Nx = 5
	integer :: Ny = 5
	integer :: Nz = 5
	real :: ana1d = 14748.65627929928
	real :: ana2d = -9792.244966312954
	real :: ana3d = 120736.2020859017

   allocate(f(Nx))
   allocate(ff(Nx,Ny))
   allocate(fff(Nx,Ny,Nz))

	call linspace(x,1.,5.,Nx)
	call linspace(y,-5.,-1.,Ny)
	call linspace(z,-5.,5.,Nz)

	dx = x(2)-x(1)
	dy = y(2)-y(1)
	dz = z(2)-z(1)

	f = x**6 + x**5 + x**4 + x**3 + x**2 + x + sin(x) + exp(x) + 1.
	trap1d = trapQuad(f,dx)
	simp1d = simpQuad(f,dx)
	print *, trap1d, simp1d, ana1d
	print *, abs(trap1d-ana1d), abs(simp1d-ana1d)

	call meshgrid(x,y,xx,yy)
	ff = xx**3 + yy**5 + sin(xx)*cos(yy) + exp(xx*yy)
	trap2d = trapQuad(ff,dx,dy)
	simp2d = simpQuad(ff,dx,dy)
	print *, trap2d, simp2d, ana2d
	print *, abs(trap2d-ana2d), abs(simp2d-ana2d)
	
	call meshgrid(x,y,z,xxx,yyy,zzz)
	fff = -xxx**4*yyy + yyy**3 + xxx**3*zzz**2 + sin(xxx)*cos(zzz) + exp(xxx*yyy)
	trap3d = trapQuad(fff,dx,dy,dz)
	simp3d = simpQuad(fff,dx,dy,dz)
	print *, trap3d, simp3d, ana3d
	print *, abs(trap3d-ana3d), abs(simp3d-ana3d)

end program
