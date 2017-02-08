subroutine linspace(x,xmin,xmax,N)
	real, intent(in) :: xmin
	real, intent(in) :: xmax
	integer, intent(in) :: N
	real, intent(out) :: x(N)
	integer :: i
	if (N==1) then
		x(1) = (xmax+xmin)/2.0
	else
		do i = 1,N
			x(i) = xmin + (i-1)*(xmax-xmin)/(N-1)
		end do	
	end if
end subroutine 

subroutine logspace(x,xmin,xmax,N)
   real, intent(in) :: xmin
   real, intent(in) :: xmax
   integer, intent(in) :: N
   real, intent(out) :: x(N)
   integer :: i
   real :: base
   real :: rmin,rmax
   rmin = log10(xmin)
   rmax = log10(xmax)
   if (N==1) then
      x(1) = 10**((rmax+rmin)/2.0)
   else
      do i = 1,N
         x(i) = 10**(rmin+(i-1)*(rmax-rmin)/(N-1))
      end do
   end if
end subroutine

function increment(start,incr,finish) result(x)
	real :: start
	real :: incr
	real :: finish
	real, allocatable :: x(:)
	integer :: N
	integer :: i
	N = floor(abs(finish-start)/incr) + 1
	allocate(x(N))
	do i = 1,N
		x(i) = start + (i-1)*incr
	end do
end function

subroutine meshgrid2d(x,y,xx,yy)
	real, intent(in) :: x(:)
	real, intent(in) :: y(:)
	real, allocatable, intent(out) :: xx(:,:)
	real, allocatable, intent(out) :: yy(:,:)
	integer :: i
	integer :: Nx
	integer :: Ny
	Nx = size(x)
	Ny = size(y)
	allocate(xx(Nx,Ny))
	allocate(yy(Nx,Ny))
	do i = 1,Ny
		xx(:,i) = x
	end do
	do i = 1,Nx
		yy(i,:) = y
	end do
end subroutine 

subroutine meshgrid3d(x,y,z,xx,yy,zz)
	real, intent(in) :: x(:)
	real, intent(in) :: y(:)
	real, intent(in) :: z(:)
	real, allocatable, intent(out) :: xx(:,:,:)
	real, allocatable, intent(out) :: yy(:,:,:)
	real, allocatable, intent(out) :: zz(:,:,:)
	integer :: i
	integer :: j
	integer :: Nx
	integer :: Ny
	integer :: Nz
	Nx = size(x)
	Ny = size(y)
	Nz = size(z)
	allocate(xx(Nx,Ny,Nz))
	allocate(yy(Nx,Ny,Nz))
	allocate(zz(Nx,Ny,Nz))
	do i = 1,Ny
		do j = 1,Nz
			xx(:,i,j) = x
		end do
	end do
	do i = 1,Nx
		do j = 1,Nz
			yy(i,:,j) = y
		end do
	end do
	do i = 1,Nx
		do j = 1,Ny
			zz(i,j,:) = z
		end do
	end do
end subroutine 

