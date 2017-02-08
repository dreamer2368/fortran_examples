function trapQuad1d(f,dx) result(S)
	real, intent(in) :: f(:)
	real, intent(in) :: dx
	real :: S
	integer :: N
	N = size(f)
	S = dx/2.*(f(1)+2.*sum(f(2:N-1))+f(N))
end function

function trapQuad2d(f,dx,dy) result(S)
	real, intent(in) :: f(:,:)
	real, intent(in) :: dx
	real, intent(in) :: dy
	real :: S
	integer :: Ny
	integer :: i
	Ny = size(f,2)
	S = 0
	do i = 2,Ny-1
		S = S + 2.*trapQuad(f(:,i),dx)
	end do
	S = dy/2.*(trapQuad1d(f(:,1),dx) + S + trapQuad1d(f(:,Ny),dx))
end function

function trapQuad3d(f,dx,dy,dz) result(S)
	real, intent(in) :: f(:,:,:)
	real, intent(in) :: dx
	real, intent(in) :: dy
	real, intent(in) :: dz
	real :: S
	integer :: Nx
	integer :: Ny
	integer :: Nz
	integer :: i
	Nx = size(f,1)
	Ny = size(f,2)
	Nz = size(f,2)
	S = 0.
	do i = 2,Nz-1
		S = S + 2.*trapQuad2d(f(:,:,i),dx,dy)
	end do
	S = dz/2.*(trapQuad2d(f(:,:,1),dx,dy) + S + trapQuad2d(f(:,:,Nz),dx,dy))
end function

function simpQuad1d(f,dx) result(S)
	real, intent(in) :: f(:)
	real, intent(in) :: dx
	real :: S
	integer :: N
	integer :: i
!	print *, 'SIMPSON"S QUADRATURE NEEDS TO BE TESTED AND DEBUGGED.'
!	stop
	N = size(f)
	S = 0.
	if (mod(N,2)==0) then
		print *, 'ERROR: Simpson"s quadrature needs an odd number of points. S = 0'
	else
		do i = 2,N-1
			if (mod(i,2)==1) then
				S = S + 2.*f(i)
			elseif (mod(i,2)==0) then
				S = S + 4.*f(i)
			end if
		end do
		S = dx/3.*(f(1) + S + f(N))
	end if
end function

function simpQuad2d(f,dx,dy) result(S)
	real, intent(in) :: f(:,:)
	real, intent(in) :: dx
	real, intent(in) :: dy
	real :: S
	integer :: Nx
	integer :: Ny
	integer :: i
	Nx = size(f,1)
	Ny = size(f,2)
	S = 0.
	if (mod(Nx,2)==0 .or. mod(Ny,2)==0) then
		print *, 'ERROR: Simpson"s quadrature needs an odd number of points. S = 0'
	else
		do i = 2,Ny-1
			if (mod(i,2)==1) then
				S = S + 2.*simpQuad(f(:,i),dx)
			elseif (mod(i,2)==0) then
				S = S + 4.*simpQuad(f(:,i),dx)
			end if
		end do
		S = dy/3.*(simpQuad(f(:,1),dx) + S + simpQuad(f(:,Ny),dx))
	end if
end function

function simpQuad3d(f,dx,dy,dz) result(S)
	real, intent(in) :: f(:,:,:)
	real, intent(in) :: dx
	real, intent(in) :: dy
	real, intent(in) :: dz
	real :: S
	integer :: Nx
	integer :: Ny
	integer :: Nz
	integer :: i
	Nx = size(f,1)
	Ny = size(f,2)
	Nz = size(f,3)
	S = 0.
	if (mod(Nx,2)==0 .or. mod(Ny,2)==0 .or. mod(Nz,2)==0) then
		print *, 'ERROR: Simpson"s quadrature needs an odd number of points. S = 0'
	else
		do i = 2,Nz-1
			if (mod(i,2)==1) then
				S = S + 2.*simpQuad(f(:,:,i),dx,dy)
			elseif (mod(i,2)==0) then
				S = S + 4.*simpQuad(f(:,:,i),dx,dy)
			end if
		end do
		S = dz/3.*(simpQuad(f(:,:,1),dx,dy) + S + simpQuad(f(:,:,Nz),dx,dy))
	end if
end function
