program main

	use MatrixVector

	implicit none

	real(mp), parameter :: pi = 4.0_mp*ATAN(1.0_mp)
	real(mp), parameter :: L = 1.0_mp
	integer, parameter :: N = 64
	real(mp), parameter :: q = 1.0_mp, rho_back = -q/L/L/L
	real(mp) :: dx(3) = (/ L/N, L/N, L/N /)
	integer :: Ng(3) = (/ N, N, N /)
	real(mp) :: x(N,N,N), rhs(N,N,N), y(N,N,N,3), solx(N,N,N), soly(N,N,N,3)
	real(mp) :: xg(N)
	integer :: i,j,k

	real(mp) :: start, finish

	! print to screen
	print *, 'calling program main'

	xg = (/ ( (i-1)*dx(1), i=1,N ) /)

	do i=1,N
		do j=1,N
			do k=1,N
				rhs(i,j,k) = -12.0_mp*pi*pi/L/L*SIN(2.0_mp*pi*xg(i)/L)*SIN(2.0_mp*pi*xg(j)/L)*SIN(2.0_mp*pi*xg(k)/L)
				soly(i,j,k,1) = 2.0_mp*pi/L*COS(2.0_mp*pi*xg(i)/L)*SIN(2.0_mp*pi*xg(j)/L)*SIN(2.0_mp*pi*xg(k)/L)
				soly(i,j,k,2) = 2.0_mp*pi/L*SIN(2.0_mp*pi*xg(i)/L)*COS(2.0_mp*pi*xg(j)/L)*SIN(2.0_mp*pi*xg(k)/L)
				soly(i,j,k,3) = 2.0_mp*pi/L*SIN(2.0_mp*pi*xg(i)/L)*SIN(2.0_mp*pi*xg(j)/L)*COS(2.0_mp*pi*xg(k)/L)
			end do
		end do
	end do
	solx = rhs/(-12.0_mp*pi*pi/L/L)


	call cpu_time(start)

	rhs(1,1,1) = 0.0_mp
	call CG_K(x,rhs,dx,Ng)

	y = Gradient(x,dx,Ng)

	call cpu_time(finish)

	print *, 'time = ', finish - start
	PRINT *, 'N = ', N
	print *, 'x error = ', MAXVAL( ABS(x-solx) )
	print *, 'y error = ', MAXVAL( ABS(y-soly) )


	! print to screen
	print *, 'program main...done.'

contains

	! You can add custom subroutines/functions here later, if you want

end program
