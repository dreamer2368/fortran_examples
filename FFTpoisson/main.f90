program main

	use FastSolver
	use FFTSolver

	implicit none

	real(mp), parameter :: L = 2.0_mp
	integer, parameter :: N = 64

	real(mp) :: dx(3) = (/ L/N, L/N, L/N /)
	integer :: Ng(3) = (/ N, N, N /)

	real(mp) :: x(N,N,N), rhs(N,N,N), y(N,N,N,3), solx(N,N,N), soly(N,N,N,3)
	real(mp) :: xg(N), fg(N), f1g(N), f2g(N)
	real(mp) :: rho_back
	integer :: i,j,k

	complex(mp),dimension(N,N,N) :: W1
	complex(mp),dimension(N/2+1,N,N) :: W2
	integer(mp) :: plan

	real(mp) :: start, finish, m(3), msum

	! print to screen
	print *, 'calling program main'

	xg = (/ ( (i-1)*dx(1), i=1,N ) /)
!	fg = xg**3 - 1.5_mp*L*xg**2 + L**2/2.0_mp*xg
!	f1g = 3.0_mp*xg**2 - 3.0_mp*L*xg + L**2/2.0_mp
!	f2g = 6.0_mp*xg - 3.0_mp*L
!	fg = fg/MAXVAL(fg)
!	f1g = f1g/MAXVAL(fg)
!	f2g = f2g/MAXVAL(fg)

	m = (/ 1.0_mp, 3.0_mp, 5.0_mp /)
	msum = sum(m**2)
	do i=1,N
		do j=1,N
			do k=1,N
!				rhs(i,j,k) = -4.0_mp*pi*pi/L/L*msum*SIN(2.0_mp*pi*m(1)*xg(i)/L)*SIN(2.0_mp*pi*m(2)*xg(j)/L)*SIN(2.0_mp*pi*m(3)*xg(k)/L)
!				soly(i,j,k,1) = 2.0_mp*pi*m(1)/L*COS(2.0_mp*pi*m(1)*xg(i)/L)*SIN(2.0_mp*pi*m(2)*xg(j)/L)*SIN(2.0_mp*pi*m(3)*xg(k)/L)
!				soly(i,j,k,2) = 2.0_mp*pi*m(2)/L*SIN(2.0_mp*pi*m(1)*xg(i)/L)*COS(2.0_mp*pi*m(2)*xg(j)/L)*SIN(2.0_mp*pi*m(3)*xg(k)/L)
!				soly(i,j,k,3) = 2.0_mp*pi*m(3)/L*SIN(2.0_mp*pi*m(1)*xg(i)/L)*SIN(2.0_mp*pi*m(2)*xg(j)/L)*COS(2.0_mp*pi*m(3)*xg(k)/L)

!				rhs(i,j,k) = f2g(i)*fg(j)*fg(k) + fg(i)*f2g(j)*fg(k) + fg(i)*fg(j)*f2g(k)
!				soly(i,j,k,1) = f1g(i)*fg(j)*fg(k)
!				soly(i,j,k,2) = fg(i)*f1g(j)*fg(k)
!				soly(i,j,k,3) = fg(i)*fg(j)*f1g(k)
!				solx(i,j,k) = fg(i)*fg(j)*fg(k)
			end do
		end do
	end do
!	solx = rhs/(-4.0_mp*pi*pi*msum/L/L)

	rhs(N/2,N/2,N/2) = 1.0_mp/PRODUCT(dx)
	rho_back = -1.0_mp/L/L/L
	rhs = rhs + rho_back

	call cpu_time(start)

	print *, '==Fast Poisson Solver=='

	call FastPoisson_setup(Ng,dx,W2)
	call FastPoisson(x,rhs,W2)

	y = Gradient(x,dx,Ng)

	call cpu_time(finish)
	print *, 'time = ', finish - start
	print *, 'x error = ', MAXVAL( ABS(x-solx) )
	print *, 'FD : y error = ', MAXVAL( ABS(y-soly) )

	solx = x
	soly = y

	call cpu_time(start)
	print *, '==FFT Poisson Solver=='

	call FFTPoisson_setup(Ng,(/L,L,L/),W1)
	call FFTPoisson(x,rhs,W1)
	y = Gradient(x,dx,Ng)
	call cpu_time(finish)
	print *, 'time = ', finish - start
	print *, 'x error = ', MAXVAL( ABS(x-solx) )
	print *, 'FD : y error = ', MAXVAL( ABS(y-soly) )

	solx = x
	soly = y

	call forwardFFT(y(:,:,:,1),Ng)

	call cpu_time(start)
	call FFTEfield(y,rhs,W1,(/L,L,L/))
	call cpu_time(finish)
	print *, 'time = ', finish - start

	print *, 'PoissonGrad : y error = ', MAXVAL( ABS(y-soly) )

	open(unit=301,file='data/readme.out',status='replace')
	write(301,*) L,N
	close(301)

	open(unit=301,file='data/x.bin',status='replace',form='unformatted',access='stream')
	write(301) x
	close(301)

	open(unit=301,file='data/y.bin',status='replace',form='unformatted',access='stream')
	write(301) y
	close(301)

	open(unit=301,file='data/rhs.bin',status='replace',form='unformatted',access='stream')
	write(301) rhs
	close(301)

	open(unit=301,file='data/solx.bin',status='replace',form='unformatted',access='stream')
	write(301) solx
	close(301)

	open(unit=301,file='data/soly.bin',status='replace',form='unformatted',access='stream')
	write(301) soly
	close(301)

	! print to screen
	print *, 'program main...done.'

contains

	! You can add custom subroutines/functions here later, if you want

end program