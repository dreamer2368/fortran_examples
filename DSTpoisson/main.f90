program main

	use constants

	implicit none

	include 'fftw3.f'

	real(mp), parameter :: L = 2.0_mp
	integer, parameter :: N = 64

	real(mp) :: dx = L/(N-1)
	integer :: Ng = N

	real(mp), dimension(N) :: phi, rhs, solphi, xg, fg, f1g, f2g
	real(mp), dimension(N-1) :: phiFFT, rhsFFT, phib
	integer :: i,j,k,wk

	complex(mp) :: W(N-1), wx
	integer(mp) :: plan

	real(mp) :: start, finish

	! print to screen
	print *, 'calling program main'

	wx = pi*eye/(L)
	do k=1,N-1
		wk = k
		W(k) = (wx*(wk-0.5_mp))**2
	end do

	xg = (/ (i,i=0,N-1) /)*dx
	rhs = -pi*pi/(2*L)/(2*L)*SIN(pi*(xg)/2/L)
!	rhs = 1.0_mp
	call dfftw_plan_r2r_1d(plan,N-1,rhs(2:N),rhsFFT,FFTW_RODFT01,FFTW_ESTIMATE)
	call dfftw_execute_r2r(plan,rhs(2:N),rhsFFT)
	call dfftw_destroy_plan(plan)

!	print *, rhsFFT

	phiFFT = rhsFFT/REALPART(W)

	call dfftw_plan_r2r_1d(plan,N,phiFFT,phib,FFTW_RODFT10,FFTW_ESTIMATE)
	call dfftw_execute_r2r(plan,phiFFT,phib)
	call dfftw_destroy_plan(plan)
print *, phib
	phi = (/0.0_mp, phib/)/2/N

	open(unit=301,file='data/phi.bin',status='replace',form='unformatted',access='stream')
	write(301) phi
	close(301)

!	solphi = 0.5_mp*xg*(xg-2.0_mp*L)
	solphi = SIN(pi*(xg-0.5_mp*dx)/2/L)
	print *, (sum((solphi-phi)**2)/N)**(1.0_mp/2.0_mp)

	! print to screen
	print *, 'program main...done.'

contains

	! You can add custom subroutines/functions here later, if you want

end program