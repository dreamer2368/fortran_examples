program main

	use fourierTransform

	implicit none

	include 'fftw3.f'

	integer, parameter :: N = 64
	integer :: j
	real(mp) :: x(N), input(N)
	complex(mp) :: k, y(N), output(N/2+1)
	integer*8 :: plan

	! print to screen
	print *, 'calling program main'

	x = (/ ( SIN(2*pi*j/N), j=0,N-1) /)
	input = x
	y = 0.0_mp
	output = 0.0_mp

	call FFT1D(N,x,y)

	do j = 1,N
		print *, 'y(',j,')=',y(j)
	end do

	call dfftw_plan_dft_r2c_1d(plan,N,input,output,FFTW_ESTIMATE)
	call dfftw_execute_dft_r2c(plan,input,output)
	call dfftw_destroy_plan(plan)

	do j = 1,N/2+1
		print *, 'output(',j,')=',output(j)
	end do

	call dfftw_plan_dft_c2r_1d(plan,N,output,input,FFTW_ESTIMATE)
	call dfftw_execute_dft_c2r(plan,output,input)
	call dfftw_destroy_plan(plan)

	do j = 1,N
		print *, 'input(',j,')=',input(j)/N
	end do

	! print to screen
	print *, 'program main...done.'

contains

	! You can add custom subroutines/functions here later, if you want

end program
