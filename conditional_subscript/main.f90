program main

	implicit none

	integer :: i,nseed,clock
	integer, allocatable :: seed(:)
	real*8 :: ii,sum_i, n(8), m(8)

	! print to screen
	print *, 'calling program main'

	n = (/ (1.7d0*i,i=1,8) /)
	m = (/ (-2.9d0*i,i=1,8) /)
	print *, (n>5.0d0)
	print *, PACK(m,(n>5.0d0))

	! print to screen
	print *, 'program main...done.'

contains

end program
