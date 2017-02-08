program main

	implicit none

	integer :: i,nseed,clock
	integer, allocatable :: seed(:)
	real*8 :: ii,sum_i, n

	! print to screen
	print *, 'calling program main'

	call RANDOM_SEED(size=nseed)
	allocate(seed(nseed))
	call SYSTEM_CLOCK(COUNT=clock)
	seed = clock + 127*(/ ( i, i=1,nseed ) /)
	call RANDOM_SEED(put=seed)
	deallocate(seed)
	call RANDOM_NUMBER(n)
	n = n*10

	ii = 2.0
	print *, n
	if( n .le. ii ) then
		print *, 'n is less than or equal to 2.0'
	else
		ii = ii+4.0
		if( n .le. ii ) then
			print *, 'n is between 2.0 and 6.0'
		else
			print *, 'n is more than 6.0'
		end if
	end if

	! print to screen
	print *, 'program main...done.'

contains

end program
