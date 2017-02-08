program main

	implicit none

	integer :: i,nseed,clock
	integer, allocatable :: seed(:)
	real*8 :: xp(2)

	! print to screen
	print *, 'calling program main'

	open(unit=305,file='0_1.bin',form='unformatted',access='stream')
	read(305) xp
	close(305)

	print *, xp

	! print to screen
	print *, 'program main...done.'

contains

end program
