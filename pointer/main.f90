program main

	implicit none

	real, dimension(:), pointer :: input=>NULL()
	real, pointer :: ptr
	integer :: g,i

	! print to screen
	print *, 'calling program main'

	ALLOCATE(input(4))
	input = (/ 0.2, 0.5, 0.8, 1.1 /)

	print *, input, g

	ptr=>input(g)
	do i=1,4
		g=i
		print *, g, ptr
	end do

	do i=1,4
		g=i
		ptr=>input(g)
		print *, g, ptr
	end do

	! print to screen
	print *, 'program main...done.'

contains

end program
