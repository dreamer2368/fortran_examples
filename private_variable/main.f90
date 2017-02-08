program main

	implicit none

	! print to screen
	print *, 'calling program main'

	call test(3.0d+0)
	call test(4.0d+0)

	! print to screen
	print *, 'program main...done.'

contains

	subroutine test(input)
		real*8 :: internal=0.0
		real*8, intent(in) :: input

		internal = internal + input
		print *, 'internal: ',internal
	end subroutine

end program
