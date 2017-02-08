program main

    use class

	implicit none

    type(test_extended) :: hello

	! print to screen
	print *, 'calling program main'

!	hello%func=>func1

    call hello%func
    print *, hello%a

!	hello%func => func2
	print *, hello%a

	! print to screen
	print *, 'program main...done.'

contains

end program
