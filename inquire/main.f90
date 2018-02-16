program main

	implicit none

    logical :: fileExists
    character(len=100) :: filename

	! print to screen
	print *, 'calling program main'

    filename = ""
    inquire(file = trim(filename), exist = fileExists)
    print *, 'File '//trim(filename)//': ',fileExists

    filename = "main.f90"
    inquire(file = trim(filename), exist = fileExists)
    print *, 'File '//trim(filename)//': ',fileExists

	! print to screen
	print *, 'program main...done.'

contains

end program
