program main

	implicit none

	type test
		real :: a
		procedure(func), pass(this), pointer :: f_ptr
	end type

	abstract interface
		subroutine func(this)
			import :: test
			class(test), intent(inout) :: this
		end subroutine
	end interface

	type(test) :: x

	x%f_ptr=>func1
	CALL x%f_ptr
	print *, x%a

	x%f_ptr=>func2
	CALL x%f_ptr
	print *, x%a

	! print to screen
	print *, 'program main...done.'

contains

	subroutine func1(this)
		implicit none
		class(test), intent(inout) :: this

		this%a=2.0d0
	end subroutine

	subroutine func2(this)
		implicit none
		class(test), intent(inout) :: this

		this%a=4.0d0
	end subroutine

end program
