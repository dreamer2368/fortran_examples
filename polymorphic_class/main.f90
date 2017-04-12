program main

	implicit none

	type :: Base
		real :: a
		procedure(func), pass(this), pointer :: f_ptr
	end type

	type, extends(Base) :: test
		real :: b
	end type

	abstract interface
		subroutine func(this)
			import :: Base
			class(Base), intent(inout) :: this
		end subroutine
	end interface

	type(Base) :: x
	type(test) :: y

	x%f_ptr=>func1
	CALL x%f_ptr
	print *, x%a

	x%f_ptr=>func2
	CALL x%f_ptr
	print *, x%a

	y%a = 2.0d0
	print *, y%a

	y%f_ptr=>func2
	CALL y%f_ptr
	print *, y%a

	! print to screen
	print *, 'program main...done.'

contains

	subroutine func1(this)
		implicit none
		class(Base), intent(inout) :: this

		this%a=2.0d0
	end subroutine

	subroutine func2(this)
		implicit none
		class(Base), intent(inout) :: this

		this%a=4.0d0
	end subroutine

end program
