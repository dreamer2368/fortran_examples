module class

	implicit none

    type, abstract :: test
        real*8 :: a
!        procedure(func_type), pointer, pass :: func
	 contains
		procedure, pass :: func
    end type

	abstract interface
		subroutine func_type(this)
			import test
			implicit none
			class(test), intent(inout) :: this
		end subroutine
	end interface

	type, extends(test) :: test_extended
		real*8 :: b
	contains
		procedure, pass :: func=>func2
	end type

contains

	subroutine func(this)
        class(test), intent(inout) :: this

        this%a = 4.0
	end subroutine

    subroutine func2(this)
        class(test_extended), intent(inout) :: this

		  call func(this)
		  print *, this%a
        this%b = 8.0
        print *, this%b
    end subroutine

end module
