program main

	implicit none

	type varArray
		integer :: N
		real :: ch
		real, allocatable :: vec(:)
	end type

	type(varArray), allocatable :: element(:)
	integer :: nk = 16

	! print to screen
	print *, 'calling program main'

	allocate(element(3))

	call buildArray(element(1),nk)
	call buildArray(element(2),2*nk)
	call buildArray(element(3),3*nk)

	element(:)%ch = 3.0
	print *, size(element(1)%vec), size(element(2)%vec), size(element(3)%vec)
	print *, element(:)%ch

	if( allocated(element(1)%vec) ) then
		deallocate(element(1)%vec)
	end if
	allocate(element(1)%vec(4*nk))

	call destroyArray(element(1))
	call destroyArray(element(2))
	call destroyArray(element(3))

	deallocate(element)

	! print to screen
	print *, 'program main...done.'

contains

	! You can add custom subroutines/functions here later, if you want

	subroutine buildArray(this,N)
		type(varArray), intent(inout) :: this
		integer, intent(in) :: N

		allocate(this%vec(N))
	end subroutine

	subroutine destroyArray(this)
		type(varArray), intent(inout) :: this

		deallocate(this%vec)
	end subroutine

end program
