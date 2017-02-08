module ModDemo

	implicit none

	real, private :: privateVariable ! Accessible only be routines in this module
	real, public :: publicVariable ! Accessible by any file that has "use ModDemo"

contains
	! All subroutines/functions in this section are accessible by any file that has "use ModDemo"

	! Sets privateVariable
	subroutine setPrivateVariable(x)
		real, intent(in) :: x
		privateVariable = x	
	end subroutine	

	! Returns privateVariable
	function getPrivateVariable() result(x)
		real :: x ! Does not have intent because it's not a formal argument
		x = privateVariable
	end function

	! Subtracts the mean from each element of vector v, using a subroutine
	subroutine incrementByPrivateVariable(v)
		real, intent(inout) :: v(:)
		v = v + privateVariable
	end subroutine

	! Subtracts the mean from each element of vector v, using a function
	! This requires allocation of another array
	function addPrivateVariable(v) result(w)
		real, intent(in) :: v(:)
		real :: w(size(v)) ! has no intent, because it's not a formal argument
		w = v + privateVariable
	end function

end module
