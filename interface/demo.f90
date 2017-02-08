module demo

   interface smartRoutine
      procedure smartRoutineScalar
      procedure smartRoutineVector
   end interface

type test
	real :: a
end type

contains

	subroutine testQoI(a,b)
		real, intent(in) :: a
		real, intent(out) :: b

		b = a**3
	end subroutine

	subroutine testSubroutine( QoI, a, b )
		real, intent(in) :: a
		real, intent(out) :: b
		type(test) :: testinput
		interface
			subroutine QoI(a,b)
				real, intent(in) :: a
				real, intent(out) :: b
			end subroutine
		end interface

		call QoI(a,b)
		b = b+a
	end subroutine

   subroutine smartRoutineScalar(a,b,x)
      real, intent(in) :: a
      real, intent(in) :: b
      real, intent(out) :: x
      x = a + b
   end subroutine

   subroutine smartRoutineVector(a,b,v)
      real, intent(In) :: a
      real, intent(in) :: b
      real, intent(out) :: v(:)
      v = a - b
   end subroutine

end module
