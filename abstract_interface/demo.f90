module demo

	implicit none

	interface smartRoutine
		subroutine smartRoutineX(x)
		end subroutine
		subroutine smartRoutineXY(x,y)
			real, intent(in) :: x,y
	end interface

contains

   subroutine smartRoutineX(x)
      real, intent(in) :: x

		print *, 'x: ',x
   end subroutine

   subroutine smartRoutineXY(x,y)
      real, intent(In) :: x,y
	
		print *, 'x+y: ',x+y
   end subroutine

end module
