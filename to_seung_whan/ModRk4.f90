module ModRK4

	use ModFluid
	use ModParameters

	implicit none

contains

	subroutine rk4Solve(y,rhs,pm,fl)

      real, intent(inout) :: y(:)
		type(fluid), intent(inout) :: fl
		type(params), intent(in) :: pm
		integer :: i
		real :: dt
		real :: k1(nSpec+1)
		real :: k2(nSpec+1)
		real :: k3(nSpec+1)
		real :: k4(nSpec+1)
      real :: f(nSpec+1)
		integer :: nSteps
		integer :: nSpec

		interface
			function rhs(y,pm,fl)
				import :: fluid
				import :: params
				real :: y(:)
				type(fluid), intent(in) :: fl
				type(params), intent(in) :: pm
				real :: rhs(:)
			end function
		end interface

		dt = fl%dt
		nSpec = fl%nSpec
		nSteps = fl%nSteps

		do i = 2,nSteps
			k1 = dt * rhs(y,pm,fl)
			k2 = dt * rhs(y+0.5*k1,pm,fl)
			k3 = dt * rhs(y+0.5*k2,pm,fl)
			k4 = dt * rhs(y+k3,pm,fl)
			y = y+1.0/6.0*(k1 + 2.0*k2 + 2.0*k3 + k4)
			call updateFluidEnthalpy(fl,y(1:nSpec),y(nSpec+1),i)	
		end do

	end subroutine

end module
