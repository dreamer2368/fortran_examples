module ModFluid

   use ModLib
   use ModConstants

	implicit none

	type fluid
		integer :: nSteps  ! number of steps in time, including initial condition
		integer :: nSpec ! number of species
		real :: dt ! s
		real :: p ! Pressure, only one value b/c isobaric. N/m^2
		real, allocatable :: time(:) ! s
		real, allocatable :: Yi(:,:) ! Mass fraction at every time step.  Unitless
		real, allocatable :: h(:) ! Enthalpy.  J/kg
		real, allocatable :: T(:) ! Temperature at every time step, K
		real, allocatable :: rho(:) ! Total density. kg/m^3
		real, allocatable :: MW(:) ! Bulk molecular weight. kg/kmol
		real, allocatable :: cp(:) ! Bulk specific heat. J/(kg*K)
		real, allocatable :: R(:) ! Bulk gas constant. J/(kg*K)
	end type

contains

	subroutine buildFluid(this,nSteps,dt,Yi0,T0)

		type(fluid), intent(out) :: this
		integer, intent(in) :: nSteps
		real, intent(in) :: dt
		real, intent(in) :: Yi0(:)
		real, intent(in) :: T0
		integer :: i

      this%nSpec = nSpec
		this%nSteps = nSteps
		this%dt = dt

		allocate(this%time(this%nSteps))
		allocate(this%Yi(this%nSteps,this%nSpec))
		allocate(this%T(this%nSteps))
		allocate(this%h(this%nSteps))
		allocate(this%rho(this%nSteps))
		allocate(this%MW(this%nSteps))
		allocate(this%R(this%nSteps))
		allocate(this%cp(this%nSteps))

      ! LM
		this%time(1) = 0
		do i = 2,this%nSteps
			this%time(i) = this%time(i-1) + this%dt
		end do
		this%p = atmospheric

      call updateFluidTemperature(this,Yi0,T0,1)

	end subroutine

	subroutine updateFluidTemperature(this,Yi,T,i)

		type(fluid), intent(inout) :: this
		real, intent(in) :: Yi(:)
		real, intent(in) :: T
		integer, intent(in) :: i

		this%Yi(i,:) = Yi
      this%T(i) = T

		this%MW(i) = 1.0/sum(Yi/MWi)
		this%R(i) = sum(Yi*Ri)
		this%cp(i) = sum(Yi*cpi)

      this%h(i) = this%cp(i)*this%T(i)
		this%rho(i) = this%p/(this%R(i)*this%T(i))
			
	end subroutine

	subroutine updateFluidEnthalpy(this,Yi,h,i)

		type(fluid), intent(inout) :: this
		real, intent(in) :: Yi(:)
		real, intent(in) :: h
		integer, intent(in) :: i

		this%Yi(i,:) = Yi
      this%h(i) = h

		this%MW(i) = 1.0/sum(Yi/MWi)
		this%R(i) = sum(Yi*Ri)
		this%cp(i) = sum(Yi*cpi)

      this%T(i) = this%h(i)/this%cp(i)
		this%rho(i) = this%p/(this%R(i)*this%T(i))
			
	end subroutine

	subroutine destroyFluid(this)
		type(fluid), intent(inout) :: this
		deallocate(this%time)
		deallocate(this%T)
		deallocate(this%h)
		deallocate(this%MW)
		deallocate(this%cp)
		deallocate(this%R)
		deallocate(this%rho)
	end subroutine

   subroutine printFluid(fl,un)
      type(fluid), intent(in) :: fl
      integer, intent(in) :: un
      integer :: i
      100 format(es22.16,a,$)
      101 format(es10.4,a,$)
      102 format(i7,a,$)
      write(un,*) 'Fluid:  '
      write(un,*) 'dt:', fl%dt
      write(un,*) 'nSteps:',fl%nSteps
      write(un,*) 'p:', fl%p
      write(un,'(a,$)') '   Step  '
      write(un,'(a,$)') '   Time    '
      write(un,'(a,$)') '         YH2           '
      write(un,'(a,$)') '         Y_O2           '
      write(un,'(a,$)') '         YH            '
      write(un,'(a,$)') '         YH2O          '
      write(un,'(a,$)') '         Y_N2           '
      write(un,'(a,$)') '      Temperature       '
      write(un,'(a,$)') '        Density         '
      write(un,'(a,$)') '        Pressure        '
      write(un,'(a,$)') '       Y_O2/YH2        '
      write(un,'(a)')   '       Mass Loss        '
      do i = 1,fl%nSteps
         write(un,102) i,'  '
         write(un,101) fl%time(i),'  '
         write(un,100) fl%Yi(i,1),'  '
         write(un,100) fl%Yi(i,2),'  '
         write(un,100) fl%Yi(i,3),'  '
         write(un,100) fl%Yi(i,4),'  '
         write(un,100) fl%Yi(i,5),'  '
         write(un,100) fl%T(i),'  '
         write(un,100) fl%rho(i),'  '
         write(un,100) fl%rho(i)*fl%R(i)*fl%T(i),'  '
         write(un,100) fl%Yi(i,2)/fl%Yi(i,1),'  '
         write(un,100) checkMass(fl,i),'  '
         write(un,*) ''
      end do
   end subroutine

	function checkMass(fl,i) result(diff)
		type(fluid), intent(in) :: fl
		integer, intent(in) :: i
		real :: diff
		diff = abs(sum(fl%Yi(i,:)) - sum(fl%Yi(1,:)))
	end function

end module

