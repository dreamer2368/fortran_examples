module ModParameters

   use ModLib
   use ModConstants

	implicit none

	type params
      real :: A1
      real :: A2
      real :: A3
      real :: b1
      real :: b2
      real :: b3
      real :: E1
      real :: E2
      real :: E3
      real :: nu21
      real :: nu22
      real :: nu23
		real :: Tref
		real :: Tflame
      real :: Qg1
      real, allocatable :: eff(:)
      real, allocatable :: massProductionPerH2Progress(:,:)
      real, allocatable :: stoichCoeffs(:,:)
      real, allocatable :: molarProgress_to_H2Progress(:)
	end type

contains

	subroutine buildParams(this,A3,E3,b3,nu21,nu22,nu23)
   ! A1,A2,b1,b2,E1,E2 are pulled from model2.xml (Luca).  All units are in MKS unless
   ! otherwise denoted.

		type(params) :: this
		real, intent(in) :: A3
		real, intent(in) :: E3
		real, intent(in) :: b3
		real, intent(in) :: nu21
		real, intent(in) :: nu22
		real, intent(in) :: nu23
      real, parameter :: A1 = 4.58e16
      real, parameter :: A2 = 1e12
      real, parameter :: E1 = 1.0439e5 ! cal/mol
      real, parameter :: E2 = 0.0 ! cal/mol
      real, parameter :: b1 = -1.4
      real, parameter :: b2 = -1.0

      this%A1 = A1 
      this%A2 = A2
      this%A3 =  A3
      this%E1 = E1 * cal_to_joule / mol_to_kmol
      this%E2 = E2 * cal_to_joule / mol_to_kmol
      this%E3 = E3 * cal_to_joule / mol_to_kmol
      this%b1 = b1
      this%b2 = b2
      this%b3 = b3
      this%nu21 = nu21
      this%nu22 = nu22
      this%nu23 = nu23

      this%Qg1 = -217.986263e6
		this%Tref = 298.0
		this%Tflame = 2200.0

      ! Third-body collisional efficiency
      ! N2 efficiency should be 1.0, but Luca is using 12.0 to avoid tracking N2 and H2O
      allocate(this%eff(nSpec))
      this%eff = (/2.5, 1.0, 1.0, 12.0, 1.0/)

      ! Reactions
      ! Reaction 1:       H2 + M => 2H + M
      ! Reaction 2:       2H + M => H2 + M
      ! Reaction 3: 2H2 + O2 + H => 2H2O + H

      ! Has units of kg/(m^3*s).  Represents the rate of progress variable for each
      ! reaction, in terms of the amount of H2 that is involved. The units are
      ! derived by multiplying molar progres (kmol/(m^3*s)) by the product of
      ! the molecular weight of H2 by H2's stoichiometric coeff in each reaction.
      ! Note the this has NO SIGN.  The progress variable may be pos or neg, but
      ! the vector below is only a scaling factor.
      allocate(this%molarProgress_to_H2Progress(nReact))
      this%molarProgress_to_H2Progress(1) = 1.0*MWi(1)
      this%molarProgress_to_H2Progress(2) = 1.0*MWi(1)
      this%molarProgress_to_H2Progress(3) = 2.0*MWi(1)

      ! Dimensionless.  For each reaction, gives the mass production of each species 
      ! relative to the H2-mass progress variable.
      allocate(this%massProductionPerH2Progress(nSpec,nReact))
      this%massProductionPerH2Progress(:,1) = (/-1.0, 0.0, 1.0, 0.0, 0.0/)
      this%massProductionPerH2Progress(:,2) = (/ 1.0, 0.0,-1.0, 0.0, 0.0/)
      this%massProductionPerH2Progress(:,3) = (/-1.0, -stoichO2, 0.0, stoichH2O, 0.0/)

      ! Stoichiometric coefficients
      allocate(this%stoichCoeffs(nSpec,nReact))
      this%stoichCoeffs(:,1) = (/-1.0, 0.0, 2.0, 0.0, 0.0/)
      this%stoichCoeffs(:,2) = (/ 1.0, 0.0,-2.0, 0.0, 0.0/)
      this%stoichCoeffs(:,3) = (/-2.0,-1.0, 0.0, 2.0, 0.0/)

	end subroutine

   subroutine printParams(pm,un)
      type(params), intent(in) :: pm
      integer, intent(in) :: un

      write(un,*) 'A1=',pm%A1
      write(un,*) 'A2=',pm%A2
      write(un,*) 'A3=',pm%A3
      write(un,*) 'b1=',pm%b1
      write(un,*) 'b2=',pm%b2
      write(un,*) 'b3=',pm%b3
      write(un,*) 'E1=',pm%E1
      write(un,*) 'E2=',pm%E2
      write(un,*) 'E3=',pm%E3
      write(un,*) 'nu21=',pm%nu21
      write(un,*) 'nu22=',pm%nu22
      write(un,*) 'nu23=',pm%nu23
		write(un,*) 'Tref=',pm%Tref
		write(un,*) 'Tflame=',pm%Tflame
      write(un,*) 'Qg1=',pm%Qg1
      write(un,*) 'massProductionPerH2Progress='
      call printArray(pm%massProductionPerH2Progress,un)
      write(un,*) 'molarProgress_to_H2Progress='
      call printArray(pm%molarProgress_to_H2Progress,un)
      write(un,*) 'stoichCoeffs='
      call printArray(pm%stoichCoeffs,un)

   end subroutine

end module
