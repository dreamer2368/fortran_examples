module ModModel

	use ModParameters
	use ModFluid
	use ModRk4
	use ModLib
   use ModConstants

	implicit none

contains

	subroutine init(Tinitial,YH,eqRat,Yi0)

		real, intent(in) :: Tinitial
		real, intent(in) :: YH
		real, intent(in) :: eqRat
		real, intent(out) :: Yi0(nSpec)
		real, parameter :: Y_O2_air = 0.231781 ! NIST
		real, parameter :: Y_N2_air = 0.755267 ! NIST
		real, parameter :: X_O2_air = 0.20946
		real, parameter :: X_N2_air = 0.78084
		real, parameter :: beta = Y_N2_air/Y_O2_air
		real, parameter :: mu = X_N2_air/X_O2_air ! Luca's value = 3.76
		real :: YH2
		real :: Y_O2
		real :: YH2O
		real :: YHO2
		real :: Y_N2
		type(fluid) :: fl

		YH2 = (1-YH)/(1+eqRat*stoichO2*(1+beta))!1d0/(1d0+stoich+7d0*mu)
		Y_O2 = eqRat*stoichO2*YH2
		YH2O = 0.0
		YHO2 = 0.0
		Y_N2 = beta*Y_O2

		Yi0(1) = YH2
		Yi0(2) = Y_O2
		Yi0(3) = YH
		Yi0(4) = YH2O
      Yi0(5) = Y_N2

	end subroutine

	subroutine runModel(nSteps,dt,T0,YH,eqRat,A3,E3,b3,nu21,nu22,nu23,verb,tig,&
      ignTemp,S,Tfinal)

      integer, intent(in) :: nSteps
      real, intent(in) :: dt
		real, intent(in) :: T0
		real, intent(in) :: YH
		real, intent(in) :: eqRat
		real, intent(in) :: A3
		real, intent(in) :: E3
		real, intent(in) :: nu21
		real, intent(in) :: nu22
		real, intent(in) :: nu23
		real, intent(in) :: b3
      integer, intent(in) :: verb
		real, intent(out) :: tig
		real, intent(out) :: ignTemp
		real, intent(out) :: S
		real, intent(out) :: Tfinal
		type(params) :: pm
		type(fluid) :: fl
		real :: Yi(nSpec) ! IC
		real :: y0(nSpec+1) ! IC; includes temperature
		real :: ign(2)
		integer :: i
      real :: nu21_fixed = 0.54434528887331124
      real :: nu22_fixed = 0.94454923438081684
      real :: nu23_fixed = 0.10957446195289232
      logical :: ex

      ! Initialize
		call init(T0,YH,eqRat,Yi)
		call buildFluid(fl,nSteps,dt,Yi,T0)
		call buildParams(pm,A3,E3,b3,nu21_fixed,nu22_fixed,nu23_fixed)

		! Run model
      y0(1:nSpec) = fl%Yi(1,:)
      y0(nSpec+1) = fl%h(1) 
		call rk4Solve(y0,rhsModel,pm,fl)

		! Output temperature, ignition time, ignition temperature
		call findIgnition(fl,ign)
		tig = ign(1)
		ignTemp = ign(2)
		S = trapQuad(fl%T,fl%dt)
		Tfinal = fl%T(fl%nSteps)

      ! Report one-line result to screen
      !$omp critical
      call report(T0,YH,eqRat,tig,ignTemp,S,Tfinal,'Model 0',verb)
		call writeModelData(pm,fl,verb)
      !$omp end critical

      ! Clean up
		call destroyFluid(fl)

	end subroutine

	function rhsModel(y,pm,fl) result(f)

		real :: y(:) ! (1:5) Yi_H2, Yi_O2, Yi_H, Yi_H2O, Yi_N2. (6) T
		type(params), intent(in) :: pm
		type(fluid), intent(in) :: fl
		real :: f(nSpec+1) ! (1:5) rhoDot for H2, O2, H, H2O, N2.  (6) Tdot
      real :: molarProgress(nReact)
      real :: H2Progress(nReact)
      real :: speciesProduction(nSpec)
      real :: Ci(nSpec)
      real :: Yi(nSpec)
      real :: Qg(nReact)
		real :: rho, R, p, T, cp, h
		real :: Qdot
      integer :: i

      do i = 1,size(y)
         if (y(i) < 1e-17) then
            y(i) = 0. ! Prevent from getting NaN = (small number)^nu in om1,ob3, or om3.
         end if
      end do

      Yi = y(1:nSpec)
      h = y(nSpec+1)
      p = fl%p
      cp = sum(Yi*cpi)
      T = h/cp
      R = sum(Yi*Ri)
      rho = p/(R*T)
      Ci = rho*Yi/MWi 

      ! Rate of Progress in kmol/(m^3*s).
		molarProgress(1) = pm%A1 * T**pm%b1 * exp(-pm%E1/(Ru*T)) * Ci(iH2)   * sum(pm%eff*Ci)
		molarProgress(2) = pm%A2 * T**pm%b2 * exp(-pm%E2/(Ru*T)) * Ci(iH)**2 * sum(pm%eff*Ci)
		molarProgress(3) = pm%A3 * T**pm%b3 * exp(-pm%E3/(Ru*T)) * Ci(iH2)**pm%nu21 &
         * Ci(iO2)**pm%nu22 * Ci(iH)**pm%nu23
      
      ! Progress variable, converted to units of H2 mass in each reaction.  kg/(m^3*s)
      H2Progress = pm%molarProgress_to_H2Progress * molarProgress

      ! Rate of species production in each reaction.  kg/(m^3*s).  i.e. rhoDot
      ! Check that speciesProduction1 == speciesProduction2
      speciesProduction = matmul(pm%massProductionPerH2Progress,H2Progress)

      ! Heat release for each reaction, per kg of H2 burned (J/kg)
      Qg(1) = pm%Qg1
      Qg(2) = -Qg(1)
		Qg(3) = R*gamma/(gamma-1.0)*(pm%Tflame-pm%Tref)*(stoichO2+7.0*N2_O2_airMoleRatio+1.0)
      Qdot = sum(H2Progress*Qg) ! Rate of total heat released

      ! Rate of mass fraction production.  1/s
      f(1:nSpec) = speciesProduction/rho
      f(nSpec+1) = Qdot/rho

100 format(a)
101 format(a,1es23.15)
102 format(a,2es23.15)
103 format(a,3es23.15)
104 format(a,4es23.15)
105 format(a,5es23.15)
106 format(a,6es23.15)
      if (.false.) then
         write(*,100) ''
         write(*,100) 'JONATHAN'
         write(*,101) 'T=',T
         write(*,101) 'rho=',rho
         write(*,104) 'Ci=',Ci(1:3),Ci(4)+Ci(5)
         write(*,103) 'A=',pm%A1,pm%A2,pm%A3
         write(*,103) 'theta=',pm%E1/Ru,pm%E2/Ru,pm%E3/Ru
         write(*,103) 'b=',pm%b1,pm%b2,pm%b3
         write(*,103) 'nu=',pm%nu21,pm%nu22,pm%nu23
         write(*,105) 'Third Body Eff=',pm%eff
         write(*,101) 'Third Body Conc=',sum(pm%eff*Ci)
         write(*,103) 'Qg=',Qg
         write(*,103) 'H2Progress=',H2Progress
         write(*,105) 'speciesProduction=',speciesProduction
         write(*,106) 'Qdot/Ru=',Qdot/Ru
         stop
      end if
         
	end function

	subroutine runCantera(nSteps,dt,T0,YH,eqRat,verb,tig,ignTemp,S,Tfinal)

      integer, intent(in) :: nSteps
      real, intent(in) :: dt
		real, intent(in) :: T0
		real, intent(in) :: YH
		real, intent(in) :: eqRat
      integer, intent(in) :: verb
		real, intent(out) :: tig
		real, intent(out) :: ignTemp
		real, intent(out) :: S
		real, intent(out) :: Tfinal
      type(fluid) :: fl
		real :: ign(2)
      real :: T
		real :: Yi(nSpec)
		real :: Xi(nSpec)
		real :: YiCantera(nSpecCantera)
      real :: canteraData(nVarsCantera*nSteps)
      integer :: i
      real :: tempData(3)

      ! Initialize
		call init(T0,YH,eqRat,Yi)
		call buildFluid(fl,nSteps,dt,Yi,T0)

      ! Run Cantera
      YiCantera = 0.
      YiCantera(iH2_Cantera-numberOfNonSpec) = Yi(iH2)
      YiCantera(iO2_Cantera-numberOfNonSpec) = Yi(iO2)
      YiCantera(iH_Cantera-numberOfNonSpec) = Yi(iH)
      YiCantera(iH2O_Cantera-numberOfNonSpec) = Yi(iH2O)
      YiCantera(iN2_Cantera-numberOfNonSpec) = Yi(iN2)
      call canteraKinetics(dt,nSteps,fl%p,T0,YiCantera,canteraData)

      ! Post-process by populating fluid object, then print.
      ! Note: Cantera solves in mole fractions.  Must convert
      do i = 1,nSteps
         T = canteraData((i-1)*nVarsCantera+iT_Cantera)
         Xi(1) = canteraData((i-1)*nVarsCantera+iH2_Cantera)
         Xi(2) = canteraData((i-1)*nVarsCantera+iO2_Cantera)
         Xi(3) = canteraData((i-1)*nVarsCantera+iH_Cantera)
         Xi(4) = canteraData((i-1)*nVarsCantera+iH2O_Cantera)
         Xi(5) = canteraData((i-1)*nVarsCantera+iN2_Cantera)
         Yi = (Xi*MWiCantera)/sum(Xi*MWiCantera)
         call updateFluidTemperature(fl,Yi,T,i)
      end do

		! Output temperature, ignition time, ignition temperature
		call findIgnition(fl,ign)
		tig = ign(1)
		ignTemp = ign(2)
		S = trapQuad(fl%T,fl%dt)
		Tfinal = fl%T(fl%nSteps)

!$omp critical
      call report(T0,YH,eqRat,tig,ignTemp,S,Tfinal,'Cantera',verb)
      call writeCanteraData(fl,verb)
!$omp end critical

!      tempData(1) = tig;
!      tempData(2) = ignTemp;
!      tempData(3) = S;
!      call printArray(tempData,'tempDataCantera.txt')
!      print *, 'T0=',fl%T(1),'; YH=',fl%Yi(1,3)
!      print *, 'tig=',tig,'ignTemp=',ignTemp,'Tfinal=',Tfinal,'S=',S

      ! Clean up
      call destroyFluid(fl)

   end subroutine

   subroutine findIgnition(fl,ign)
      type(fluid), intent(in) :: fl
      real, intent(out) :: ign(2)
      real, parameter :: ignTempThreshold = 1300.
      ign = findMaxSlope(fl%time,fl%T)
      if (ign(2) < ignTempThreshold) then
         ign(1) = 0. !fl%time(fl%nSteps)
         ign(2) = 0. !fl%T(fl%nSteps)
      end if
   end subroutine

	subroutine report(T0,YH,eqRat,tig,ignTemp,S,Tfinal,header,verb)

		real, intent(in) :: T0
		real, intent(in) :: YH
		real, intent(in) :: eqRat
		real, intent(in) :: tig
		real, intent(in) :: ignTemp
		real, intent(in) :: S
		real, intent(in) :: Tfinal
      character(*), intent(in) :: header
      integer, intent(in) :: verb

      if (verb >= 2) then
         write(*,'(a,a,a,$)') '[',header,']:  '
         write(*,'(a,f8.3,$)') 'T0=',T0
         write(*,'(a,es9.3,$)') ', YH=', YH
         write(*,'(a,f5.3,$)') ', eqRat=', eqRat
         write(*,'(a,es9.3,$)') ' || tig=',tig
         write(*,'(a,f6.3,$)') ', S=',S
         write(*,'(a,f8.3,$)') ', ignTemp=',ignTemp
         write(*,'(a,f8.3)')   ', Tfinal=',Tfinal
      end if

	end subroutine

	subroutine writeCanteraData(fl,verb)

		type(fluid), intent(in) :: fl
      integer, intent(in) :: verb
		integer :: j
		integer, parameter :: debugUnit = 1
		character(32) :: tecplotFile = 'tecplotCantera.txt'
		character(32) :: matlabFile = 'plotCantera.txt'
		character(32) :: debugFile = 'debugCantera.txt'
		real, allocatable :: tecplotArray(:,:)

		if (verb >= 3) then
         !call writeDataTecplot(fl,tecplotFile)
         call writeDataMatlab(fl,matlabFile)
		end if
		if (verb >= 4) then
			open(unit=debugUnit, file=debugFile, action='write', status='replace')
         write(debugUnit,*) '**** CANTERA RUN ****'
         call printFluid(fl,debugUnit)
			close(debugUnit)
			print *, 'File Written: ', debugFile
		end if

	end subroutine

	subroutine writeModelData(pm,fl,verb)

		type(params), intent(in) :: pm
		type(fluid), intent(in) :: fl
      integer, intent(in) :: verb
		integer :: j
		integer, parameter :: debugUnit = 1
		character(32) :: tecplotFile = 'tecplotModel.txt'
		character(32) :: matlabFile = 'plotModel.txt'
		character(32) :: debugFile = 'debugModel.txt'
		real, allocatable :: tecplotArray(:,:)

		if (verb >= 3) then
         !call writeDataTecplot(fl,tecplotFile)
         call writeDataMatlab(fl,matlabFile)
		end if
		if (verb >= 4) then
			open(unit=debugUnit, file=debugFile, action='write', status='replace')
         write(debugUnit,*) '**** MODEL 0 RUN ****'
         call printParams(pm,debugUnit)
         call printFluid(fl,debugUnit)
			close(debugUnit)
			print *, 'File Written: ', debugFile
		end if

	end subroutine

   subroutine writeDataMatlab(fl,filename)
      type(fluid), intent(in) :: fl
      character(*), intent(in) :: filename
      integer :: j
      real, allocatable :: matlabArray(:,:)
      allocate(matlabArray(fl%nSteps,nSpec+2)) ! Plus 2 for time and temperature
      matlabArray(:,1) = fl%time
      do j = 1,fl%nSteps
         matlabArray(j,2:nSpec+1) = fl%Yi(j,:)
      end do
      matlabArray(:,nSpec+2) = fl%T
      call printArray(matlabArray,filename)
   end subroutine

   subroutine writeDataTecplot(fl,filename)
		type(fluid), intent(in) :: fl
      character(*), intent(in) :: filename
		integer :: j
		real, allocatable :: tecplotArray(:,:)
      allocate(tecplotArray(fl%nSteps,nSpec+2)) ! Plus 2 for time and temperature
      tecplotArray(:,1) = fl%time
      do j = 1,fl%nSteps
         tecplotArray(j,2:nSpec+1) = fl%Yi(j,:)
      end do
      tecplotArray(:,nSpec+2) = fl%T
      call tecplotAscii(tecplotArray,filename,'"H2-Air Combustion"',&
         '"Time","YH2","Y_O2","YH","YH2O","Y_N2","Temperature"')
   end subroutine

end module

