program ProgInit

   use ModLib
   use ModConstants

   ! Calibration points (initial conditions)
   real, parameter :: T0_calib_bnd(2) = (/901.0,1201.0/)
   real, parameter :: YH_calib_bnd(2) = (/1e-12,1e-4/)
   real, parameter :: eqRat_calib_bnd(2) = (/0.8,1.2/)
	integer, parameter :: nT0_calib = 5
	integer, parameter :: nYH_calib = 5
	integer, parameter :: nEqRat_calib = 5
   integer, parameter :: nCalib = nT0_calib*nYH_calib*nEqRat_calib
	real :: T0_calib(nT0_calib)
	real :: YH_calib(nYH_calib)
	real :: eqRat_calib(nEqRat_calib)

   ! Evaluation points (initial conditions)
   real, parameter :: T0_eval_bnd(2) = T0_calib_bnd
   real, parameter :: YH_eval_bnd(2) = YH_calib_bnd
   real, parameter :: eqRat_eval_bnd(2) = eqRat_calib_bnd
	integer, parameter :: nT0_eval = 5
	integer, parameter :: nYH_eval = 5
	integer, parameter :: nEqRat_eval = 5
	real :: T0_eval(nT0_eval)
	real :: YH_eval(nYH_eval)
	real :: eqRat_eval(nEqRat_eval)

   ! Fine grid (for plotting purposes)
   real, parameter :: T0_fine_bnd(2) = T0_calib_bnd
   real, parameter :: YH_fine_bnd(2) = YH_calib_bnd
   real, parameter :: eqRat_fine_bnd(2) = eqRat_calib_bnd
	integer, parameter :: nT0_fine = 5
	integer, parameter :: nYH_fine = 5
	integer, parameter :: nEqRat_fine = 5
	real :: T0_fine(nT0_fine)
	real :: YH_fine(nYH_fine)
	real :: eqRat_fine(nEqRat_fine)

   ! MCMC
   integer, parameter :: nModelParams = 3
   integer, parameter :: nParams = nModelParams + nCalib
	integer, parameter :: nSamples = 1e7
	integer, parameter :: nBurn = nint(0.1*nSamples)
	real, parameter :: A3(2) = (/1e15,4e17/) ! MKS
	real, parameter :: E3(2) = (/0.1e2,4e4/) ! cal/mol
	real, parameter :: b3(2) = (/-4.0,-1.7/)
   real, parameter :: sigma_primitive(2) = (/0.1, 10.0/)
	real :: sigma(nCalib,2)
	real :: sampleMin(nParams)
	real :: sampleMax(nParams)
   real, parameter :: sample0_simple = 0.5
   real, parameter :: sample0_model(nModelParams) = (/1e17, 2.5e4, -2.25/)
   real, parameter :: deltaMax_simple = 0.0025
   real :: sample0_sigma(nCalib)
   real :: sample0(nParams)
   real :: deltaMax(nParams)

   ! Miscellaneous
	integer, parameter :: nThreads = min(40,nCalib)
   real, parameter :: tf = 1e-3
	real, parameter :: dtCantera = 5e-8
	real, parameter :: dtModel = 2.8e-6 ! From 7 3-D PlasComCM simulations 
	integer, parameter :: nStepsCantera = nint(tf/dtCantera)
	integer, parameter :: nStepsModel = nint(tf/dtModel)
   integer, parameter :: verbCantera = 1 ! 1-percent; 2-each run; 3-MATLAB file; 4-debug file
	integer, parameter :: verbModel = 1 ! 1-(none); 2-each run; 3-MATLAB file; 4-debug file
	integer, parameter :: verbMCMC = 1 ! 1-percent; 2-accepted samples; 3-accepted+rejected

   ! Variable
   character(16) :: inputsFile
   character(16) :: readmeFile
   integer :: ts(8)
   integer :: i

   inputsFile = 'inputs.txt'
   readmeFile = 'readme.txt'

   do i = 1,nCalib
      sigma(i,:) = sigma_primitive
   end do 
   sampleMin(1:nModelParams) = (/A3(1),E3(1),b3(1)/)
   sampleMax(1:nModelParams) = (/A3(2),E3(2),b3(2)/)
   sampleMin(nModelParams+1:nParams) = sigma(:,1)
   sampleMax(nModelParams+1:nParams) = sigma(:,2)
   sample0_sigma = sample0_simple*(sampleMin(nModelParams+1:nParams) +&
      sampleMax(nModelParams+1:nParams))
   sample0(1:nModelParams) = sample0_model
   sample0(nModelParams+1:nParams) = sample0_sigma
   deltaMax = deltaMax_simple*(sampleMax-sampleMin)

   call date_and_time(values=ts)

	call linspace(T0_calib,T0_calib_bnd(1),T0_calib_bnd(2),nT0_calib)
	call linspace(T0_eval,T0_eval_bnd(1),T0_eval_bnd(2),nT0_eval)
	call linspace(T0_fine,T0_fine_bnd(1),T0_fine_bnd(2),nT0_fine)
	call logspace(YH_calib,YH_calib_bnd(1),YH_calib_bnd(2),nYH_calib)
	call logspace(YH_eval,YH_eval_bnd(1),YH_eval_bnd(2),nYH_eval)
	call logspace(YH_fine,YH_fine_bnd(1),YH_fine_bnd(2),nYH_fine)
	call linspace(eqRat_calib,eqRat_calib_bnd(1),eqRat_calib_bnd(2),nEqRat_calib)
	call linspace(eqRat_eval,eqRat_eval_bnd(1),eqRat_eval_bnd(2),nEqRat_eval)
	call linspace(eqRat_fine,eqRat_fine_bnd(1),eqRat_fine_bnd(2),nEqRat_fine)

   call writeReadme()
   call writeInputs()

   call printArray(T0_calib,'T0_calib.dat')
   call printArray(T0_eval,'T0_eval.dat')
   call printArray(T0_fine,'T0_fine.dat')
   call printArray(YH_calib,'YH_calib.dat')
   call printArray(YH_eval,'YH_eval.dat')
   call printArray(YH_fine,'YH_fine.dat')
   call printArray(eqRat_calib,'eqRat_calib.dat')
   call printArray(eqRat_eval,'eqRat_eval.dat')
   call printArray(eqRat_fine,'eqRat_fine.dat')
   call printArray(sample0,'sample0.dat')
   call printArray(sampleMin,'sampleMin.dat')
   call printArray(sampleMax,'sampleMax.dat')
   call printArray(deltaMax,'deltaMax.dat')

contains

   subroutine writeInputs()
      open(unit=1,file=inputsFile,status='replace',action='write')
      write(1,*) '&INPUTS'
      write(1,*) 'nT0_calib = ', nT0_calib
      write(1,*) 'nYH_calib = ', nYH_calib
      write(1,*) 'nEqRat_calib = ', nEqRat_calib
      write(1,*) 'nCalib = ', nCalib
      write(1,*) 'nT0_eval = ', nT0_eval
      write(1,*) 'nYH_eval = ', nYH_eval
      write(1,*) 'nEqRat_eval = ', nEqRat_eval
      write(1,*) 'nT0_fine = ', nT0_fine
      write(1,*) 'nYH_fine = ', nYH_fine
      write(1,*) 'nEqRat_fine = ', nEqRat_fine
      write(1,*) 'nSamples = ',nSamples
      write(1,*) 'nBurn = ',nBurn
      write(1,*) 'deltaMax_simple = ',deltaMax_simple
      write(1,*) 'nModelParams = ',nModelParams
      write(1,*) 'nParams = ', nParams
      write(1,*) 'nThreads = ', nThreads
      write(1,*) 'dtCantera = ', dtCantera
      write(1,*) 'dtModel = ', dtModel
      write(1,*) 'nStepsCantera = ', nStepsCantera
      write(1,*) 'nStepsModel = ', nStepsModel
      write(1,*) 'verbCantera = ', verbCantera
      write(1,*) 'verbModel = ', verbModel
      write(1,*) 'verbMCMC = ', verbMCMC
      write(1,*) '/'
      close(1)
      print *, 'File Written: ',inputsFile
   end subroutine

   subroutine writeReadme()

100 format(a)

101 format(a,i8)
102 format(a,i9)
103 format(a,i2,a,i2,a,i2,a)
104 format(a,i3)
105 format(a,i2,a,i2,a,i4,a,i2,a,i2)

110 format(a,f6.4)
111 format(a,f5.2,a,f5.2)
112 format(a,f8.2,a,f8.2)
113 format(a,4f6.3,a)

121 format(a,es9.2)
122 format(a,es9.2,a,es9.2)

130 format(a,es10.3,es10.3,f7.3)

      write(*,100) ''
      write(*,100) '>> Calibrating Model 0 against Cantera using Bayesian analysis'
      write(*,105) '   Date and Time:',ts(2),'-',ts(3),'-',ts(1),'-',ts(5),'-',ts(6)
      write(*,100) '>> Isobaric hydrogen-air combustion in zero dimensions'
      write(*,104) '   nThreads = ', nThreads
      write(*,121) '   dtCantera = ',dtCantera
      write(*,121) '   dtModel = ',dtModel
      write(*,101) '   nStepsCantera = ',nStepsCantera
      write(*,101) '   nStepsModel = ',nStepsModel
      write(*,100) '>> Parameter Range:'
      write(*,122) '   A3 = ', A3(1),' : ',A3(2)
      write(*,122) '   E3 = ', E3(1),' : ',E3(2)
      write(*,111) '   b3 = ', b3(1),' : ',b3(2)
      write(*,111) '   sigma_primitive = ', sigma_primitive(1),' : ',sigma_primitive(2)
      write(*,100) '>> Calibration Range:'
      write(*,112) '   T0_calib = ', T0_calib(1),' : ',T0_calib(nT0_calib)
      write(*,122) '   YH_calib = ', YH_calib(1),' : ',YH_calib(nYH_calib)
      write(*,111) '   eqRat_calib = ', eqRat_calib(1),' : ',eqRat_calib(nEqRat_calib)
      write(*,103) '>> Calibration Points: (',nT0_calib,' x ',nYH_calib,' x ',nEqRat_calib,')'
      write(*,103) '>> Evaluation Points: (',nT0_eval,' x ',nYH_eval,' x ',nEqRat_eval,')'
      write(*,103) '>> Fine Points: (',nT0_fine,' x ',nYH_fine,' x ',nEqRat_fine,')'
      write(*,100) '>> MCMC:'
      write(*,130) '   sample0_model = ',sample0_model
      write(*,110) '   sample0_simple = ',sample0_simple
      write(*,110) '   deltaMax_simple = ',deltaMax_simple
      write(*,102) '   nSamples = ',nSamples
      write(*,101) '   nBurn = ',nBurn
      write(*,100)   ''

      open(unit=1,file=readmeFile,status='replace')
      write(1,100) '>> Calibrating Model 0 against Cantera using Bayesian analysis'
      write(1,105) '   Date and Time:',ts(2),'-',ts(3),'-',ts(1),'-',ts(5),'-',ts(6)
      write(1,100) '>> Isobaric hydrogen-air combustion in zero dimensions'
      write(1,104) '   nThreads = ', nThreads
      write(1,121) '   dtCantera = ',dtCantera
      write(1,121) '   dtModel = ',dtModel
      write(1,101) '   nStepsCantera = ',nStepsCantera
      write(1,101) '   nStepsModel = ',nStepsModel
      write(1,100) '>> Parameter Range:'
      write(1,122) '   A3 = ', A3(1),' : ',A3(2)
      write(1,122) '   E3 = ', E3(1),' : ',E3(2)
      write(1,111) '   b3 = ', b3(1),' : ',b3(2)
      write(1,111) '   sigma_primitive = ', sigma_primitive(1),' : ',sigma_primitive(2)
      write(1,100) '>> Calibration Range:'
      write(1,112) '   T0_calib = ', T0_calib(1),' : ',T0_calib(nT0_calib)
      write(1,122) '   YH_calib = ', YH_calib(1),' : ',YH_calib(nYH_calib)
      write(1,111) '   eqRat_calib = ', eqRat_calib(1),' : ',eqRat_calib(nEqRat_calib)
      write(1,103) '>> Calibration Points: (',nT0_calib,' x ',nYH_calib,' x ',nEqRat_calib,')'
      write(1,103) '>> Evaluation Points: (',nT0_eval,' x ',nYH_eval,' x ',nEqRat_eval,')'
      write(1,103) '>> Fine Points: (',nT0_fine,' x ',nYH_fine,' x ',nEqRat_fine,')'
      write(1,100) '>> MCMC:'
      write(1,130) '   sample0_model = ',sample0_model
      write(1,110) '   sample0_simple = ',sample0_simple
      write(1,110) '   deltaMax_simple = ',deltaMax_simple
      write(1,102) '   nSamples = ',nSamples
      write(1,101) '   nBurn = ',nBurn
      close(1)

      write(*,*) 'File Written: ',readmeFile

   end subroutine

end program
