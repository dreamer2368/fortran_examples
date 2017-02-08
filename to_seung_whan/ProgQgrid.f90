program ProgQGrid

   use ModReadInputs
   use ModLib
   use ModModel
   use ModParameters

   implicit none

   character(32) :: inputsFile = 'test3/other/inputs.dat'
   real, allocatable :: expSample(:)
   real, allocatable :: varSample(:)
   real, allocatable :: T0_eval(:)
   real, allocatable :: YH_eval(:)
   real, allocatable :: eqRat_eval(:)
   integer :: nT0_eval,nYH_eval,nEqRat_eval
   real :: dtModel
   integer :: nStepsModel, nThreads, nParams
   integer :: verbModel
   real :: T0,YH,eqRat 
   integer :: iEqRat
   integer :: nModelParams
   real, allocatable :: thetaLowerBnd(:)
   real, allocatable :: thetaUpperBnd(:)
   real, allocatable :: theta1(:)
   real, allocatable :: theta2(:)
   real, allocatable :: theta3(:)
   real, allocatable :: theta1Mesh(:,:,:)
   real, allocatable :: theta2Mesh(:,:,:)
   real, allocatable :: theta3Mesh(:,:,:)
   real :: A2,E2,b2
   real :: tig,ignTemp,Tfinal
   real, allocatable :: Qgrid(:,:,:)
   real :: pmfactor = 3.0
   integer :: t1,t2,clock_rate,clock_max,counter
   real :: progress
   real :: timeQgrid, timePerRun
   integer, parameter :: iT0 = 4
   integer, parameter :: iYH = 2
   integer, parameter :: nTheta1 = 21
   integer, parameter :: nTheta2 = 21
   integer, parameter :: nTheta3 = 21
   integer :: i,j,k
   character(16) :: dir
     
   call readInputsForQGrid(inputsFile,expSample,varSample,T0_eval,YH_eval,eqRat_eval,&
      nT0_eval,nYH_eval,nEqRat_eval,nThreads,nStepsModel,nModelParams,nParams,dtModel,verbModel)

   if (verbModel >= 1) then
      write(*,'(/,a)') 'RUNNING Q ON PARAMETER GRID AROUND MEAN...'
   end if

  
   verbModel = 1

   call system_clock(t1,clock_rate,clock_max)
   
   iEqRat = ceiling(nEqRat_eval/2.0)

   T0 = T0_eval(iT0)
   YH = YH_eval(iYH)
   eqRat = eqRat_eval(iEqRat)

   allocate(thetaLowerBnd(nModelParams)) 
   allocate(thetaUpperBnd(nModelParams)) 
   do i = 1,nModelParams
      thetaLowerBnd(i) = expSample(i)-pmfactor*sqrt(varSample(i))
      thetaUpperBnd(i) = expSample(i)+pmfactor*sqrt(varSample(i))
   end do

   allocate(theta1(nTheta1))
   allocate(theta2(nTheta2))
   allocate(theta3(nTheta3))
   call linspace(theta1,thetaLowerBnd(1),thetaUpperBnd(1),nTheta1)
   call linspace(theta2,thetaLowerBnd(2),thetaUpperBnd(2),nTheta2)
   call linspace(theta3,thetaLowerBnd(3),thetaUpperBnd(3),nTheta3)

   allocate(theta1Mesh(nTheta1,nTheta2,nTheta3))
   allocate(theta2Mesh(nTheta1,nTheta2,nTheta3))
   allocate(theta3Mesh(nTheta1,nTheta2,nTheta3))
   call meshgrid3d(theta1,theta2,theta3,theta1Mesh,theta2Mesh,theta3Mesh)

   counter = 0
   progress = 0.0

   allocate(Qgrid(nTheta1,nTheta2,nTheta3))
   !$omp parallel do private(tig,ignTemp,A2,E2,b2) default(shared) collapse(3) &
   !$omp num_threads(nThreads)
   do k = 1,nTheta3
      do j = 1,nTheta2
         do i = 1,nTheta1

            A2 = theta1Mesh(i,j,k)
            E2 = theta2Mesh(i,j,k)
            b2 = theta3Mesh(i,j,k)
            call runModel(nStepsModel,dtModel,T0,YH,eqRat,A2,E2,b2,0.0,0.0,0.0,verbModel,&
               tig,ignTemp,Qgrid(i,j,k),Tfinal)

            !$omp critical
            counter = counter + 1
            if (verbModel == 1) then
               call trackProgress(counter,nTheta1*nTheta2*nTheta3,progress)
            end if
            !$omp end critical

         end do
      end do
   end do
   !$omp end parallel do

   call system_clock(t2,clock_rate,clock_max)
   timeQgrid = real(t2-t1)/real(clock_rate)
   timePerRun = timeQgrid/(nTheta1*nTheta2*nTheta3)

100 format(a,f8.1,a)
101 format(a,f8.6,a)
   if (verbModel >= 1) then
      write(*,'(/,a)') 'Q Grid...DONE.'
      write(*,100) 'Time Per Run: ',timePerRun,' sec'
      write(*,100) 'Time Elapsed: ',timeQgrid,' sec'
   end if


   dir = 'Qgrid/'
   call printArray(Qgrid,trim(dir)//'Qgrid.dat')
   call printArray(expSample,trim(dir)//'expSample.dat')
   call printArray(varSample,trim(dir)//'varSample.dat')
   call printArray((/nTheta1,nTheta2,nTheta3/),trim(dir)//'nTheta.dat')
   call printArray((/iT0,iYH,iEqRat/),trim(dir)//'IC.dat')
   call printArray(T0_eval,trim(dir)//'T0_eval.dat')
   call printArray(YH_eval,trim(dir)//'YH_eval.dat')
   call printArray(eqRat_eval,trim(dir)//'eqRat_eval.dat')
   call printArray(theta1Mesh,trim(dir)//'theta1Mesh.dat')
   call printArray(theta2Mesh,trim(dir)//'theta2Mesh.dat')
   call printArray(theta3Mesh,trim(dir)//'theta3Mesh.dat')

   call writeQgridReadme()

contains 

   subroutine writeQgridReadme()

      character(32) :: fname

      fname = trim(dir)//'readmeQgrid.dat'

      open(unit=1,file=fname,status='replace',action='write')
      write(1,*) 'dtModel = ',dtModel
      write(1,*) 'nStepsModel = ',nStepsModel
      write(1,*) 'pmfactor = ',pmfactor
      write(1,*) 'thetaLowerBnd = ',thetaLowerBnd
      write(1,*) 'thetaUpperBnd = ',thetaUpperBnd
      write(1,*) 'nTheta1 = ',nTheta1
      write(1,*) 'nTheta2 = ',nTheta2
      write(1,*) 'nTheta3 = ',nTheta3
      write(1,*) 'iT0 = ',iT0
      write(1,*) 'iYH = ',iYH
      write(1,*) 'iEqRat = ',iEqRat 
      write(1,*) 'T0 = ',T0
      write(1,*) 'YH = ',YH
      write(1,*) 'eqRat = ',eqRat
      write(1,*) 'Time Per Run = ',timePerRun
      write(1,*) 'Time Elapsed = ',timeQgrid
      close(1)

      print *, 'File Written: ',fname

   end subroutine


end program
