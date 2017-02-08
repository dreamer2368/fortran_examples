module ModCalibrator

   use ModLib
   use ModReadInputs
   use ModMcmc
   use ModModel
   use ModParameters

   implicit none

   real, allocatable, private :: T0_calib(:)
   real, allocatable, private :: YH_calib(:)
   real, allocatable, private :: eqRat_calib(:)
   integer, private :: nT0_calib
   integer, private :: nYH_calib
   integer, private :: nEqRat_calib
   real, private :: dtModel
   integer, private :: nStepsModel
   integer, private :: verbModel
   real, allocatable, private :: SDataCalib(:,:,:)
   integer, private :: nThreads
   real, allocatable, private :: sampleMin(:)
   real, allocatable, private :: sampleMax(:)
   integer, allocatable, private :: sigInd(:,:,:)
   real, allocatable, private :: priorCoeffs(:)
   integer, private :: nPriorCoeffs

contains

   subroutine progModelSub()

      real, allocatable :: T0_eval(:)
      real, allocatable :: YH_eval(:)
      real, allocatable :: eqRat_eval(:)
      real, allocatable :: T0_fine(:)
      real, allocatable :: YH_fine(:)
      real, allocatable :: eqRat_fine(:)
      integer :: nT0_eval
      integer :: nYH_eval
      integer :: nEqRat_eval
      integer :: nT0_fine
      integer :: nYH_fine
      integer :: nEqRat_fine
      integer :: nSamples
      integer :: nBurn
      real :: deltaMax_simple
      integer :: nParams
      real, allocatable :: sample0(:)
      integer :: verbMCMC
      character(16) :: inputsFile = 'inputs.txt'
      character(16) :: readmeModelFile = 'readmeModel.txt'
      character(32) :: filename
      real :: tig,ignTemp,Tfinal
      integer :: i,j,k,l,ii,jj,kk,counter,nCalib,nEval,nFine
      integer :: t1,t2,t3,t4,clock_rate,clock_max
      real :: timeEval,timeCalib,timeFine,timeIO,timeInterp,timePerRun,timePerSample,timeTotal
      real :: progress
      real :: acceptRate, rejectRate
      real :: A3,E3,b3,nu21,nu22,nu23
      real :: S
      integer :: mode
      real, allocatable :: deltaMax(:)
      real, allocatable :: SModelCalib(:,:,:,:)
      real, allocatable :: SModelEval(:,:,:,:)
      real, allocatable :: expSEval(:,:,:)
      real, allocatable :: varSEval(:,:,:)
      real, allocatable :: modeSEval(:,:,:)
      real, allocatable :: modeSFine(:,:,:)
      real, allocatable :: samples(:,:)
      real, allocatable :: post(:)
      real, allocatable :: expSample(:)
      real, allocatable :: varSample(:)
      real, allocatable :: modeSample(:)
      real, allocatable :: interp(:,:,:,:,:)
      real, allocatable :: sample(:)
      real, allocatable :: sigmaSample(:)
      integer :: nModelParams

100 format(a,f8.1,a)
101 format(a,f8.4,a)
102 format(a,f8.6,a)
103 format(a,f4.1,a)

      call system_clock(t3,clock_rate,clock_max)

      call readInputsModel(inputsFile,SDataCalib,T0_calib,YH_calib,eqRat_calib,T0_eval,YH_eval,&
         eqRat_eval,T0_fine,YH_fine,eqRat_fine,nT0_calib,nYH_calib,nEqRat_calib,nCalib,nT0_eval,&
         nYH_eval,nEqRat_eval,nT0_fine,nYH_fine,nEqRat_fine,nSamples,nBurn,deltaMax_simple,&
         deltaMax,nModelParams,nParams,nThreads,nStepsModel,dtModel,sample0,sampleMin,sampleMax,&
         priorCoeffs,verbModel,verbMCMC)

      nCalib = nT0_calib * nYH_calib * nEqRat_calib
      nEval = nT0_eval * nYH_eval * nEqRat_eval
      nFine = nT0_fine * nYH_fine * nEqRat_fine
      nPriorCoeffs = size(priorCoeffs)

      allocate(sigInd(nT0_calib,nYH_calib,nEqRat_calib))
      l = nModelParams + 1
      do k = 1,nEqRat_calib
         do j = 1,nYH_calib
            do i = 1,nT0_calib
               sigInd(i,j,k) = l 
               l = l + 1
            end do
         end do
      end do

      if (verbModel >= 1) then
         write(*,'(a)') ''
         write(*,'(a)') '********************* MODEL 0 **********************'
         write(*,'(a)') ''
         write(*,'(a)') 'CALIBRATING MODEL...'
      end if

      call system_clock(t1,clock_rate,clock_max)

      allocate(samples(nParams,nSamples))
      allocate(post(nSamples))
      allocate(SModelCalib(nT0_calib,nYH_calib,nEqRat_calib,nSamples))

      call metropolis(nSamples,nBurn,nParams,sample0,deltaMax,state,nT0_calib,nYH_calib,&
         nEqRat_calib,verbMCMC,mode,samples,post,SModelCalib,acceptRate,rejectRate,timePerSample)

      call system_clock(t2,clock_rate,clock_max)

      timeCalib = real(t2-t1)/real(clock_rate)
      timePerRun = timePerSample/nCalib

      if (verbModel >= 1) then
         write(*,'(/,a)') 'MODEL CALIBRATION...done.'
         write(*,103) 'Acceptance Rate: ',100.0*acceptRate,' %' 
         write(*,103) 'Rejection Rate: ',100.0*rejectRate,' %' 
         write(*,100) 'Time Elapsed: ',timeCalib,' sec'
         write(*,101) 'Time Per Sample: ',timeCalib/(nSamples+nBurn+1),' sec'
         write(*,102) 'Time Per Run: ',timePerRun,' sec'
      end if

      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      if (verbModel >= 1) then
         write(*,'(/,a)') 'RUNNING MODEL AT EVALUATION POINTS...'
      end if

      call system_clock(t1,clock_rate,clock_max)
      
      allocate(SModelEval(nT0_eval,nYH_eval,nEqRat_eval,nSamples))
      allocate(modeSEval(nT0_eval,nYH_eval,nEqRat_eval))
      progress = 0.

      do l = 1,nSamples
         A3 = samples(iA3,l)
         E3 = samples(iE3,l)
         b3 = samples(ib3,l)
         !$omp parallel do private(tig,ignTemp,ii,jj,kk) default(shared) collapse(3) &
         !$omp num_threads(nThreads)
         do k = 1,nEqRat_eval
            do j = 1,nYH_eval
               do i = 1,nT0_eval
                  ii = findMember(T0_eval(i),T0_calib)
                  jj = findMember(YH_eval(j),YH_calib)
                  kk = findMember(eqRat_eval(k),eqRat_calib)
                  if (ii/=0 .and. jj/=0 .and. kk/=0) then
                     SModelEval(i,j,k,l) = SModelCalib(ii,jj,kk,l)
                  else
                     call runModel(nStepsModel,dtModel,T0_eval(i),YH_eval(j),eqRat_eval(k),A3,&
                        E3,b3,nu21,nu22,nu23,verbModel,tig,ignTemp,SModelEval(i,j,k,l),Tfinal)
                  end if
               end do
            end do
         end do
         !$omp end parallel do
         if (mode == l) then
            modeSEval = SModelEval(:,:,:,l)
         end if
         if (verbModel == 1) then
            call trackProgress(l,nSamples,progress)
         end if
      end do

      call system_clock(t2,clock_rate,clock_max)
      timeEval = real(t2-t1)/real(clock_rate)

      allocate(expSEval(nT0_eval,nYH_eval,nEqRat_eval))
      allocate(varSEval(nT0_eval,nYH_eval,nEqRat_eval))
      allocate(expSample(nParams))
      allocate(varSample(nParams))
      allocate(modeSample(nParams))

      expSEval = 0.0
      expSample = 0.0
      do l = 1,nSamples
         expSEval = expSEval + SModelEval(:,:,:,l)
         expSample = expSample + samples(:,l)
      end do
      expSEval = expSEval/nSamples
      expSample = expSample/nSamples

      varSEval = 0.0
      varSample = 0.0
      do l = 1,nSamples
         varSEval = varSEval + (SModelEval(:,:,:,l) - expSEval)**2
         varSample = varSample + (samples(:,l) - expSample)**2
      end do
      varSEval = varSEval/nSamples
      varSample = varSample/nSamples

      modeSample = samples(:,mode)

      if (verbModel >= 1) then
         write(*,'(/,a)') 'EVALUATION POINTS...DONE.'
         write(*,100) 'Time Elapsed: ',timeEval,' sec'
      end if

      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      if (verbModel >= 1) then
         write(*,'(/,a)') 'RUNNING MODEL AT FINE GRID POINTS...'
      end if

      call system_clock(t1,clock_rate,clock_max)

      allocate(modeSFine(nT0_fine,nYH_fine,nEqRat_fine))
      A3 = modeSample(iA3)
      E3 = modeSample(iE3)
      b3 = modeSample(ib3)
      counter = 0
      progress = 0.0

      !$omp parallel do private(tig,ignTemp,ii,jj,kk) default(shared) collapse(3) &
      !$omp num_threads(nThreads)
      do k = 1,nEqRat_fine
         do j = 1,nYH_fine
            do i = 1,nT0_fine

               ii = findMember(T0_fine(i),T0_eval)
               jj = findMember(YH_fine(j),YH_eval)
               kk = findMember(eqRat_fine(k),eqRat_eval)
               if (ii/=0 .and. jj/=0 .and. kk/=0) then
                  modeSFine(i,j,k) = SModelEval(ii,jj,kk,mode)
               else
                  call runModel(nStepsModel,dtModel,T0_fine(i),YH_fine(j),eqRat_fine(k),A3,&
                     E3,b3,nu21,nu22,nu23,verbModel,tig,ignTemp,modeSFine(i,j,k),Tfinal)
               end if

               !$omp critical
               counter = counter + 1
               if (verbModel == 1) then
                  call trackProgress(counter,nFine,progress)
               end if
               !$omp end critical

            end do
         end do
      end do
      !$omp end parallel do

      call system_clock(t2,clock_rate,clock_max)
      timeFine = real(t2-t1)/real(clock_rate)

      if (verbModel >= 1) then
         write(*,'(/,a)') 'FINE GRID POINTS...DONE.'
         write(*,100) 'Time Elapsed: ',timeFine,' sec'
      end if

      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      if (verbModel >= 1) then
         write(*,'(/,a)') 'GENERATING INTERPOLATION POINTS...'
      end if

      call system_clock(t1,clock_rate,clock_max)


      ! Generate interpolating points
      nModelParams = nParams - 1 ! Exclude sigma
      allocate(interp(nModelParams*2+1,nModelParams+1,nT0_eval,nYH_eval,nEqRat_eval))
      allocate(sample(nModelParams))
      allocate(sigmaSample(nModelParams))
      progress = 0.0
      counter = 0 

      !$omp parallel do private(A3,E3,b3,tig,ignTemp,sample,sigmaSample,S,l) default(shared) &
      !$omp collapse(3) num_threads(nThreads)
      do k = 1,nEqRat_eval
         do j = 1,nYH_eval
            do i = 1,nT0_eval

               sample = expSample(1:nModelParams)
               A3 = sample(iA3)
               E3 = sample(iE3)
               b3 = sample(ib3)
!               call runModel(nStepsModel,dtModel,T0_eval(i),YH_eval(j),eqRat_eval(k),A3,&
!                  E3,b3,nu21,nu22,nu23,verbModel,tig,ignTemp,S,Tfinal)
               interp(1,1:nModelParams,i,j,k) = sample
               interp(1,nModelParams+1,i,j,k) = S

               do l = 1,nModelParams !! Exclude sigma, because we know it has no dependence
                  sigmaSample = 0.0
                  sigmaSample(l) = sqrt(varSample(l))

                  sample = expSample(1:nModelParams) + sigmaSample
                  A3 = sample(iA3)
                  E3 = sample(iE3)
                  b3 = sample(ib3)
!                  call runModel(nStepsModel,dtModel,T0_eval(i),YH_eval(j),eqRat_eval(k),A3,&
!                     E3,b3,nu21,nu22,nu23,verbModel,tig,ignTemp,S,Tfinal)
                  interp(2*l,1:nModelParams,i,j,k) = sample
                  interp(2*l,nModelParams+1,i,j,k) = S

                  sample = expSample(1:nModelParams) - sigmaSample
                  A3 = sample(iA3)
                  E3 = sample(iE3)
                  b3 = sample(ib3)
!                  call runModel(nStepsModel,dtModel,T0_eval(i),YH_eval(j),eqRat_eval(k),A3,&
!                     E3,b3,nu21,nu22,nu23,verbModel,tig,ignTemp,S,Tfinal)
                  interp(2*l+1,1:nModelParams,i,j,k) = sample
                  interp(2*l+1,nModelParams+1,i,j,k) = S
               end do

               !$omp critical
               counter = counter + 1
               if (verbModel == 1) then
                  call trackProgress(counter,nEval,progress)
               end if
               !$omp end critical

            end do
         end do
      end do
      !$omp end parallel do
      
      call system_clock(t2,clock_rate,clock_max)
      timeInterp = real(t2-t1)/real(clock_rate)

      if (verbModel >= 1) then
         write(*,'(/,a)') 'INTERPOLATION POINTS...DONE.'
         write(*,100) 'Time Elapsed: ',timeInterp,' sec'
      end if

      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      if (verbModel >= 1) then
         write(*,'(/,a)') 'WRITING MODEL OUTPUT TO FILE...'
      end if

      call system_clock(t1,clock_rate,clock_max)

      call printArray(samples,'samples.dat')
      call printArray(post,'post.dat')
      call printArray((/mode/),'mode.dat')
      call printArray(modeSEval,'modeSEval.dat')
      call printArray(samples(:,mode),'modeSample.dat')
      call printArray(expSample,'expSample.dat')
      call printArray(varSample,'varSample.dat')
      call printArray(expSEval,'expSEval.dat')
      call printArray(varSEval,'varSEval.dat')
      call printArray(modeSEval,'modeSEval.dat')
      call printArray(modeSFine,'modeSFine.dat')

      do k = 1,nEqRat_eval
         do j = 1,nYH_eval
            do i = 1,nT0_eval
               write(filename,'(a,i1,a,i1,a,i1,a)') 'SModelEval_',i,'',j,'',k,'.dat'
               call printArray(SModelEval(i,j,k,:),filename)
               write(filename,'(a,i1,a,i1,a,i1,a)') 'interp_',i,'',j,'',k,'.dat'
               call printArray(interp(:,:,i,j,k),filename)
            end do
         end do
      end do

      call system_clock(t2,clock_rate,clock_max)
      timeIO = real(t2-t1)/real(clock_rate)

      call system_clock(t4,clock_rate,clock_max)
      timeTotal = real(t4-t3)/real(clock_rate)

      if (verbModel >= 1) then
         write(*,'(a)') 'WRITING...DONE.'
         write(*,100) 'Time Elapsed: ',timeIO,' sec'
         write(*,*) '' 
         write(*,100) 'TOTAL TIME ELAPSED: ',timeTotal, ' sec'
         write(*,'(/,a,/)') '*********************************************************'
      end if

      open(unit=1,file=readmeModelFile,status='replace',action='write')
      write(1,'(a)') 'Some Stats from Model Calibration:'
      write(1,103) 'Acceptance Rate: ',100.0*acceptRate,' %' 
      write(1,103) 'Rejection Rate: ',100.0*rejectRate,' %' 
      write(1,101) 'Avg. Time Per Run: ',timePerRun,' sec'
      write(1,101) 'Avg. Time Per Sample: ',timePerSample,' sec'
      write(1,100) 'Time for Sampling: ',timeCalib,' sec'
      write(1,100) 'Time for Evaluation Points: ',timeEval,' sec'
      write(1,100) 'Time for Fine Points: ',timeFine,' sec'
      write(1,100) 'Time for Interpolation Points: ',timeInterp,' sec'
      write(1,100) 'Time for Input/Output: ',timeIO,' sec'
      write(1,100) 'Total Time Elapsed: ', timeTotal,' sec'
      close(1)
      print *, 'File Written: ', readmeModelFile

   end subroutine

	subroutine state(sample,post,pred)

		real, intent(in) :: sample(:)
		real, intent(out) :: post
      real, intent(out) :: pred(:,:,:)
      real :: sigma(nT0_calib,nYH_calib,nEqRat_calib)
		real :: A3
		real :: E3
		real :: nu21
		real :: nu22
		real :: nu23
		real :: b3
      real :: ignTemp
      real :: Tfinal
		real :: prior
		real :: SLike
      real :: tig
		integer :: i
		integer :: j
		integer :: k
      logical :: unstable
      logical :: ex

      unstable = .false.

		prior = computePrior(sample,sampleMin,sampleMax)

      if (prior == 0.0) then
         post = 0.0
         pred = 0.0
      else
         A3 = sample(iA3)
         E3 = sample(iE3)
         b3 = sample(ib3)
         !$omp parallel do private(tig,ignTemp,Tfinal) default(shared) collapse(3) &
         !$omp num_threads(nThreads)
         do k = 1,nEqRat_calib
            do j = 1,nYH_calib
               do i = 1,nT0_calib
                  call runModel(nStepsModel,dtModel,T0_calib(i),YH_calib(j),eqRat_calib(k),A3,&
                     E3,b3,nu21,nu22,nu23,verbModel,tig,ignTemp,pred(i,j,k),Tfinal)
                  if (Tfinal > 3000.0) then
                     unstable = .true.
                  end if
                  sigma(i,j,k) = sample(sigInd(i,j,k))
               end do
            end do
         end do
         !$omp end parallel do
         SLike = computeLikelihood(pred,SDataCalib,sigma)
         post = SLike*prior
      end if

      if (unstable) then
         inquire(file='UNSTABLE.dat',exist=ex)
         if (ex) then
            open(unit=1,file='UNSTABLE.dat',status='old',action='write',position='append')
         else
            open(unit=1,file='UNSTABLE.dat',status='new',action='write')
         end if
         write(1,*) sample,post
         close(1)
         print *, '**UNSTABLE**'
         post = 0.0
      end if

	end subroutine

	function computeLikelihood(pred,model,sigma) result(L)

		real, intent(in) :: pred(:,:,:)
		real, intent(in) :: model(:,:,:)
		real, intent(in) :: sigma(:,:,:)
      real, parameter :: pi = 3.1415926535897932
		real :: L

		L = product(1.0/(sigma*sqrt(2.0*pi))*exp(-(pred-model)**2/(2.0*sigma**2)))

	end function

	function computePrior(sample,sampleMin,sampleMax) result(prior)

		real, intent(in) :: sample(:)
		real, intent(in) :: sampleMin(:)
		real, intent(in) :: sampleMax(:)
		real :: prior
      real :: E3,b3,b3_bound
      integer :: i

      E3 = sample(iE3)
      b3 = sample(ib3)

		if (any(sample<sampleMin) .or. any(sample>sampleMax)) then
			prior = 0.0
      else
         b3_bound = 0.0
         do i = 1,nPriorCoeffs
            b3_bound = b3_bound + priorCoeffs(i)*E3**(i-1)
         end do
         if (b3 > b3_bound) then
            prior = 0.0
         else
            prior = 1.0
         end if
		end if

	end function

end module
