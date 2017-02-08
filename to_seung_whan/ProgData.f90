program ProgData

   use ModLib
   use ModConstants
   use ModModel
   use ModReadInputs

   implicit none

   real, allocatable :: T0_calib(:)
   real, allocatable :: YH_calib(:)
   real, allocatable :: eqRat_calib(:)
   real, allocatable :: T0_eval(:)
   real, allocatable :: YH_eval(:)
   real, allocatable :: eqRat_eval(:)
   real, allocatable :: T0_fine(:)
   real, allocatable :: YH_fine(:)
   real, allocatable :: eqRat_fine(:)
   integer :: nT0_calib
   integer :: nYH_calib
   integer :: nEqRat_calib
   integer :: nT0_eval
   integer :: nYH_eval
   integer :: nEqRat_eval
   integer :: nT0_fine
   integer :: nYH_fine
   integer :: nEqRat_fine
   real :: dtCantera
   integer :: nStepsCantera
   integer :: verbCantera
   character(16) :: inputsFile = 'inputs.txt'
   character(16) :: readmeDataFile = 'readmeData.txt'
   real :: tig,ignTemp,Tfinal
   integer :: i,j,k,ii,jj,kk,counter,nCalib,nEval,nFine
   integer :: t1,t2,t3,t4,clock_rate,clock_max
   real :: timeFine,timeEval,timeCalib,timeIO,timePerRun,timeTotal
   real :: progress
   real, allocatable :: SDataCalib(:,:,:)
   real, allocatable :: SDataEval(:,:,:)
   real, allocatable :: SDataFine(:,:,:)

100 format(a,f8.1,a)
101 format(a,f8.4,a)

   call system_clock(t3,clock_rate,clock_max)

   call readInputsData(inputsFile,T0_calib,YH_calib,eqRat_calib,T0_eval,YH_eval,&
      eqRat_eval,T0_fine,YH_fine,eqRat_fine,nT0_calib,nYH_calib,nEqRat_calib,nT0_eval,&
      nYH_eval,nEqRat_eval,nT0_fine,nYH_fine,nEqRat_fine,dtCantera,nStepsCantera,verbCantera)

   if (verbCantera >= 1) then
      write(*,'(a)') ''
      write(*,'(a)') '********************* CANTERA (GRI 3.0) **********************'
      write(*,'(a)') ''
      write(*,'(a)') 'RUNNING CANTERA AT CALIBRATION POINTS...'
   end if

   call system_clock(t1,clock_rate,clock_max)

   allocate(SDataCalib(nT0_calib,nYH_calib,nEqRat_calib))
   nCalib = nT0_calib*nYH_calib*nEqRat_calib
   progress = 0.
   counter = 0

   do k = 1,nEqRat_calib
      do j = 1,nYH_calib
         do i = 1,nT0_calib
            counter = counter + 1
            if (verbCantera == 1) then
               call trackProgress(counter,nCalib,progress)
            end if
            call runCantera(nStepsCantera,dtCantera,T0_calib(i),YH_calib(j),eqRat_calib(k),&
               verbCantera,tig,ignTemp,SDataCalib(i,j,k),Tfinal)
         end do
      end do
   end do

   call system_clock(t2,clock_rate,clock_max)

   timeCalib = real(t2-t1)/real(clock_rate)
   timePerRun = timeCalib/nCalib

   if (verbCantera >= 1) then
      write(*,'(/,a)') 'CALIBRATION POINTS...DONE.'
      write(*,100) 'Time Elapsed: ',timeCalib,' sec'
      write(*,101) 'Time Per Run: ',timePerRun,' sec'
   end if
   
   if (verbCantera >= 1) then
      write(*,'(/,a)') 'RUNNING CANTERA AT EVALUATION POINTS...'
   end if

   call system_clock(t1,clock_rate,clock_max)

   allocate(SDataEval(nT0_eval,nYH_eval,nEqRat_eval))
   nEval = nT0_eval*nYH_eval*nEqRat_eval
   progress = 0.
   counter = 0

   do k = 1,nEqRat_eval
      do j = 1,nYH_eval
         do i = 1,nT0_eval
            counter = counter + 1
            if (verbCantera == 1) then
               call trackProgress(counter,nEval,progress)
            end if
            ii = findMember(T0_eval(i),T0_calib)
            jj = findMember(YH_eval(j),YH_calib)
            kk = findMember(eqRat_eval(k),eqRat_calib)
            if (ii/=0 .and. jj/=0 .and. kk/=0) then
               SDataEval(i,j,k) = SDataCalib(ii,jj,kk)
            else
               call runCantera(nStepsCantera,dtCantera,T0_eval(i),YH_eval(j),eqRat_eval(k),&
                  verbCantera,tig,ignTemp,SDataEval(i,j,k),Tfinal)
            end if
         end do
      end do
   end do

   call system_clock(t2,clock_rate,clock_max)
   timeEval = real(t2-t1)/real(clock_rate)

   if (verbCantera >= 1) then
      write(*,'(/,a)') 'EVALUATION POINTS...DONE.'
      write(*,100) 'Time Elapsed: ',timeEval,' sec'
   end if

   if (verbCantera >= 1) then
      write(*,'(/,a)') 'RUNNING CANTERA AT FINE GRID POINTS...'
   end if

   call system_clock(t1,clock_rate,clock_max)

   allocate(SDataFine(nT0_fine,nYH_fine,nEqRat_fine))
   nFine = nT0_fine*nYH_fine*nEqRat_fine
   progress = 0.
   counter = 0

   do k = 1,nEqRat_fine
      do j = 1,nYH_fine
         do i = 1,nT0_fine
            counter = counter + 1
            if (verbCantera == 1) then
               call trackProgress(counter,nFine,progress)
            end if
            ii = findMember(T0_fine(i),T0_eval)
            jj = findMember(YH_fine(j),YH_eval)
            kk = findMember(eqRat_fine(k),eqRat_eval)
            if (ii/=0 .and. jj/=0 .and. kk/=0) then
               SDataFine(i,j,k) = SDataEval(ii,jj,kk)
            else
               call runCantera(nStepsCantera,dtCantera,T0_fine(i),YH_fine(j),eqRat_fine(k),&
                  verbCantera,tig,ignTemp,SDataFine(i,j,k),Tfinal)
            end if
         end do
      end do
   end do

   call system_clock(t2,clock_rate,clock_max)
   timeFine = real(t2-t1)/real(clock_rate)

   if (verbCantera >= 1) then
      write(*,'(/,a)') 'FINE GRID POINTS...DONE.'
      write(*,100) 'Time Elapsed: ',timeFine,' sec'
   end if

   if (verbCantera >= 1) then
      write(*,'(/,a)') 'WRITING CANTERA DATA TO FILE...'
   end if

   call system_clock(t1,clock_rate,clock_max)

   call printArray(SDataCalib,'SDataCalib.dat')
   call printArray(SDataEval,'SDataEval.dat')
   call printArray(SDataFine,'SDataFine.dat')

   call system_clock(t2,clock_rate,clock_max)
   timeIO = real(t2-t1)/real(clock_rate)
   
   call system_clock(t4,clock_rate,clock_max)
   timeTotal = real(t4-t3)/real(clock_rate)
   if (verbCantera >= 1) then
      write(*,'(a)') 'WRITING...DONE.'
      write(*,100) 'Time Elapsed: ',timeIO,' sec'
      write(*,*) '' 
      write(*,100) 'TOTAL TIME ELAPSED: ',timeTotal, ' sec'
      write(*,'(/,a,/)') '*********************************************************'
   end if

   open(unit=1,file=readmeDataFile,status='replace',action='write')
   write(1,'(a)') 'Some Stats from Cantera Runs:'
   write(1,101) 'Avg. Time Per Run: ',timePerRun,' sec'
   write(1,100) 'Time for Calibration Points: ',timeCalib,' sec'
   write(1,100) 'Time for Evaluation Points: ',timeEval,' sec'
   write(1,100) 'Time for Fine Grid Points: ',timeFine,' sec'
   write(1,100) 'Time for Input/Output: ',timeIO,' sec'
   write(1,100) 'Total Time Elapsed: ', timeTotal,' sec'
   close(1)
   print *, 'File Written: ',readmeDataFile

end program


