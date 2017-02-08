program ProgStar

   use ModLib
   use ModReadInputs
   use ModConstants

   implicit none

   integer :: nT0_eval
   integer :: nYH_eval
   integer :: nEqRat_eval
   integer :: nSamples
   integer :: nParams
   integer :: nThreads
   real, allocatable :: thetas(:,:)
   integer :: verbModel
   real :: A3,E3,b3
   real :: h
   real, allocatable :: c(:,:)
   real, allocatable :: expSStar(:,:,:)
   real, allocatable :: varSStar(:,:,:)
   character(16) :: inputsFile = 'inputs.txt'
   character(16) :: readmeStarFile = 'readmeStar.txt'
   character(32) :: filename
   integer :: i,j,k,l
   integer :: nModelParams
   integer, parameter :: maxPow = 2
   integer :: t1,t2,clock_rate,clock_max
   real :: timeIntegral

   ! hello

100 format(a,f8.1,a)

   call readInputsProcessor(inputsFile,nT0_eval,nYH_eval,nEqRat_eval,nSamples,nModelParams,&
      nParams,nThreads,thetas,verbModel)

   if (verbModel >= 1) then
      write(*,'(/,a)') 'COMPUTING MC INTEGRAL WITH INTERPOLATED POINTS...'
   end if

   call system_clock(t1,clock_rate,clock_max)

   allocate(c(nModelParams+1,maxPow))
   allocate(expSStar(nT0_eval,nYH_eval,nEqRat_eval))
   expSStar = 0.0
   do k = 1,nEqRat_eval
      do j = 1,nYH_eval
         do i = 1,nT0_eval
            write(filename,'(a,i1,a,i1,a,i1,a)') 'Scoeffs_',i,'',j,'',k,'.dat'
            call readArray(c,shape(c),filename)
            do l = 1,nSamples
               A3 = thetas(iA3,l)
               E3 = thetas(iE3,l)
               b3 = thetas(ib3,l)
               h = c(nModelParams+1,1) + c(iA3,1)*A3 + c(iA3,2)*A3**2 + c(iE3,1)*E3 &
                  + c(iE3,2)*E3**2 + c(ib3,1)*b3 + c(ib3,2)*b3**2
               expSStar(i,j,k) = expSStar(i,j,k) + h
            end do
         end do
      end do
   end do
   expSStar = expSStar/nSamples
   
   allocate(varSStar(nT0_eval,nYH_eval,nEqRat_eval))
   varSStar = 0.0
   do k = 1,nEqRat_eval
      do j = 1,nYH_eval
         do i = 1,nT0_eval
            write(filename,'(a,i1,a,i1,a,i1,a)') 'Scoeffs_',i,'',j,'',k,'.dat'
            call readArray(c,shape(c),filename)
            do l = 1,nSamples
               A3 = thetas(iA3,l)
               E3 = thetas(iE3,l)
               b3 = thetas(ib3,l)
               h = c(nModelParams+1,1) + c(iA3,1)*A3 + c(iA3,2)*A3**2 + c(iE3,1)*E3 &
                  + c(iE3,2)*E3**2 + c(ib3,1)*b3 + c(ib3,2)*b3**2
               varSStar(i,j,k) = varSStar(i,j,k) + (h-expSStar(i,j,k))**2
            end do
         end do
      end do
   end do
   varSStar = varSStar/nSamples

   call printArray(expSStar,'expSStar.dat') 
   call printArray(varSStar,'varSStar.dat')

   call system_clock(t2,clock_rate,clock_max)
   timeIntegral = real(t2-t1)/real(clock_rate)

   if (verbModel >= 1) then
      write(*,'(/,a)') 'INTEGRAL...DONE.'
      write(*,100) 'Time Elapsed: ',timeIntegral,' sec'
   end if

   open(unit=1,file=readmeStarFile,status='replace',action='write')
   write(1,'(a)') 'Some Stats from Calculate Starred Integral:'
   write(1,100) 'Total Time Elapsed: ', timeIntegral,' sec'
   close(1)
   print *, 'File Written: ',readmeStarFile

end program
