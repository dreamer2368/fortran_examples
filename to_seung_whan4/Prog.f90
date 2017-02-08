program Prog

   use ModLib
   use ModBurgers
   use ModUQ

   implicit none

   ! Constants
   integer, parameter :: nSteps = 301
   integer, parameter :: nx = 201
   integer, parameter :: nxb = nx+2
   integer, parameter :: nSamples = 1000
	integer, parameter :: nDumps = 4
   real, parameter :: nu_mean = 8e-3 ! do not go below 3e-3
   real, parameter :: nu_sigma = 6e-4
   real, parameter :: u0 = 1.0
   real, parameter :: u_sigma = 0.0
   real, parameter :: tf = 0.6
   integer, parameter :: verb = 0
   real, parameter :: L = 1.0
   real, parameter :: pi = 3.1415926535897932

   ! Inputs/outputs of model
	real :: u_mean(nx)
   real :: t(nSteps)
   real :: x(nx)
   real :: xb(nxb)
   real :: u_init(nx)
   real :: nu
	integer :: xShock_ind
	integer :: tShock_ind
	real :: uFull(nxb,nSteps,nSamples)
	real :: uxFull(nxb-2,nSteps,nSamples)
	real :: tShock(nSamples)
	real :: uShock(nxb,nSamples)
	real :: uxShock(nxb-2,nSamples)

	! Post-processing
	integer :: dumpFrequency
	real :: tDump(nDumps)
	integer :: tDump_ind(nDumps)
	real :: mean_tShock
	real :: mean_uFull(nxb,nSteps)
	real :: mean_uxFull(nxb-2,nSteps)
	real :: mean_uShock(nxb)
	real :: mean_uxShock(nxb-2)
	real :: std_tShock
	real :: std_uFull(nxb,nSteps)
	real :: std_uxFull(nxb-2,nSteps)
	real :: std_uShock(nxb)
	real :: std_uxShock(nxb-2)
	real :: post_uxShock(nSamples)
   integer :: i,j,k
   integer :: t1,t2,t3,clock_rate,clock_max
   real :: progress,simulationTime,postTime,totalTime

!	do i = 1,nn
!		r1(i) = randl(mu,sigma)
!	end do
!
!	call printArray(r1,'r1.dat')
!	stop

   call system_clock(t1,clock_rate,clock_max)

	call writeReadme()

   write(*,'(a)') ''
   write(*,'(a)') 'RUNNING SIMULATIONS WITH UNCERTAINTY...'

   call init_random_seed()

	! Space & time discretization
   call linspace(t,0.0,tf,nSteps)
   call linspace(xb,0.0,L,nxb)
   x = xb(2:nxb-1)
   u_mean = u0*sin(2*pi*x)

   progress = 0.0

   do i = 1,nSamples 
      u_init = u_mean + gaussianNoise(nx,u_sigma)
      nu = nu_mean + gaussianNoise(nu_sigma)
      call burgers(nSteps,nx,t,x,xb,u_init,nu,nDumps,verb,xShock_ind,tShock_ind,&
			uFull(:,:,i),uxFull(:,:,i))
		tShock(i) = t(tShock_ind)
		uShock(:,i) = uFull(:,tShock_ind,i)
		uxShock(:,i) = uxFull(:,tShock_ind,i)
		post_uxShock(i) = abs(uxFull(xShock_ind,tShock_ind,i))
      call trackProgress(i,nSamples,progress)
   end do

   call system_clock(t2,clock_rate,clock_max)

   write(*,'(a)') ''
   write(*,'(a)') 'SIMULATIONS...done.'
   write(*,'(a)') ''
   write(*,'(a)') 'POST-PROCESSING...'

	dumpFrequency = floor(real(nSteps-1)/real(nDumps-1))
	k = 1
	do i = 1,nSteps
		if (mod(i-1,dumpFrequency)==0) then
			tDump_ind(k) = i
			tDump(k) = t(i)
			k = k + 1
		end if
	end do

	mean_tShock = sum(tShock)/nSamples
	std_tShock = sqrt(sum((tShock-mean_tShock)**2)/nSamples)

	mean_uFull = 0.0
	mean_uxFull = 0.0
	mean_uShock = 0.0
	mean_uxShock = 0.0
	do i = 1,nSamples
		mean_uFull = mean_uFull + uFull(:,:,i)	
		mean_uxFull = mean_uxFull + uxFull(:,:,i)
		mean_uShock = mean_uShock + uShock(:,i)
		mean_uxShock = mean_uxShock + uxShock(:,i)
	end do
	mean_uFull = mean_uFull/nSamples
	mean_uxFull = mean_uxFull/nSamples
	mean_uShock = mean_uShock/nSamples
	mean_uxShock = mean_uxShock/nSamples

	std_uFull = 0.0
	std_uxFull = 0.0
	std_uShock = 0.0
	std_uxShock = 0.0
	do i = 1,nSamples
		std_uFull = std_uFull + (uFull(:,:,i) - mean_uFull)**2
		std_uxFull = std_uxFull + (uxFull(:,:,i) - mean_uxFull)**2
		std_uShock = std_uShock + (uShock(:,i) - mean_uShock)**2
		std_uxShock = std_uxShock + (uxShock(:,i) - mean_uxShock)**2
	end do
	std_uFull = sqrt(std_uFull/nSamples)
	std_uxFull = sqrt(std_uxFull/nSamples)
	std_uShock = sqrt(std_uShock/nSamples)
	std_uxShock = sqrt(std_uxShock/nSamples)

	call printArray(mean_uFull,'mean_uFull.dat')
	call printArray(mean_uxFull,'mean_uxFull.dat')
	call printArray(std_uFull,'std_uFull.dat')
	call printArray(std_uxFull,'std_uxFull.dat')
	call printArray(mean_uShock,'mean_uShock.dat')
	call printArray(mean_uxShock,'mean_uxShock.dat')
	call printArray(std_uShock,'std_uShock.dat')
	call printArray(std_uxShock,'std_uxShock.dat')
	call printArray(post_uxShock,'post_uxShock.dat')
   call printArray(x,'x.dat')
   call printArray(xb,'xb.dat')
	call printArray(t,'t.dat')
	call printArray(tDump,'tDump.dat')
	call printArray(tDump_ind,'tDump_ind.dat')
	call printArray((/mean_tShock,std_tShock/),'tShock.dat')

   call system_clock(t3,clock_rate,clock_max)

   simulationTime = real(t2-t1)/real(clock_rate)
   postTime = real(t3-t2)/real(clock_rate)
   totalTime = real(t3-t1)/real(clock_rate)

	call writeStats()

contains

	subroutine writeReadme()

		write(*,*) 'nSteps = ',nSteps
		write(*,*) 'nx = ',nx
		write(*,*) 'nSamples = ',nSamples
		write(*,*) 'nu_mean = ',nu_mean
		write(*,*) 'nu_sigma = ',nu_sigma
		write(*,*) 'u0 = ',u0
		write(*,*) 'u_sigma = ',u_sigma
		write(*,*) 'tf = ',tf
		write(*,*) 'verb = ',verb
		write(*,*) 'L = ',L	

		open(unit=1,file='readme.dat',action='write',status='replace')
		write(1,*) 'nSteps = ',nSteps
		write(1,*) 'nx = ',nx
		write(1,*) 'nSamples = ',nSamples
		write(1,*) 'nu_mean = ',nu_mean
		write(1,*) 'nu_sigma = ',nu_sigma
		write(1,*) 'u0 = ',u0
		write(1,*) 'u_sigma = ',u_sigma
		write(1,*) 'tf = ',tf
		write(1,*) 'verb = ',verb
		write(1,*) 'L = ',L	
		close(1)

	end subroutine

	subroutine writeStats()

		open(unit=1,file='readme.dat',action='write',status='old',position='append')
		write(1,'(a,f6.5,a)') 'Average Time Per Simulation: ', simulationTime/nSamples, ' sec'
		write(1,'(a,f6.0,a)') 'Total Simulation Time: ', simulationTime, ' sec'
		write(1,'(a,f6.2,a)') 'Total Post-Processing Time: ', postTime, ' sec'
		write(1,'(a,f6.0,a)') 'Total Time Elapsed: ', totalTime, ' sec'
		close(1)

		write(*,'(a)') ''
		write(*,'(a,f6.5,a)') 'Average Time Per Simulation: ', simulationTime/nSamples, ' sec'
		write(*,'(a,f6.0,a)') 'Total Simulation Time: ', simulationTime, ' sec'
		write(*,'(a,f6.2,a)') 'Total Post-Processing Time: ', postTime, ' sec'
		write(*,'(a,f6.0,a)') 'Total Time Elapsed: ', totalTime, ' sec'
		write(*,'(a)') ''

	end subroutine

   subroutine init_random_seed()
      integer :: i, n, clock
      integer, dimension(:), allocatable :: seed
      call random_seed(size = n)
      allocate(seed(n))
      call system_clock(count=clock)
      seed = clock + 37 * (/ (i - 1, i = 1, n) /)
      call random_seed(put = seed)
      deallocate(seed)
    end subroutine

end program
