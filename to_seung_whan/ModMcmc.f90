module ModMcmc

   use ModLib

	implicit none

contains

	subroutine metropolis(nSamples,nBurn,nParams,sample0,deltaMax,state,n1,n2,n3,verb,mode,&
      samples,posts,preds,acceptRate,rejectRate,timePerSample)

		integer, intent(in) :: nSamples
		integer, intent(in) :: nBurn
		integer, intent(in) :: nParams
		real, intent(in) :: sample0(:)
		real, intent(in) :: deltaMax(:)
      integer, intent(in) :: n1
      integer, intent(in) :: n2
      integer, intent(in) :: n3
		integer, intent(in) :: verb
      integer, intent(out) :: mode ! index of sample w/highest posterior
		real, intent(out) :: samples(nParams,nSamples) ! All accepted samples
		real, intent(out) :: posts(nSamples) ! Posterior of all samples
      real, intent(out) :: preds(n1,n2,n3,nSamples)
		real, intent(out) :: acceptRate
		real, intent(out) :: rejectRate
      real, intent(out) :: timePerSample
		integer :: acceptCount
		integer :: rejectCount
		real :: post
		real :: postNew
      real :: pred(n1,n2,n3)
      real :: predNew(n1,n2,n3)
      real :: modePost
		real :: delta(nParams)
		real :: sample(nParams)
		real :: sampleNew(nParams)
		real :: randArray(nParams)
		real :: randNum
		integer :: i
      real :: progress
      integer :: t1
      integer :: t2
      integer :: clock_rate
      integer :: clock_max

		interface
			subroutine state(sample,post,pred)
				real, intent(in) :: sample(:)
				real, intent(out) :: post
            real, intent(out) :: pred(:,:,:)
			end subroutine
		end interface	

		call init_random_seed()

      progress = 0.
		sample = sample0
		call state(sample,post,pred)
      call mcmcReport(0,nSamples,nBurn,.true.,progress,sample,sampleNew,post,postNew,verb)
		do i = 1,nBurn
			call random_number(randArray)
			delta = 2*(randArray-0.5)*deltaMax
			sampleNew = sample + delta
			call state(sampleNew,postNew,predNew)
			call random_number(randNum)
			if (postNew/post > randNum) then
            pred = predNew
				post = postNew
				sample = sampleNew
            call mcmcReport(-i,nSamples,nBurn,.true.,progress,sample,sampleNew,post,postNew,verb)
         else
            call mcmcReport(-i,nSamples,nBurn,.false.,progress,sample,sampleNew,post,postNew,verb)
			end if
		end do

      progress = 0.
		acceptCount = 0
		rejectCount = 0
      modePost = post
      mode = 1

      call system_clock(t1,clock_rate,clock_max)
		do i = 1,nSamples
			call random_number(randArray)
			delta = 2.*(randArray-0.5)*deltaMax
			sampleNew = sample + delta
			call state(sampleNew,postNew,predNew)
			call random_number(randNum)
			if (postNew/post > randNum) then
				acceptCount = acceptCount + 1
            pred = predNew
				post = postNew
				sample = sampleNew
            call mcmcReport(i,nSamples,nBurn,.true.,progress,sample,sampleNew,post,postNew,verb)
			else
				rejectCount = rejectCount + 1
            call mcmcReport(i,nSamples,nBurn,.false.,progress,sample,sampleNew,post,postNew,verb)
			end if
         if (post > modePost) then
            modePost = post
            mode = i
         end if
         preds(:,:,:,i) = pred
         samples(:,i) = sample
         posts(i) = post
		end do
      call system_clock(t2,clock_rate,clock_max)
      timePerSample = (real(t2-t1)/real(clock_rate))/real(nSamples)

      acceptRate = real(acceptCount)/real(nSamples)
      rejectRate = real(rejectCount)/real(nSamples)

	end subroutine

   subroutine mcmcReport(i,nSamples,nBurn,accepted,progress,sample,sampleNew,post,postNew,verb)
      integer, intent(in) :: i
      integer, intent(in) :: nSamples
      integer, intent(in) :: nBurn
      logical, intent(in) :: accepted
      real, intent(inout) :: progress
      real, intent(in) :: sample(:)
      real, intent(in) :: sampleNew(:)
      real, intent(in) :: post
      real, intent(in) :: postNew
      integer, intent(in) :: verb
      integer :: nn
      if (verb == 1) then
         if (i == -1) then
            write(*,'(a,$)') '   Begin Burn-in... '
         elseif (i == 1) then
            write(*,*) ''
            write(*,'(a,$)') '   Begin Sampling...'
         end if
         if (i /= 0) then
            if (i < 0) then
               nn = nBurn
            elseif (i > 0) then
               nn = nSamples
            end if
            call trackProgress(abs(i),nn,progress)
         end if
      elseif (verb >= 2) then
         if (i == -1) then
            write(*,*) 'Burn-in... '
         elseif (i == 1) then
            write(*,*) 'Begin Sampling...'
         end if
         if (i /= 0 .and. verb == 3) then
            if (accepted) then
               call mcmcPrintTrial('Accepted',sampleNew,postNew)
            else
               call mcmcPrintTrial('Rejected',sampleNew,postNew)
            end if
         end if
         call mcmcPrintSample(i,sample,post)
      end if
   end subroutine

	subroutine mcmcPrintSample(i,sample,post)
		integer, intent(in) :: i
		real, intent(in) :: sample(:)
		real, intent(in) :: post
		integer :: j
      write(*,'(a,i6,a,$)') '[MCMC] Sample',i,':  State = ['
      do j = 1,size(sample)
         write(*,'(es11.4,a,$)') sample(j),', '
      end do
      write(*,'(a,es11.4)') ']  Post=',post
	end subroutine

	subroutine mcmcPrintTrial(header,sample,post)

      character(*), intent(in) :: header
		real, intent(in) :: sample(:)
		real, intent(in) :: post
		integer :: j
      write(*,'(a,a,a,$)') '[MCMC] <',header,'> :   State = ['
      do j = 1,size(sample)
         write(*,'(es11.4,a,$)') sample(j),', '
      end do
      write(*,'(a,es11.4)') ']  Post=',post
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

!	! Pulled from https://gcc.gnu.org/onlinedocs/gfortran/RANDOM_005fSEED.html#RANDOM_005fSEED 
!	subroutine init_random_seed()
!		use iso_fortran_env, only: int64
!		implicit none
!		integer, allocatable :: seed(:)
!		integer :: i, n, un, istat, dt(8), pid
!		integer(int64) :: t
!
!		call random_seed(size = n)
!		allocate(seed(n))
!		! First try if the OS provides a random number generator
!		open(newunit=un, file="/dev/urandom", access="stream", &
!			  form="unformatted", action="read", status="old", iostat=istat)
!		if (istat == 0) then
!			read(un) seed
!			close(un)
!		else
!			! Fallback to XOR:ing the current time and pid. The PID is
!			! useful in case one launches multiple instances of the same
!			! program in parallel.
!			call system_clock(t)
!			if (t == 0) then
!				call date_and_time(values=dt)
!				t = (dt(1) - 1970) * 365_int64 * 24 * 60 * 60 * 1000 &
!					  + dt(2) * 31_int64 * 24 * 60 * 60 * 1000 &
!					  + dt(3) * 24_int64 * 60 * 60 * 1000 &
!					  + dt(5) * 60 * 60 * 1000 &
!					  + dt(6) * 60 * 1000 + dt(7) * 1000 &
!					  + dt(8)
!			end if
!			pid = getpid()
!			t = ieor(t, int(pid, kind(t)))
!			do i = 1, n
!				seed(i) = lcg(t)
!			end do
!		end if
!		call random_seed(put=seed)
!	end subroutine
!
!	! This simple PRNG might not be good enough for real work, but is
!	! sufficient for seeding a better PRNG.
!	! Pulled from https://gcc.gnu.org/onlinedocs/gfortran/RANDOM_005fSEED.html#RANDOM_005fSEED 
!	function lcg(s)
!		use iso_fortran_env, only: int64
!		integer :: lcg
!		integer(int64) :: s
!		if (s == 0) then
!			s = 104729
!		else
!			s = mod(s, 4294967296_int64)
!		end if
!		s = mod(s * 279470273_int64, 4294967291_int64)
!		lcg = int(mod(s, int(huge(0), int64)), kind(0))
!	end function lcg

end module
