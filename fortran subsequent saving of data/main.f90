program main

	implicit none


	integer :: i,j
	real :: input(4,2), output(4,2), output2(4,2)
	character(40) :: numchr

	! print to screen
	print *, 'calling program main'

	input(:,1) = (/ 0.2, 0.5, 0.8, 1.1 /)
	input(:,2) = (/ 3.2, 1.5, 2.8, 4.1 /)

	write(numchr,*) 102

	call openStream
	call writeStream
	print *, 'hello'
	call closeStream

	open(unit=1,file='data/data.bin',form='unformatted',access='stream')
	read(1) output
	close(1)

	open(unit=1,file='data/data'//trim(adjustl(numchr))//'.bin',form='unformatted',access='stream')
	read(1) output2
	close(1)

	print *, size(input)		! size(array) = row * column

	do i=1,size(input,1)
		do j=1,size(input,2)
			print *, 'input(',i,',',j,')=',output(i,j),', input2(',i,',',j,')=',output2(i,j)
		end do
	end do

	! print to screen
	print *, 'program main...done.'

contains

	!Standard FORTRAN reserves two UNIT numbers for I/O to user. They are:
	!UNIT = 5   for INPUT from the keyboard with the READ statement
	!UNIT = 6   for OUTPUT to the screen with the WRITE statement
	!don't use them for stream unit.
	subroutine openStream()
		open(unit=1,file='data/data.bin',status='replace',form='unformatted',access='stream')
		open(unit=2,file='data/data'//trim(adjustl(numchr))//'.bin',status='replace',form='unformatted',access='stream')
	end subroutine

	subroutine closeStream()
		close(1)
		close(2)
	end subroutine

	subroutine writeStream()
		write(1) input
		write(2) input(:,1)
		write(2) input(:,2)
	end subroutine

end program
