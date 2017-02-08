program main

	implicit none


	integer :: i
	real :: input(4), output(4)

	! print to screen
	print *, 'calling program main'

	input = (/ 0.2, 0.5, 0.8, 1.1 /)

	open(unit=1,file='data/data.bin',status='replace',form='unformatted',access='stream')
	write(1) input
	close(1)

	open(unit=1,file='data/data.bin',form='unformatted',access='stream')
	read(1) output
	close(1)
	do i=1,size(input)
		print *, 'input(',i,')=',input(i),', output(',i,')=',output(i)
	end do

	print *, FLOOR(-0.1)
	! print to screen
	print *, 'program main...done.'

contains

	! You can add custom subroutines/functions here later, if you want

end program
