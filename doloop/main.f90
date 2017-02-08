program main

	implicit none


	integer :: i,j,k(10), n
	real :: input(4,2)

	! print to screen
	print *, 'calling program main'

	input(:,1) = (/ 0.2, 0.5, 0.8, 1.1 /)
	input(:,2) = (/ 3.2, 1.5, 2.8, 4.1 /)

	do i=1,0
		print *, 'it will not appear : ',input(:,i+1)
	end do

	do i=1,1
		print *, 'it will show i=2 : ',input(:,i+1)
	end do

	do i=2,1
		print *, 'it will not appear : ',input(:,i)
	end do

	do i=2,0
		print *, 'it also will not appear : ',input(:,i)
	end do

	do i=2,2
		print *, 'it will show i=1 : ',input(:,i-1)
	end do

	i = 5
	j = 8
	do while( i<j )
		print *, i,j
		i = i+1
	end do

	j = 12
	k = (/ (i, i=1,10) /)
	i = 1
	n = 10
	do while(i.le.n)
		if( mod(j,k(i)).eq.0 ) then
			k(i) = k(n)
			n = n-1
			i = i-1
		end if
		i = i+1
		print *, i, n, ', (',k,')'
	end do
	! print to screen
	print *, 'program main...done.'

contains

end program
