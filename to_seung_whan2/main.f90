program main

	implicit none

	integer, parameter :: N = 5 ! 'parameter' means it's unchangeable after declaration
	real, allocatable :: x_allocatable(:)
	real, allocatable :: A_allocatable(:,:)
	real, allocatable :: b_allocatable(:)
	real :: x(N)
	real :: A(N,N)
	real :: b(N)
	real :: v(N)
	real :: w(N)
	real :: y(N)
	real :: Z(N,N)
	integer :: i,j
	character(32) :: filename ! You can make this longer than 32 characters, if needed

	! print to screen
	print *, 'calling program main'

	allocate(x_allocatable(N))
	allocate(A_allocatable(N,N))
	allocate(b_allocatable(N))

	do j = 1,N
		x(j) = j
		x_allocatable(j) = j
		do i = 1,N
			A(i,j) = i + j
			A_allocatable(i,j) = i + j
			print *, 'i=',i,',  j=',j
		end do
	end do

	! You can also assigned all elements of the vector/matrix to the same value:
	! x = 1.0
	! A = 1.0
	!
	! Or, you can assign vectors in the following manner:
	! x = (/ 1.0, 2.0, 3.0, 4.0, 5.0 /)

	b = matmul(A,x)
	b_allocatable = matmul(A_allocatable,x_allocatable) ! Same as above operation
	w = b**2 ! square each element of b and assign it to w
	v = b + w ! elementwise addition
	y = sin(b) + log(b) ! elementise operation
	Z = A**N ! square each element of A and assign it to Z
	
	! opens a file with unit number of 1, and replaces any file with the same name
	! You can pretty much choose any unit number you want
	open(unit=1,file='b.out',status='replace') 
	write(1,*) b ! write to unit 1, with generic formatting (the asterisk)
	close(1)

	! we can reuse the unit number 1 because we've closed it	
	open(unit=1,file='x.out',status='replace')
	write(1,*) x
	close(1)

	open(unit=1,file='w.out',status='replace')
	write(1,*) w
	close(1)

	open(unit=10,file='v.out',status='replace') 
	write(10,*) v
	close(10)

	open(unit=100,file='y.out',status='replace') 
	write(100,*) y
	close(100)

	! write(.,.) only writes a sequence of number (vector) but doesn't know how to 
	! line break when printing matrices.  So we loop:
	open(unit=100,file='A.out',status='replace') 
	open(unit=101,file='Z.out',status='replace') 
	do i = 1,N
		write(100,*) A(i,:)
		write(101,*) Z(i,:)
	end do
	close(100)
	close(101)

	! print to screen
	print *, 'program main...done.'

contains

	! You can add custom subroutines/functions here later, if you want

end program
