program Prog

   use demo

   integer, parameter :: N = 5
   real :: a
   real :: b
   real :: x
   real :: v(N)

   a = 2.0
   b = 1.0

   call smartRoutine(a,b,x)
   call smartRoutine(a,b,v)

   print *, 'x = ',x
   print *, 'v = ',v

	call testSubroutine( testQoI, a,b )
	print *, b

end program
