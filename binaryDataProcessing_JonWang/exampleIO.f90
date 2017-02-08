program exampleIO

   integer, parameter :: m = 6
   integer, parameter :: n = 4
   real :: A(m,n)
   integer :: i,j

   do j = 1,n
      do i = 1,m
         A(i,j) = real(10*i+j)
      end do
   end do

   call printArray(A,'A.dat')

contains

   subroutine printArray(array,filename)
      real, intent(in) :: array(:,:)
      character(*), intent(in) :: filename
      open(unit=1,file=trim(adjustl(filename)),access='sequential',form='unformatted',&
         status='replace')
      write(1) array
      close(1)
      print *, 'File Written (BINARY): ', filename
   end subroutine

end program
