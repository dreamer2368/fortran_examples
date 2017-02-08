subroutine trackProgress(i,N,progress)
   integer, intent(in) :: i   
   integer, intent(in) :: N
   real, intent(inout) :: progress
   real, parameter:: increment = 0.05
   real :: p
   p = int((real(i)/real(N))/increment)*increment
   if (p > progress) then
      write(*,'(i3,a,$)') nint(p*100),'% '
      progress = p
   end if
end subroutine

function findMaxSlope(x,y) result(inflection)
	real, intent(in) :: y(:)
	real, intent(in) :: x(:)
	real :: inflection(2)
	integer :: inflIndex
	real :: dy
	real :: maxdy
	real :: h
	real :: coeffs(3)
	integer :: i
	integer :: N
	integer :: ghost ! number of points in each direction used to compute derivative

	coeffs = (/ -1d0/2d0, 0d0, 1d0/2d0 /)

	ghost = floor(size(coeffs)/2d0)
	h = x(2)-x(1)
	N = size(y)

	maxdy = 0
	inflIndex = 1+ghost
	do i = 1+ghost,N-ghost
		dy = abs(sum(coeffs*y(i-ghost:i+ghost))/h**2)
		if (dy > maxdy) then
			maxdy = dy
			inflIndex = i
		end if
	end do

	inflection(1) = x(inflIndex)
	inflection(2) = y(inflIndex)

end function

function isNaN(x)
   real :: x
   logical :: isNaN  
   isNaN = (x /= x)
end function 

function isInf(x)
   real :: x
   logical :: isInf
   real :: infinity
   infinity = huge(dbl_prec_var)
   isInf = x > infinity
end function

subroutine findNeighbors(v,x,j1,j2)
   real, intent(in) :: v(:)
   real, intent(in) :: x
   integer, intent(out) :: j1
   integer, intent(out) :: j2
   integer :: i
   real :: diff
   j1 = 1
   j2 = 1
   diff = abs(v(1)-x)
   do i = 2,size(v)
      if (abs(v(i)-x) < diff) then
         diff = abs(v(i)-x)
         j1 = i
         j2 = j1
      end if
   end do
end subroutine

function findMember(x,v) result(ii)
   real :: x
   real :: v(:)
   integer :: i
   integer :: ii
   real, parameter :: tol = 1e-14
   ii = 0
   do i = 1,size(v)
      if (abs(v(i)-x) < tol) then
         ii = i
         exit
      end if
   end do
!   ii = minloc(abs(v-x),1)
!   if (abs(v(ii(1))-x) < tol) then
!      i = ii(1)
!   else
!      i = 0
!   end if
end function


