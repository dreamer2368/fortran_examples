function cat_vec(v1,v2) result(output)
! Concatenates v1 and v2 side-by-side if opt = 1, and head-to-tail if opt=2.
	real, intent(in) :: v1(:)
	real, intent(in) :: v2(:)
	real, allocatable :: output(:,:)
	integer :: s1(1)
	s1 = shape(v1)
	allocate(output(s1(1),2))
	output(:,1) = v1
	output(:,2) = v2
end function


function cat_arr(A1,A2,opt) result(output)
! Concatenantes A1 and A2 horizontally if opt = 1 and vertically if dir = 2
	integer, intent(in) :: opt
	real, intent(in) :: A1(:,:)
	real, intent(in) :: A2(:,:)
	real, allocatable :: output(:,:)
	integer :: s1(2)
	integer :: s2(2)
	s1 = shape(A1)
	s2 = shape(A2)
	if (opt == 1) then
		allocate(output(s1(1),s1(2)+s2(2)))
		output(:,1:s1(2)) = A1
		output(:,s1(2)+1:s1(2)+s2(2)) = A2
	elseif (opt == 2) then
		allocate(output(s1(1)+s2(1),s1(2)))
		output(1:s1(1),:) = A1
		output(s1(1)+1:s1(1)+s2(1),:) = A2
	end if
end function

function cat_mixed(v,A,opt) result(output)
! Concatenates a column vector on the left side of A if opt = 1, right side if opt = 2
	real, intent(in) :: v(:)
	real, intent(in) :: A(:,:)
	integer, intent(in) :: opt
	real, allocatable :: output(:,:)
	integer :: s(2)
	s = shape(A)
	allocate(output(s(1),s(2)+1))
	if (opt == 1) then
		output(:,1) = v
		output(:,2:s(2)+1) = A
	else if (opt == 2) then
		output(:,1:s(2)) = A
		output(:,s(2)+1) = v
	end if
end function

