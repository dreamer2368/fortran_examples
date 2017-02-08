function meanInteger1d(x) result(mean)
	integer :: x(:)
	real :: mean
	mean = real(sum(x))/real(size(x))
end function

function meanInteger2d(x) result(mean)
	integer :: x(:,:)
	real :: mean
	mean = real(sum(x))/real(size(x))
end function

function meanInteger3d(x) result(mean)
	integer :: x(:,:,:)
	real :: mean
	mean = real(sum(x))/real(size(x))
end function

function meanReal1d(x) result(mean)
	real :: x(:)
	real :: mean
	mean = sum(x)/size(x)
end function

function meanReal2d(x) result(mean)
	real :: x(:,:)
	real :: mean
	mean = sum(x)/size(x)
end function

function meanReal3d(x) result(mean)
	real :: x(:,:,:)
	real :: mean
	mean = sum(x)/size(x)
end function

function meanDouble1d(x) result(mean)
	double precision :: x(:)
	double precision :: mean
	mean = sum(x)/size(x)
end function

function meanDouble2d(x) result(mean)
	double precision :: x(:,:)
	double precision :: mean
	mean = sum(x)/size(x)
end function

function meanDouble3d(x) result(mean)
	double precision :: x(:,:,:)
	double precision :: mean
	mean = sum(x)/size(x)
end function

