module ModUQ

   use ModLib
	use ModRandom

   implicit none

   interface gaussianNoise
      procedure gaussianNoise0D
      procedure gaussianNoise1D
   end interface

contains

   function gaussianNoise0D(sigma) result(noise)
      real, intent(in) :: sigma
      real :: noise
      noise = randn(0.0,sigma)
   end function

   function gaussianNoise1D(n,sigma) result(noise)
      integer, intent(in) :: n
      real, intent(in) :: sigma
      real :: noise(n)
      integer :: i
      do i = 1,n
         noise(i) = randn(0.0,sigma)
      end do
   end function

   function randn(mu,sigma) result(r)
      real, intent(in) :: mu
      real, intent(in) :: sigma
      real :: r
      r = random_normal()
      r = r*sigma + mu
   end function

	function randb(alpha,beta) result(r)
		real, intent(in) :: alpha
		real, intent(in) :: beta
		real :: r
		r = random_beta(alpha,beta,.true.)
	end function

	function randl(mu,sigma) result(r)
		real, intent(in) :: mu
		real, intent(in) :: sigma
		real :: r
		r = exp(randn(mu,sigma))
	end function

end module
