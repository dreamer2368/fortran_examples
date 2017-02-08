module fourierTransform

!	use modPlasma										!machine precision mp should be assigned

	implicit none

	integer, parameter :: mp = SELECTED_REAL_KIND(15)
	real(mp), parameter :: pi = 4.0_mp*ATAN(1.0_mp)
	complex(mp), parameter :: I = (0.0_mp,1.0_mp)

contains

	subroutine FFT1D(N,x,y)			!N = 2^k
		integer, intent(in) :: N
		real(mp), intent(in) :: x(N)
		complex(mp), intent(out) :: y(N)
		integer :: j,k,ktot
		complex(mp), allocatable :: w(:), wk(:)
		complex(mp) :: y0(N), y1(N)

		!check N=2^k
		if ( LOG(1.0_mp*N)/LOG(2.0_mp) .eq. FLOOR(LOG(1.0_mp*N)/LOG(2.0_mp)) ) then
			ktot = LOG(1.0_mp*N)/LOG(2.0_mp)
		else
			print *, '==========================================='
			print *, '================= FAULT ==================='
			print *, '======= N=/=2^k, program stopped. ========='
			print *, '==========================================='
			stop
		end if

		allocate(w(ktot))
		w = (/ ( -2.0_mp*pi*I/(2.0_mp**k) , k=1,ktot) /)
		w = EXP(w)

		y0 = x
		do k=1,ktot
			allocate(wk(2**k))
			wk = (/ ( w(k)**(j-1), j=1,2**(k-1) ) /)
			do j=1,2**(ktot-k)
				y1( (j-1)*(2**k)+1:(j-1)*(2**k)+2**(k-1) ) =	&
							y0( (j-1)*(2**(k-1))+1:j*(2**(k-1)) ) +	&
					wk*y0( 2**(ktot-1)+(j-1)*(2**(k-1))+1:2**(ktot-1)+j*(2**(k-1)) )
				y1( (j-1)*(2**k)+2**(k-1)+1:j*(2**k) ) =	&
							y0( (j-1)*(2**(k-1))+1:j*(2**(k-1)) ) -	&
					wk*y0( 2**(ktot-1)+(j-1)*(2**(k-1))+1:2**(ktot-1)+j*(2**(k-1)) )
			end do
			y0 = y1
			deallocate(wk)
		end do

		y = y0
		deallocate(w)
	end subroutine

end module