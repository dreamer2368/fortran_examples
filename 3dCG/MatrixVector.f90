module MatrixVector

!	use modPlasma										!machine precision mp should be assigned

	implicit none

	integer, parameter :: mp = SELECTED_REAL_KIND(15)

contains

	function Gradient(x,dx,Ng) result(y)						!Derivative with periodic BC
		real(mp), intent(in) :: x(:,:,:)						!(nx,ny,nz)
		real(mp), intent(in) :: dx(3)							!dx, dy, dz
		integer, intent(in) :: Ng(3)							!nx, ny, nz
		real(mp) :: y(size(x,1),size(x,2),size(x,3),3)
		integer :: i

		y=0.0_mp
		do i=2,Ng(1)-1
			y(i,:,:,1) = 0.5_mp/dx(1)*( x(i+1,:,:) - x(i-1,:,:) )
		end do
		y(1,:,:,1) = 0.5_mp/dx(1)*( x(2,:,:) - x(Ng(1),:,:) )
		y(Ng(1),:,:,1) = 0.5_mp/dx(1)*( x(1,:,:) - x(Ng(1)-1,:,:) )

		do i=2,Ng(2)-1
			y(:,i,:,2) = 0.5_mp/dx(2)*( x(:,i+1,:) - x(:,i-1,:) )
		end do
		y(:,1,:,2) = 0.5_mp/dx(2)*( x(:,2,:) - x(:,Ng(2),:) )
		y(:,Ng(2),:,2) = 0.5_mp/dx(2)*( x(:,1,:) - x(:,Ng(2)-1,:) )

		do i=2,Ng(3)-1
			y(:,:,i,3) = 0.5_mp/dx(3)*( x(:,:,i+1) - x(:,:,i-1) )
		end do
		y(:,:,1,3) = 0.5_mp/dx(3)*( x(:,:,2) - x(:,:,Ng(3)) )
		y(:,:,Ng(3),3) = 0.5_mp/dx(3)*( x(:,:,1) - x(:,:,Ng(3)-1) )
	end function

	function multiplyK(x,dx,Ng) result(y)						!set BC : x(0)=0
		real(mp), intent(inout) :: x(:,:,:)						!(nx,ny,nz)
		real(mp), intent(in) :: dx(3)							!dx, dy, dz
		integer, intent(in) :: Ng(3)							!nx, ny, nz
		real(mp) :: y(size(x,1),size(x,2),size(x,3))!, x1(size(x))
		integer :: k

!		x1 = x
		x(1,1,1) = 0.0_mp									!set BC

		y = 0.0_mp
		do k=2,Ng(3)-1
			y(:,:,k) = y(:,:,k) + ( x(:,:,k+1) - 2.0_mp*x(:,:,k) + x(:,:,k-1) )/dx(3)/dx(3)
		end do
		y(:,:,1) = y(:,:,1) + ( x(:,:,2) - 2.0_mp*x(:,:,1) + x(:,:,Ng(3)) )/dx(3)/dx(3)
		y(:,:,Ng(3)) = y(:,:,Ng(3)) + ( x(:,:,1) - 2.0_mp*x(:,:,Ng(3)) + x(:,:,Ng(3)-1) )/dx(3)/dx(3)

		do k=2,Ng(2)-1
			y(:,k,:) = y(:,k,:) + ( x(:,k+1,:) - 2.0_mp*x(:,k,:) + x(:,k-1,:) )/dx(2)/dx(2)
		end do
		y(:,1,:) = y(:,1,:) + ( x(:,2,:) - 2.0_mp*x(:,1,:) + x(:,Ng(2),:) )/dx(2)/dx(2)
		y(:,Ng(2),:) = y(:,Ng(2),:) + ( x(:,1,:) - 2.0_mp*x(:,Ng(2),:) + x(:,Ng(2)-1,:) )/dx(2)/dx(2)

		do k=2,Ng(1)-1
			y(k,:,:) = y(k,:,:) + ( x(k+1,:,:) - 2.0_mp*x(k,:,:) + x(k-1,:,:) )/dx(1)/dx(1)
		end do
		y(1,:,:) = y(1,:,:) + ( x(2,:,:) - 2.0_mp*x(1,:,:) + x(Ng(1),:,:) )/dx(1)/dx(1)
		y(Ng(1),:,:) = y(Ng(1),:,:) + ( x(1,:,:) - 2.0_mp*x(Ng(1),:,:) + x(Ng(1)-1,:,:) )/dx(1)/dx(1)

		y(1,1,1) = 0.0_mp									!we don't solve Kx=b at x(1,1,1).
	end function

	subroutine CG_K(x,b,dx,Ng)								!Kx = b
		real(mp), intent(in) :: b(:,:,:)					!b(1,1,1) = 0 as BC.
		real(mp), intent(in) :: dx(3)
		integer, intent(in) :: Ng(3)
		real(mp), intent(out) :: x(:,:,:)					!x(nx,ny,nz)
		real(mp) :: r(size(x,1),size(x,2),size(x,3)), p(size(x,1),size(x,2),size(x,3)), r1(size(x,1),size(x,2),size(x,3))
		real(mp) :: alpha, beta
		real(mp) :: tol
		integer :: k = 0

		select case (mp)
			case (SELECTED_REAL_KIND(4))
				tol = 1.0e-16
			case (SELECTED_REAL_KIND(15))
				tol = 1.0e-32
			case (SELECTED_REAL_KIND(33))
				tol = 10.0_mp**(-64.0_mp)
			case default
				tol = 1.0e-32
		end select

		if( size(b)/=size(x) ) then
			print *, '===================================================='
			print *, '====================  FAULT  ======================='
			print *, '=========  x AND b HAVE NOT EQUAL SIZES  ==========='
			print *, '====================  Ax=b  ========================'
			print *, '===================================================='
			stop
		end if

		if( b(1,1,1) .ne. 0.0_mp ) then
			print *, '===================================================='
			print *, '====================  FAULT  ======================='
			print *, '=================  b(1,1,1)=/=0  ==================='
			print *, '===========  Not solvable for x(1,1,1)=0  =========='
			print *, '===================================================='
			stop
		end if

		x = 0.0_mp
		r = b - multiplyK(x,dx,Ng)
		p = r

		k = 0
		do
			alpha = SUM(r*r)/SUM(p*multiplyK(p,dx,Ng))
			x = x + alpha*p
			r1 = r
			r = r - alpha*multiplyK(p,dx,Ng)
			if( SUM(r*r) < tol ) then
!				print *, SUM(r*r)
				exit
			end if
			beta = SUM(r*r)/SUM(r1*r1)
			p = r + beta*p
			k = k+1
			if( k > 1e8 ) then
				print *, '====================================='
				print *, '=========  CG METHOD FAILS   ========'
				print *, '====================================='
				stop
			end if
		end do
	end subroutine

end module