module FFTSolver

	use constants										!machine precision mp should be assigned

	implicit none

	include 'fftw3.f'

contains

	subroutine forwardFFT(A,N)
		integer, intent(in) :: N(3)
		integer*8 :: plan
		real(mp), intent(in), dimension(N(1),N(2),N(3)) :: A
		complex, dimension(N(1),N(2),N(3)) :: input, output
		real(mp), dimension(N(1),N(2),N(3)) :: output_real

		input = A
		call dfftw_plan_dft_3d(plan,N(1),N(2),N(3),input,output,FFTW_FORWARD,FFTW_ESTIMATE)
		call dfftw_execute_dft(plan,input,output)
		call dfftw_destroy_plan(plan)

		output_real = ABS(output)
		open(unit=501,file='data/yFFT.bin',status='replace',form='unformatted',access='stream')
		write(501) output_real
		close(501)
	end subroutine

	subroutine FFTPoisson_setup(N,L,W)
		integer, intent(in) :: N(3)
		real(mp), intent(in) :: L(3)
		complex(mp), intent(out) :: W(N(1),N(2),N(3))
		integer :: i,j,k, wi,wj,wk
		complex(mp) :: wx,wy,wz

		wx = 2.0_mp*pi*eye/L(1)
		wy = 2.0_mp*pi*eye/L(2)
		wz = 2.0_mp*pi*eye/L(3)
		do k=0,N(3)-1
			if( k.le.N(3)/2 ) then
				wk = k
			else
				wk = - ( N(3)-k )
			end if
			do j=0,N(2)-1
				if( j.le.N(2)/2 ) then
					wj = j
				else
					wj = - ( N(2)-j )
				end if
				do i=0,N(1)-1
					if( i.le.N(1)/2 ) then
						wi = i
					else
						wi = - ( N(1)-i )
					end if
					W(i+1,j+1,k+1) = + (wx*wi)**2.0_mp + (wy*wj)**2.0_mp + (wz*wk)**2.0_mp
				end do
			end do
		end do
		W(1,1,1) = 1.0_mp
	end subroutine

	subroutine FFTPoisson(x,rhs,W)
		real(mp), intent(in) :: rhs(:,:,:)
		complex(mp), intent(in) :: W(size(rhs,1),size(rhs,2),size(rhs,3))
		real(mp), intent(out) :: x(size(rhs,1),size(rhs,2),size(rhs,3))
		integer*8 :: plan
		complex(mp), dimension(size(rhs,1),size(rhs,2),size(rhs,3)) :: rhsFFT, xFFT, rhsb, xb
		integer :: K,M,N

		K = size(rhs,1)
		M = size(rhs,2)
		N = size(rhs,3)

		rhsb = rhs
		call dfftw_plan_dft_3d(plan,K,M,N,rhsb,rhsFFT,FFTW_FORWARD,FFTW_ESTIMATE)
		call dfftw_execute_dft(plan,rhsb,rhsFFT)
		call dfftw_destroy_plan(plan)

		xFFT = rhsFFT/W
		xFFT(1,1,1) = (0.0_mp, 0.0_mp)

		call dfftw_plan_dft_3d(plan,K,M,N,xFFT,xb,FFTW_BACKWARD,FFTW_ESTIMATE)
		call dfftw_execute_dft(plan,xFFT,xb)
		call dfftw_destroy_plan(plan)

		x = REALPART(xb)*1.0_mp/K/M/N
	end subroutine

	subroutine FFTEfield(y,rhs,W,D)
		real(mp), intent(in) :: rhs(:,:,:), D(3)
		complex(mp), intent(in) :: W(size(rhs,1),size(rhs,2),size(rhs,3))
		real(mp), intent(out) :: y(size(rhs,1),size(rhs,2),size(rhs,3),3)			!!Gradient of the solution
		integer*8 :: plan
		complex(mp), dimension(size(rhs,1),size(rhs,2),size(rhs,3)) :: rhsb, rhsFFT, xFFT, yb, yFFT
		integer :: L,M,N, i, fi,fj,fk
		real(mp) :: dk, wi

		L = size(rhs,1)
		M = size(rhs,2)
		N = size(rhs,3)

		fi = L*3/20
		fj = M*3/20
		fk = N*3/20

		rhsb = rhs
		call dfftw_plan_dft_3d(plan,L,M,N,rhsb,rhsFFT,FFTW_FORWARD,FFTW_ESTIMATE)
		call dfftw_execute_dft(plan,rhsb,rhsFFT)
		call dfftw_destroy_plan(plan)

		xFFT = rhsFFT/W
		xFFT(1,1,1) = (0.0_mp, 0.0_mp)
!		xFFT(L/2-fi:L/2+fi,:,:) = (0.0_mp, 0.0_mp)
!		xFFT(:,M/2-fj:M/2+fj,:) = (0.0_mp, 0.0_mp)
!		xFFT(:,:,N/2-fk:N/2+fk) = (0.0_mp, 0.0_mp)

		!(-1)*gradient in z direction
		do i=0,N-1
			if( i.le.N/2 ) then
				wi = i
			else
				wi = - ( N-i )
!				wi = i
			end if
			yFFT(:,:,i+1) = xFFT(:,:,i+1)*2.0_mp*pi*eye*wi/D(3)
		end do

		call dfftw_plan_dft_3d(plan,L,M,N,yFFT,yb,FFTW_BACKWARD,FFTW_ESTIMATE)
		call dfftw_execute_dft(plan,yFFT,yb)
		call dfftw_destroy_plan(plan)
		y(:,:,:,3) = REALPART(yb)*1.0_mp/L/M/N

		!gradient in y direction
		do i=0,M-1
			if( i.le.M/2 ) then
				wi = i
			else
				wi = - ( M-i )
!				wi = i
			end if
			yFFT(:,i+1,:) = xFFT(:,i+1,:)*2.0_mp*pi*eye*wi/D(2)
		end do

		call dfftw_plan_dft_3d(plan,L,M,N,yFFT,yb,FFTW_BACKWARD,FFTW_ESTIMATE)
		call dfftw_execute_dft(plan,yFFT,yb)
		call dfftw_destroy_plan(plan)
		y(:,:,:,2) = REALPART(yb)*1.0_mp/L/M/N

		!gradient in x direction
		do i=0,L-1
			if( i.le.L/2 ) then
				wi = i
			else
				wi = - ( L-i )
!				wi = i
			end if
			yFFT(i+1,:,:) = xFFT(i+1,:,:)*2.0_mp*pi*eye*wi/D(1)
		end do

		open(unit=301,file='data/yFFTk.bin',status='replace',form='unformatted',access='stream')
		write(301) ABS(yFFT)
		close(301)

		call dfftw_plan_dft_3d(plan,L,M,N,yFFT,yb,FFTW_BACKWARD,FFTW_ESTIMATE)
		call dfftw_execute_dft(plan,yFFT,yb)
		call dfftw_destroy_plan(plan)
		y(:,:,:,1) = REALPART(yb)*1.0_mp/L/M/N
	end subroutine

end module