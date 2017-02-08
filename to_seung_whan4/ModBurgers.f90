module ModBurgers

   use ModLib

   implicit none

contains

   subroutine burgers(nSteps,nx,t,x,xb,u_init,nu,nDumps,verb,xShock_ind,tShock_ind,uFull,uxFull)


      integer, intent(in) :: nSteps
      integer, intent(in) :: nx
      real, intent(in) :: t(:)
      real, intent(in) :: x(:)
      real, intent(in) :: xb(:)
      real, intent(in) :: u_init(:)
      real, intent(in) :: nu
		integer, intent(in) :: nDumps
      integer, intent(in) :: verb
		integer, intent(out) :: xShock_ind
		integer, intent(out) :: tShock_ind
		real, intent(out) :: uFull(:,:)
		real, intent(out) :: uxFull(:,:)
      integer :: dumpFrequency
      real :: Dsparse(nx,nx)
      real :: ux(nx),ux1(nx),ux2(nx),ux3(nx),uxMax(nx)
      real :: u(nx),u1(nx),u2(nx),u3(nx)
      real :: u0,dt,dx,max_u0
      real :: b1,b2,b3,e1,e2,e3,r
      integer :: i,j,k
		real :: uMax
      real :: HL(nx-1), HD(nx), HU(nx-1), HU2(nx-2)
      integer :: ipiv(nx)
      integer :: info

100 format(a,i7,a,f9.6,a,f19.15,a,f19.15)

      dumpFrequency = nint(real(nSteps-1)/real(nDumps-1))

      ! Discretization
      dt = t(2)-t(1)
      dx = x(2)-x(1)
      u = u_init

      ! Centered difference for 1st derivative (convective term), sparse storage
      Dsparse = 0.0
      Dsparse(1,2:nx) = 1.0/(2.0*dx) ! upper diagonal
      Dsparse(3,1:nx-1) = -1.0/(2.0*dx) ! lower diagonal

      ! Initialize
      u3 = 0.0; ux3 = 0.0;
      u2 = 0.0; ux2 = 0.0;
      u1 = 0.0; ux1 = 0.0;

      ! Shock?
      call sparseMultiply(ux,Dsparse,u)
		uFull(:,1) = applyBC(u)
		uxFull(:,1) = ux
		uxMax = maxval(abs(ux))
		tShock_ind = 1
		xShock_ind = maxloc(abs(ux),1)

		! Report result to screen
      if (verb >= 2) then
         write(*,100) 'step = ',1,'; t = ',t(1),'; max(u) = ',maxval(abs(uFull)),&
				'; max(ux) = ',maxval(abs(uxFull))
      end if

      ! Step forward in time
      do i = 2,nSteps
         
         ! Set implicit,explicit coeffs
         if (i == 2) then
            r = 1.0
            b1 = 1.0; b2 = 0.0; b3 = 0.0;
            e1 = 1.0; e2 = 0.0; e3 = 0.0;
         elseif (i == 3) then
            r = 2.0/3.0
            b1 = 4.0/3.0; b2 = -1.0/3.0; b3 = 0.0;
            e1 = 2.0; e2 = -1.0; e3 = 0.0;
         else
            r = 2.0/3.0
            b1 = 4.0/3.0; b2 = -1.0/3.0; b3 = 0.0;
            e1 = 2.0; e2 = -1.0; e3 = 0.0;
!            r = 6.0/11.0
!            b1 = 18.0/11.0; b2 = -9.0/11.0; b3 = 2.0/11.0;
!            e1 = 3.0; e2 = -3.0; e3 = 1.0;
         end if

         ! LHS for implicit time stepper
         HL = -r*dt*nu*1.0/dx**2
         HD = 1.0-r*dt*nu*(-2.0)/dx**2
         HU = HL

         ! Convective term
         u3 = u2; u2 = u1; u1 = u
         call sparseMultiply(ux1,Dsparse,u1)
         call sparseMultiply(ux2,Dsparse,u2)
         call sparseMultiply(ux3,Dsparse,u3)

         ! Invert
         u = b1*u1 + b2*u2 + b3*u3 - r*dt*(e1*u1*ux1 + e2*u2*ux2 + e3*u3*ux3)
         call sparseSolve(u,HL,HD,HU,nx)

         ! Shock?
         call sparseMultiply(ux,Dsparse,u)
			uFull(:,i) = applyBC(u)
			uxFull(:,i) = ux
			if (maxval(abs(ux)) > uMax) then
				uMax = maxval(abs(ux))
				tShock_ind = i
				xShock_ind = maxloc(abs(ux),1)
			end if

         ! Report result to screen and dump solution
         if (mod(i-1,dumpFrequency)==0) then
            if (verb >= 2) then
               write(*,100) 'step = ',i,'; t = ',t(i),'; max(u) = ',maxval(abs(uFull)),&
						'; max(ux) = ',maxval(abs(uxFull))
            end if
			end if

      end do

101 format(a,es9.3,a,es9.3,a,es10.4,a,f6.4,a,f6.4,a,f8.4)

      if (verb >= 1) then
         write(*,101) '[Burgers''] dx=',dx,', dt=',dt,', nu=',nu,', u0max=',maxval(u_init),&
            ' >> uMaxMax=',maxval(abs(uFull)),', uxMaxMax=',maxval(abs(uxFull))
      end if

		if (maxval(abs(uFull)) > 2.0*maxval(u_init)) then
			print *, 'UNSTABLE'
		end if

   end subroutine

   function applyBC(u) result(ub)
      real, intent(in) :: u(:)
      real :: ub(size(u)+2)
      ub(1) = 0.0
      ub(2:size(u)+1) = u
      ub(size(u)+2) = 0.0
   end function

   subroutine sparseSolve(b,lower,diag,upper,N)
      ! Solves Ax=b, where UPPER, DIAG, and LOWER are the upper-diagonal, diagonal, and &
      ! lower-diagonal of A. Note: b is overwritten with x
      real, intent(inout) :: b(:)
      real, intent(in) :: lower(:)
      real, intent(in) :: diag(:)
      real, intent(in) :: upper(:)
      integer, intent(in) :: N
      integer, parameter :: NRHS = 1
      character(1) :: TRANS
      real :: bb(N,NRHS),upper2(N-2)
      integer :: LDB,info
      integer :: ipiv(N)
      TRANS = 'N'
      LDB = N
      call dgttrf(N,lower,diag,upper,upper2,ipiv,info)
      if (info /= 0) then
         print *, '*********** WARNING ************'
         print *, 'FAILED LU TRIDIAGONAL FACTORIZATION'
      end if
      bb(:,1) = b
      call dgttrs(TRANS,N,NRHS,lower,diag,upper,upper2,ipiv,bb,LDB,info)
      if (info /= 0) then
         print *, '*********** WARNING ************'
         print *, '   FAILED TRIDIAGONAL SOLVE'
      end if
      b = bb(:,1)
   end subroutine

   subroutine sparseMultiply(y,A,x) 
      ! Computes y=Ax, where A is a sparse matrix represented as required by
      ! LAPACK, assumed to be originally tridiagonal
      real, intent(in) :: A(:,:)
      real, intent(in) :: x(:)
      real, intent(out) :: y(:)
      integer :: M,N,KL,KU,LDA,INCX,INCY
      real :: ALPHA,BETA
      character(1) :: TRANS
      TRANS = 'N'
      M = size(x)
      N = M
      KL = 1
      KU = 1
      ALPHA = 1.0
      LDA = M
      INCX = 1
      BETA = 0.0
      INCY = 1
      call dgbmv(TRANS,M,N,KL,KU,ALPHA,A,LDA,x,INCX,BETA,y,INCY)
   end subroutine

end module
