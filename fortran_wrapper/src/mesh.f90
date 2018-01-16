module mesh

	implicit none

!	integer, parameter :: dp = selected_real_kind(15, 307)

contains

	subroutine ptc2pdf(Ng,dx,dy,N,pg,x,y,spwt)
		integer, intent(in) :: Ng,N
		real*8, intent(in) :: dx, dy, x(N), y(N)
		real*8, intent(in) :: spwt(N)
		integer :: i,g(2,2),xg,yg
		real*8 :: f(2,2)
		real*8, dimension(0:2*Ng,0:2*Ng), intent(out) :: pg

		pg = 0.0d0
		do i=1,N
			xg = FLOOR(x(i)/dx)
			yg = FLOOR(y(i)/dy)
			g(:,1) = (/xg,xg+1/) + Ng
			g(:,2) = (/yg,yg+1/) + Ng
			if( (minval(g)<0).or.(maxval(g)>2*Ng) ) then
				cycle
			end if
			f = spwt(i)/dx/dy
			f(1,:) = f(1,:)*( 1.0d0 - (x(i)/dx-xg) )
			f(2,:) = f(2,:)*(x(i)/dx-xg)
			f(:,1) = f(:,1)*( 1.0d0 - (y(i)/dy-yg) )
			f(:,2) = f(:,2)*(y(i)/dy-yg)
			pg(g(:,1),g(:,2)) = pg(g(:,1),g(:,2)) + f
		end do
	end subroutine

end module