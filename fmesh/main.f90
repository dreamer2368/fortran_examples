program main

	use mesh

	implicit none

	integer, parameter :: Ng = 5, N = 10000, N2 = 10
	real, parameter :: Lx = 0.5d0, Ly = 0.5d0
	real :: dx = Lx/Ng, dy = Ly/Ng
	real :: xg(2*Ng+1), yg(2*Ng+1)
	real, dimension(0:2*Ng,0:2*Ng) :: pg, pg_dx, pg_dy
	real :: x(N), y(N), spwt(N)
	real :: xt(N2), y(N2)

	call RANDOM_NUMBER(x)
	x = x-0.5d0
	call RANDOM_NUMBER(y)
	y = y-0.5d0
!	x = 0.44d0
!	y = 0.0d0
!	print *, x,y
	spwt = 1.0/N
	call ptc2pdf_tsc(Ng,dx,dy,N,pg,x,y,spwt)
	call pdfgrad(Ng,dx,dy,pg,pg_dx,pg_dy)

	open(unit=301,file='x.bin',form='unformatted',access='stream')
	write(301) x
	close(301)
	open(unit=302,file='y.bin',form='unformatted',access='stream')
	write(302) y
	close(302)
	open(unit=303,file='pg.bin',form='unformatted',access='stream')
	write(303) pg
	close(303)
	open(unit=303,file='pg_dx.bin',form='unformatted',access='stream')
	write(303) pg_dx
	close(303)
	open(unit=303,file='pg_dy.bin',form='unformatted',access='stream')
	write(303) pg_dy
	close(303)
	open(unit=304,file='output.bin',form='unformatted',access='stream')
	write(304) Ng, N
	close(304)

end program
