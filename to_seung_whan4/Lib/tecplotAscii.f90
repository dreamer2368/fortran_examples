subroutine tecplotAscii1d_opt0(data,filename,title,varNames)

	real, intent(in) :: data(:,:)
	character(*), intent(in) :: filename
	character(*), intent(in) :: title
	character(*), intent(in) :: varNames
	integer :: N
	integer :: i

	N = size(data,1)

	open(unit=1,file=filename,access='append',status='replace')

	write(1,'(a,a)') 'TITLE=',title
	write(1,'(a,a)') 'VARIABLES=',varNames
	write(1,'(a,i8,a)') 'ZONE I=',N,', F=POINT'

	do i = 1,N
		write(1,*) data(i,:)
	end do

	close(1)

end subroutine

subroutine tecplotAscii1d_opt1(data,filename,title,varNames,stat)

	real, intent(in) :: data(:,:)
	character(*), intent(in) :: filename
	character(*), intent(in) :: title
	character(*), intent(in) :: varNames
   character(*), intent(in) :: stat
	integer :: N
	integer :: i

	N = size(data,1)

	open(unit=1,file=filename,access='append',status=stat)

	write(1,'(a,a)') 'TITLE=',title
	write(1,'(a,a)') 'VARIABLES=',varNames
	write(1,'(a,i8,a)') 'ZONE I=',N,', F=POINT'

	do i = 1,N
		write(1,*) data(i,:)
	end do

	close(1)

   if (stat == 'old') then
      print *, 'File Appended: ', filename
   elseif (stat == 'replace') then
      print *, 'File Written: ', filename
   else
      print *, 'ERROR in tecplotAscii: option unknown'
   end if

end subroutine

subroutine tecplotAscii1d_opt2(data,filename,title,zone,varNames,stat)

	real, intent(in) :: data(:,:)
	character(*), intent(in) :: filename
	character(*), intent(in) :: title
	character(*), intent(in) :: zone
	character(*), intent(in) :: varNames
   character(*), intent(in) :: stat
	integer :: N
	integer :: i

	N = size(data,1)

	open(unit=1,file=filename,access='append',status=stat)

	write(1,'(a,a)') 'TITLE=',title
	write(1,'(a,a)') 'VARIABLES=',varNames
	write(1,'(a,a,a,i8,a)') 'ZONE T=',zone,', I=',N,', F=POINT'

	do i = 1,N
		write(1,*) data(i,:)
	end do

	close(1)

   if (stat == 'old') then
      print *, 'File Appended: ', filename
   elseif (stat == 'replace') then
      print *, 'File Written: ', filename
   else
      print *, 'ERROR in tecplotAscii: option unknown'
   end if

end subroutine

subroutine tecplotAscii2d(data, filename, title, varNames)

	double precision, intent(in) :: data(:,:,:)
	character(*), intent(in) :: filename
	character(*), intent(in) :: title
	character(*), intent(in) :: varNames
	integer :: Nx
	integer :: Ny
	integer :: i
	integer :: j

	Nx = size(data,1)
	Ny = size(data,2)

	open(unit=1,file=filename,action='write',status='replace')

	write(1,'(a,a)') 'TITLE=',title
	write(1, '(a,a)') 'VARIABLES =',varNames
	write(1, '(a,i9,a,i9,a)') 'ZONE T="BIG ZONE", I=', Nx, ', J=', Ny, ', DATAPACKING=POINT'

	do i = 1,Nx
		do j = 1,Ny
			write(1,*) data(i,j,:)
		end do
	end do

	close(1)
	print *, 'File Written: ', filename

end subroutine

