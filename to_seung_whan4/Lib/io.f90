
!!!!! INTEGERS !!!!!
subroutine printIntegerToScrn1d(array)
	integer, intent(in) :: array(:)
	integer :: i
	do i = 1,size(array)
		write(*,*) array(i)
	end do
end subroutine

subroutine printIntegerToUnit1d(array,un)
	integer, intent(in) :: array(:)
	integer, intent(in) :: un
	integer :: i
	do i = 1,size(array)
		write(un,*) array(i)
	end do
end subroutine

subroutine printIntegerToFile1d(array,filename)
	integer, intent(in) :: array(:)
	character(*), intent(in) :: filename
	integer :: i
	open(unit=1,file=filename,action='write',status='replace')
	do i = 1,size(array)
		write(1,*) array(i)
	end do
	close(1)
	print *, 'File Written: ', filename
end subroutine

subroutine printIntegerToScrn2d(array)
	integer, intent(in) :: array(:,:)
	integer :: i
	do i = 1,size(array,1)
		write(*,*) array(i,:)
	end do
end subroutine

subroutine printIntegerToUnit2d(array,un)
	integer, intent(in) :: array(:,:)
	integer, intent(in) :: un
	integer :: i
	do i = 1,size(array,1)
		write(un,*) array(i,:)
	end do
end subroutine

subroutine printIntegerToFile2d(array,filename)
	integer, intent(in) :: array(:,:)
	character(*), intent(in) :: filename
	integer :: i
	open(unit=1,file=filename,action='write',status='replace')
	do i = 1,size(array,1)
		write(1,*) array(i,:)
	end do
	close(1)
	print *, 'File Written: ', filename
end subroutine

subroutine printIntegerToScrn3d(array)
	integer, intent(in) :: array(:,:,:)
	integer :: i,k
	do k = 1,size(array,3)
		do i = 1,size(array,1)
			write(*,*) array(i,:,k)
		end do
		write(*,*) ''
	end do
end subroutine

subroutine printIntegerToUnit3d(array,un)
	integer, intent(in) :: array(:,:,:)
	integer, intent(in) :: un
	integer :: i,k
	do k = 1,size(array,3)
		do i = 1,size(array,1)
			write(un,*) array(i,:,k)
		end do
		write(un,*) ''
	end do
end subroutine

subroutine printIntegerToFile3d(array,filename)
	integer, intent(in) :: array(:,:,:)
	character(*), intent(in) :: filename
	integer :: i,k
	open(unit=1,file=filename,action='write',status='replace')
	do k = 1,size(array,3)
		do i = 1,size(array,1)
			write(1,*) array(i,:,k)
		end do
		write(1,*) ''
	end do
	close(1)
	print *, 'File Written: ', filename
end subroutine

!!!!! REALS !!!!!

subroutine printRealToScrn1d(array)
	real, intent(in) :: array(:)
	integer :: i
	do i = 1,size(array)
		write(*,*) array(i)
	end do
end subroutine

subroutine printRealToUnit1d(array,un)
	real, intent(in) :: array(:)
	integer, intent(in) :: un
	integer :: i
	do i = 1,size(array)
		write(un,*) array(i)
	end do
end subroutine

subroutine printRealToFile1d(array,filename)
	real, intent(in) :: array(:)
	character(*), intent(in) :: filename
	integer :: i
	open(unit=1,file=filename,action='write',status='replace')
	do i = 1,size(array)
		write(1,*) array(i)
	end do
	close(1)
	print *, 'File Written: ', filename
end subroutine

subroutine printRealToScrn2d(array)
	real, intent(in) :: array(:,:)
	integer :: i
	do i = 1,size(array,1)
		write(*,*) array(i,:)
	end do
end subroutine

subroutine printRealToUnit2d(array,un)
	real, intent(in) :: array(:,:)
	integer, intent(in) :: un
	integer :: i
	do i = 1,size(array,1)
		write(un,*) array(i,:)
	end do
end subroutine

subroutine printRealToFile2d(array,filename)
	real, intent(in) :: array(:,:)
	character(*), intent(in) :: filename
	integer :: i
	open(unit=1,file=filename,action='write',status='replace')
	do i = 1,size(array,1)
		write(1,*) array(i,:)
	end do
	close(1)
	print *, 'File Written: ', filename
end subroutine

subroutine printRealToScrn3d(array)
	real, intent(in) :: array(:,:,:)
	integer :: i,k
	do k = 1,size(array,3)
		do i = 1,size(array,1)
			write(*,*) array(i,:,k)
		end do
		write(*,*) ''
	end do
end subroutine

subroutine printRealToUnit3d(array,un)
	real, intent(in) :: array(:,:,:)
	integer, intent(in) :: un
	integer :: i,k
	do k = 1,size(array,3)
		do i = 1,size(array,1)
			write(un,*) array(i,:,k)
		end do
		write(un,*) ''
	end do
end subroutine

subroutine printRealToFile3d(array,filename)
	real, intent(in) :: array(:,:,:)
	character(*), intent(in) :: filename
	integer :: i,k
	open(unit=1,file=filename,action='write',status='replace')
	do k = 1,size(array,3)
		do i = 1,size(array,1)
			write(1,*) array(i,:,k)
		end do
		write(1,*) ''
	end do
	close(1)
	print *, 'File Written: ', filename
end subroutine

!!!!! DOUBLES !!!!!

subroutine printDoubleToScrn1d(array)
	double precision, intent(in) :: array(:)
	integer :: i
	do i = 1,size(array)
		write(*,*) array(i)
	end do
end subroutine

subroutine printDoubleToUnit1d(array,un)
	double precision, intent(in) :: array(:)
	integer, intent(in) :: un
	integer :: i
	do i = 1,size(array)
		write(un,*) array(i)
	end do
end subroutine

subroutine printDoubleToFile1d(array,filename)
	double precision, intent(in) :: array(:)
	character(*), intent(in) :: filename
	integer :: i
	open(unit=1,file=filename,action='write',status='replace')
	do i = 1,size(array)
		write(1,*) array(i)
	end do
	close(1)
	print *, 'File Written: ', filename
end subroutine

subroutine printDoubleToScrn2d(array)
	double precision, intent(in) :: array(:,:)
	integer :: i
	do i = 1,size(array,1)
		write(*,*) array(i,:)
	end do
end subroutine

subroutine printDoubleToUnit2d(array,un)
	double precision, intent(in) :: array(:,:)
	integer, intent(in) :: un
	integer :: i
	do i = 1,size(array,1)
		write(un,*) array(i,:)
	end do
end subroutine

subroutine printDoubleToFile2d(array,filename)
	double precision, intent(in) :: array(:,:)
	character(*), intent(in) :: filename
	integer :: i
	open(unit=1,file=filename,action='write',status='replace')
	do i = 1,size(array,1)
		write(1,*) array(i,:)
	end do
	close(1)
	print *, 'File Written: ', filename
end subroutine

subroutine printDoubleToScrn3d(array)
	double precision, intent(in) :: array(:,:,:)
	integer :: i,k
	do k = 1,size(array,3)
		do i = 1,size(array,1)
			write(*,*) array(i,:,k)
		end do
		write(*,*) ''
	end do
end subroutine

subroutine printDoubleToUnit3d(array,un)
	double precision, intent(in) :: array(:,:,:)
	integer, intent(in) :: un
	integer :: i,k
	do k = 1,size(array,3)
		do i = 1,size(array,1)
			write(un,*) array(i,:,k)
		end do
		write(un,*) ''
	end do
end subroutine

subroutine printDoubleToFile3d(array,filename)
	double precision, intent(in) :: array(:,:,:)
	character(*), intent(in) :: filename
	integer :: i,k
	open(unit=1,file=filename,action='write',status='replace')
	do k = 1,size(array,3)
		do i = 1,size(array,1)
			write(1,*) array(i,:,k)
		end do
		write(1,*) ''
	end do
	close(1)
	print *, 'File Written: ', filename
end subroutine

!!!!! LOGICALS !!!!!

subroutine printLogicalToScrn1d(array)
	logical, intent(in) :: array(:)
	integer :: i
	do i = 1,size(array)
		write(*,*) array(i)
	end do
end subroutine

subroutine printLogicalToUnit1d(array,un)
	logical, intent(in) :: array(:)
	integer, intent(in) :: un
	integer :: i
	do i = 1,size(array)
		write(un,*) array(i)
	end do
end subroutine

subroutine printLogicalToFile1d(array,filename)
	logical, intent(in) :: array(:)
	character(*), intent(in) :: filename
	integer :: i
	open(unit=1,file=filename,action='write',status='replace')
	do i = 1,size(array)
		write(1,*) array(i)
	end do
	close(1)
	print *, 'File Written: ', filename
end subroutine

subroutine printLogicalToScrn2d(array)
	logical, intent(in) :: array(:,:)
	integer :: i
	do i = 1,size(array,1)
		write(*,*) array(i,:)
	end do
end subroutine

subroutine printLogicalToUnit2d(array,un)
	logical, intent(in) :: array(:,:)
	integer, intent(in) :: un
	integer :: i
	do i = 1,size(array,1)
		write(un,*) array(i,:)
	end do
end subroutine

subroutine printLogicalToFile2d(array,filename)
	logical, intent(in) :: array(:,:)
	character(*), intent(in) :: filename
	integer :: i
	open(unit=1,file=filename,action='write',status='replace')
	do i = 1,size(array,1)
		write(1,*) array(i,:)
	end do
	close(1)
	print *, 'File Written: ', filename
end subroutine

subroutine printLogicalToScrn3d(array)
	logical, intent(in) :: array(:,:,:)
	integer :: i,k
	do k = 1,size(array,3)
		do i = 1,size(array,1)
			write(*,*) array(i,:,k)
		end do
		write(*,*) ''
	end do
end subroutine

subroutine printLogicalToUnit3d(array,un)
	logical, intent(in) :: array(:,:,:)
	integer, intent(in) :: un
	integer :: i,k
	do k = 1,size(array,3)
		do i = 1,size(array,1)
			write(un,*) array(i,:,k)
		end do
		write(un,*) ''
	end do
end subroutine

subroutine printLogicalToFile3d(array,filename)
	logical, intent(in) :: array(:,:,:)
	character(*), intent(in) :: filename
	integer :: i,k
	open(unit=1,file=filename,action='write',status='replace')
	do k = 1,size(array,3)
		do i = 1,size(array,1)
			write(1,*) array(i,:,k)
		end do
		write(1,*) ''
	end do
	close(1)
	print *, 'File Written: ', filename
end subroutine

subroutine readRealArray1dFromFile(array,dm,filename)
   integer, intent(in) :: dm
   character(*), intent(in) :: filename
   real, intent(out) :: array(:)
   open(unit=1,file=filename,status='old')
   read(1,*) array
   close(1)
   print *, 'File Read: ', filename
end subroutine
 
subroutine readRealArray2dFromFile(array,dm,filename)
   integer, intent(in) :: dm(2)
   character(*), intent(in) :: filename
   real, intent(out) :: array(:,:)
   real :: temp(dm(2),dm(1))
   open(unit=1,file=filename,status='old')
   read(1,*) temp
   close(1)
   array = transpose(temp)
   print *, 'File Read: ', filename
end subroutine
 
subroutine readRealArray3dFromFile(array,dm,filename)
   integer, intent(in) :: dm(3)
   character(*), intent(in) :: filename
   real, intent(out) :: array(:,:,:)
   real :: vec(product(dm))
   real :: temp(dm(2),dm(1),dm(3))
   integer :: k
   open(unit=1,file=filename,status='old')
   read(1,*) vec
   close(1)
   temp = reshape(vec,(/dm(2),dm(1),dm(3)/))
   do k = 1,dm(3)
      array(:,:,k) = transpose(temp(:,:,k))
   end do
   print *, 'File Read: ', filename
end subroutine
