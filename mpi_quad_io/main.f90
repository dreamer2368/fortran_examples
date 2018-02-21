program simple

    use mpi

    implicit none

    integer, parameter :: dp = SELECTED_REAL_KIND(15,307),       &
                          qp = SELECTED_REAL_KIND(33,4931)
    integer :: ierr, my_rank, s
    integer :: thefile, disp
    real(dp) :: temp_dp(2)
    real(qp) :: temp_qp(2)
    character(len=100) :: filename

    ! print to screen
 !   call mpi_comm_rank(MPI_COMM_WORLD,my_rank,ierr)
    print *, 'calling program main'

    call mpi_init(ierr)

    call mpi_comm_rank(MPI_COMM_WORLD,my_rank,ierr)
    call mpi_comm_size(MPI_COMM_WORLD,s,ierr)

    write(6,100), my_rank, s

    filename = "test_dp.dat"
    call MPI_FILE_OPEN(MPI_COMM_WORLD, trim(filename), &
                        MPI_MODE_WRONLY  &
                         + MPI_MODE_CREATE, &
                        MPI_INFO_NULL, thefile, ierr)
    disp = my_rank*8*2
    temp_dp = (/ 0.0_dp, 1.0_dp /)
    temp_dp = 1.7_dp*( my_rank + temp_dp )
    call MPI_FILE_SET_VIEW(thefile,disp,MPI_DOUBLE,MPI_DOUBLE,   &
                            'native',MPI_INFO_NULL,ierr)
    call MPI_FILE_WRITE(thefile, temp_dp, 2, MPI_DOUBLE, & 
                        MPI_STATUS_IGNORE, ierr)
    call MPI_FILE_SYNC(thefile,ierr)
    call MPI_FILE_CLOSE(thefile, ierr) 

    call mpi_finalize(ierr)

100   format('Hello World! I am rank ', I2, ' of size ', I2)
! print to screen
 print *, 'program main...done.'

 contains
end
