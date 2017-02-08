program simple

    use mpi

    implicit none

    integer ierr, my_rank, s

    ! print to screen
 !   call mpi_comm_rank(MPI_COMM_WORLD,my_rank,ierr)
    print *, 'calling program main'

    call mpi_init(ierr)

    call mpi_comm_rank(MPI_COMM_WORLD,my_rank,ierr)
    call mpi_comm_size(MPI_COMM_WORLD,s,ierr)

    write(6,100), my_rank, s

    call mpi_finalize(ierr)

100   format('Hello World! I am rank ', I2, ' of size ', I2)
! print to screen
 print *, 'program main...done.'

 contains
end