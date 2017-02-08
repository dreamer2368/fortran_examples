program main

    use InputHelperImpl

	implicit none

	integer :: i
    real(mp) :: k
    logical :: bool
    character(len = 256) :: line, left,right,filename, message

	! print to screen
	print *, 'calling program main'

    filename = 'example.inp'

    call parseInputFile(filename,'#','=')

!    do i=1,size(dict)
!        print *, trim(dict(i)%key),'=',trim(dict(i)%val)
!    end do

    call sort()

    do i=1,size(dict)
        print *, trim(dict(i)%key),' : ',trim(dict(i)%val)
    end do

    call find('Real_test',i)
    print *, i, trim(dict(i)%key), trim(dict(i)%val)

    i = getOptionInteger_('Integer_test',1208)
    print *, i

    bool = getOptionLogical_('Boolean_test',.false.)
    print *, bool

    k = getOptionReal_('Real_test',12.58_mp)
    print *, k

    message = getOptionString_('String_test','Good morning')
    print *, trim(message)

	! print to screen
	print *, 'program main...done.'

contains


end program
