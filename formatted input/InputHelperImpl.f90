module InputHelperImpl

	implicit none
    public

    integer, parameter :: STRING_LENGTH = 256, mp = SELECTED_REAL_KIND(15,307)

    type, private :: t_DictElement
        character(len = STRING_LENGTH) :: key, val
    end type t_DictElement
    type(t_DictElement), allocatable, public :: dict(:)

contains

    subroutine split(str,left,right,separator)
        character(len=*), intent(inout) :: str
        character(len=*), intent(out) :: left, right
        character(len=1), intent(in) :: separator
        integer :: i, j
        i=1
        do j=1,len_trim(str)
            if ( str(j:j)==separator ) exit
            left(i:i) = str(j:j)
            i = i+1
        end do

        left(i:) = " "
        right = str(j+1:len_trim(str))
        call trimAll(left)
        call trimAll(right)
    end subroutine

    subroutine trimAll(str)
        ! <<< Arguments >>>
        character(len = *), intent(inout) :: str

        ! <<< Local variables >>>
        integer :: i

        do i = 1, len(str)
            if (ichar(str(i:i)) > 32) exit
            if (ichar(str(i:i)) <= 32) str(i:i) = " "
        end do

        do i = len(str), 1, -1
            if (ichar(str(i:i)) > 32) exit
            if (ichar(str(i:i)) <= 32) str(i:i) = " "
        end do

        str = trim(adjustl(str))
    end subroutine trimAll

    subroutine sort()
        ! <<< Local variables >>>
        integer :: i, j
        type(t_DictElement) :: temp

        do i = 2, size(dict)
            j = i - 1
            temp = dict(i)
            do while (dict(j)%key > temp%key)
                dict(j+1) = dict(j)
                j = j - 1
                if (j < 1) exit
            end do
            dict(j+1) = temp
        end do
    end subroutine sort

    subroutine find(key, index)
        ! <<< Arguments >>>
        character(len = *), intent(in) :: key
        integer, intent(out) :: index

        ! <<< Local variables >>>
        integer :: iStart, iEnd, iMid

        index = -1

        if (allocated(dict)) then

            iStart = 1
            iEnd = size(dict)

            do while (iEnd >= iStart) !... `dict` is sorted; use binary search.
                iMid = (iStart + iEnd) / 2
                if (dict(iMid)%key == trim(key)) then
                    index = iMid
                    return
                end if
                if (dict(iMid)%key > trim(key)) then
                    iEnd = iMid - 1
                else
                    iStart = iMid + 1
                end if
            end do

        end if
    end subroutine find

    subroutine parseInputFile(filename, commentor, separator)
!        ! <<< Private members >>>
!        use InputHelperImpl, only : dict, split, sort!, skip_comment, getFreeUnit
!
!        implicit none

        ! <<< Arguments >>>
        character(len = STRING_LENGTH), intent(in) :: filename
        character(len=1), intent(in) :: commentor, separator
    !        type(t_DictElement), allocatable, intent(out) :: dict(:)

        ! <<< Local variables >>>
        character(len = STRING_LENGTH) :: line, message
        integer :: i,dictSize, fileUnit, istat, lineNo

        open(unit=getFreeUnit(fileUnit),file=trim(filename),status='old')

        dictSize = 0
        do !... read once to find input dictionary size.
            read(fileUnit, '(A)', iostat = istat) line
            if (istat < 0) exit
            call skip_comment(line, '#') !... skip comments.
            if (len_trim(line) == 0) cycle !... skip empty lines.
            dictSize = dictSize + 1
        end do
        close(fileUnit)
        print *, dictSize

        allocate(dict(dictSize))

        i = 0 ; lineNo = 0 ; istat = 0
        open(unit = getFreeUnit(fileUnit), file = trim(filename), action = 'read', status = 'old')
        do !... read again to fill input dictionary.
            read(fileUnit, '(A)', iostat = istat) line
            if (istat < 0) then
                istat = 0
                exit
            end if
            lineNo = lineNo + 1 !... need line number for reporting errors.
            call skip_comment(line, '#')
            if (len_trim(line) == 0) cycle
            i = i + 1

            ! Parse in 'key <separator> value' format.
            call split(line, dict(i)%key, dict(i)%val, '=')

            if (istat /= 0) then
                write(message, "(2A,I0.0,A)") trim(filename), ": Failed to parse input on line ", &
                lineNo, "!"
                exit
            end if

            if (len_trim(dict(i)%key) == 0) then
                istat = 1
                write(message, "(2A,I0.0,A)") trim(filename), ": Empty parameter key on line ",   &
                lineNo, "!"
                exit
            end if
        end do
        close(fileUnit)
    end subroutine

    subroutine skip_comment(str,commentor)
        character(len=*), intent(inout) :: str
        character(len=1), intent(in) :: commentor
        integer :: i,j

        call trimAll(str)

        do i=1,len(str)
            if ( str(i:i)==commentor ) then
                str(i:) = " "
            end if
        end do
    end subroutine

    function getFreeUnit(fileUnit) result(freeUnit)
        ! <<< Arguments >>>
        integer, intent(out), optional :: fileUnit

        ! <<< Result >>>
        integer :: freeUnit

        ! <<< Local variables >>>
        logical :: isOpened

        do freeUnit = 10, 10000
            inquire(unit = freeUnit, opened = isOpened)
            if (.not. isOpened) exit
        end do

        if (present(fileUnit)) fileUnit = freeUnit
    end function getFreeUnit

    function getOptionInteger_(key, defaultValue) result(val)

!        ! <<< Private members >>>
!        use InputHelperImpl, only : dict, find
!
!        implicit none

        ! <<< Arguments >>>
        character(len = *), intent(in) :: key
        integer, intent(in) :: defaultValue

        ! <<< Result >>>
        integer :: val

        ! <<< Local variables >>>
        integer :: index, stat

        call find(key, index)

        if (index == -1) then
            val = defaultValue
            return
        end if

        read(dict(index)%val, *, iostat = stat) val
        if (stat /= 0) val = defaultValue

    end function getOptionInteger_

    function getOptionLogical_(key, defaultValue) result(val)

!        ! <<< Private members >>>
!        use InputHelperImpl, only : dict, find
!
!        implicit none

        ! <<< Arguments >>>
        character(len = *), intent(in) :: key
        logical, intent(in) :: defaultValue

        ! <<< Result >>>
        logical :: val

        ! <<< Local variables >>>
        integer :: index

        call find(key, index)

        if (index == -1) then
            val = defaultValue
            return
        end if

        if (trim(dict(index)%val) == "true" .or. trim(dict(index)%val) == "TRUE" .or.              &
            trim(dict(index)%val) == "True") then
            val = .true.
        else if (trim(dict(index)%val) == "false" .or. trim(dict(index)%val) == "FALSE" .or.       &
            trim(dict(index)%val) == "False") then
            val = .false.
        else
            val = defaultValue
        end if

    end function getOptionLogical_

    function getOptionReal_(key, defaultValue) result(val)

!        ! <<< Private members >>>
!        use InputHelperImpl, only : dict, find, mp
!
!        implicit none

        ! <<< Arguments >>>
        character(len = *), intent(in) :: key
        real(mp), intent(in) :: defaultValue

        ! <<< Result >>>
        real(mp) :: val

        ! <<< Local variables >>>
        integer :: index, stat

        call find(key, index)

        if (index == -1) then
            val = defaultValue
            return
        end if

        read(dict(index)%val, *, iostat = stat) val
        if (stat /= 0) val = defaultValue

    end function getOptionReal_

    function getOptionString_(key, defaultValue) result(val)

!        ! <<< Private members >>>
!        use InputHelperImpl, only : dict, find
!
!        implicit none

        ! <<< Arguments >>>
        character(len = *), intent(in) :: key
        character(len = *), intent(in) :: defaultValue

        ! <<< Result >>>
        character(len = STRING_LENGTH) :: val

        ! <<< Local variables >>>
        integer :: index

        call find(key, index)

        if (index == -1) then
            if (len_trim(defaultValue) == 0) then
                val = ""
            else
                read(defaultValue, '(A)') val
            end if
            return
        end if

        read(dict(index)%val, *) val

    end function getOptionString_

end module