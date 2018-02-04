module example
     implicit none
contains
    subroutine plus_in_fortran(a,b,aplusb)
        real*8, intent(in) :: a, b
        real*8, intent(out) :: aplusb

        aplusb = a+b
    end subroutine

    subroutine product_in_fortran(a,b,aprodb)
        real*8, intent(in) :: a, b
        real*8, intent(out) :: aprodb

        aprodb = a*b
    end subroutine
end module
           
