module example_wrapper

    use iso_c_binding, only: c_double
    use example

    implicit none

contains

    subroutine c_plus_in_fortran(a,b,aplusb) bind(c)
        real(c_double), intent(in) :: a,b
        real(c_double), intent(out) :: aplusb
        call plus_in_fortran(a,b,aplusb)
    end subroutine

    subroutine c_product_in_fortran(a,b,aprodb) bind(c)
        real(c_double), intent(in) :: a,b
        real(c_double), intent(out) :: aprodb
        call product_in_fortran(a,b,aprodb)
    end subroutine

end module
