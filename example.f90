module example
  2     implicit none
  3 contains
  4     subroutine example1(a,b,aplusb)
  5         real*8, intent(in) :: a, b
  6         real*8, intent(out) :: aplusb
  7 
  8         aplusb = a+b
  9     end subroutine
 10 
 11     subroutine example2(a,b,aprodb)
 12         real*8, intent(in) :: a, b
 13         real*8, intent(out) :: aprodb
 14 
 15         aprodb = a*b
 16     end subroutine
 17 end module
~              
