module DT_mod
	implicit none
	private
	public :: DT
	type DT
		integer :: i
	end type
	contains
end module

module subs_mod
	use DT_mod
	implicit none
	private
	public :: add,mult,op_int

	abstract interface
		subroutine op_int(d,x,y,z)
			import :: DT
			implicit none
			type(DT),intent(inout) :: d
			integer,intent(inout) :: x
			integer,intent(in) :: y,z
		end subroutine
	end interface

	contains
		subroutine add(d,x,y,z)
			implicit none
			type(DT),intent(inout) :: d
			integer,intent(inout) :: x
			integer,intent(in) :: y,z
			x = y+z
			d%i = 1
		end subroutine
		subroutine mult(d,x,y,z)
			implicit none
			type(DT),intent(inout) :: d
			integer,intent(inout) :: x
			integer,intent(in) :: y,z
			x = y*z
			d%i = 2
		end subroutine
end module

module type_A_mod
	use DT_mod
	use subs_mod
	implicit none
	private
	public :: type_A,init,operate
	type type_A
		procedure(op_int),pointer,nopass :: op
	end type
contains
	subroutine init(A,op)
		implicit none
		procedure(op_int) :: op
		type(type_A),intent(inout) :: A
		A%op => op
	end subroutine
	subroutine operate(A,d,x,y,z)
		implicit none
		type(DT),intent(inout) :: d
		type(type_A),intent(in) :: A
		integer,intent(inout) :: x
		integer,intent(in) :: y,z
		call A%op(d,x,y,z)
	end subroutine
end module

program test
	use type_A_mod
	use subs_mod
	use DT_mod
	implicit none
	type(type_A) :: A
	type(DT) :: d
	integer :: x,y,z
	y = 3; z = 5
	call init(A,mult)
	call operate(A,d,x,y,z)
	write(*,*) 'x,y,x = ',y,z,x
	write(*,*) 'd%i = ',d%i
end program
