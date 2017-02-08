        SUBROUTINE print( x )
	 USE myStructModule

         TYPE(myStruct) :: x

         print *, "myStruct x = ", x
        END SUBROUTINE

!========================================================

	PROGRAM main
	 USE myStructModule

	 INTERFACE
	  SUBROUTINE print( x )
	   USE myStructModule
	   TYPE(myStruct) :: x
          END SUBROUTINE
	 END INTERFACE


	 TYPE(myStruct) :: A

	 A%i = 4; 
	 A%f =3.14

	 CALL print(A)

	END PROGRAM
