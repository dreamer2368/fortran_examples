module ModLib

	! mean.f90
	interface mean
		procedure meanInteger1d
		procedure meanInteger2d
		procedure meanInteger3d
		procedure meanReal1d
		procedure meanReal2d
		procedure meanReal3d
!		procedure meanDouble1d
!		procedure meanDouble2d
!		procedure meanDouble3d
	end interface

	! printArray.f90
	interface printArray
		procedure printIntegerToScrn1d
		procedure printIntegerToScrn2d
		procedure printIntegerToScrn3d
		procedure printIntegerToUnit1d
		procedure printIntegerToUnit2d
		procedure printIntegerToUnit3d
		procedure printIntegerToFile1d
		procedure printIntegerToFile2d
		procedure printIntegerToFile3d
		procedure printRealToScrn1d
		procedure printRealToScrn2d
		procedure printRealToScrn3d
		procedure printRealToUnit1d
		procedure printRealToUnit2d
		procedure printRealToUnit3d
		procedure printRealToFile1d
		procedure printRealToFile2d
		procedure printRealToFile3d
!		procedure printDoubleToScrn1d
!		procedure printDoubleToScrn2d
!		procedure printDoubleToScrn3d
!		procedure printDoubleToUnit1d
!		procedure printDoubleToUnit2d
!		procedure printDoubleToUnit3d
!		procedure printDoubleToFile1d
!		procedure printDoubleToFile2d
!		procedure printDoubleToFile3d
		procedure printLogicalToScrn1d
		procedure printLogicalToScrn2d
		procedure printLogicalToScrn3d
		procedure printLogicalToUnit1d
		procedure printLogicalToUnit2d
		procedure printLogicalToUnit3d
		procedure printLogicalToFile1d
		procedure printLogicalToFile2d
		procedure printLogicalToFile3d
	end interface

   ! io.f90
   interface readArray
      procedure readRealArray1dFromBinary
      procedure readRealArray2dFromBinary
      procedure readRealArray3dFromBinary
      procedure readRealArray1dFromAscii
      procedure readRealArray2dFromAscii
      procedure readRealArray3dFromAscii
   end interface

	! tecplotAscii.f90
	interface tecplotAscii
		procedure tecplotAscii1d_opt0
		procedure tecplotAscii1d_opt1
		procedure tecplotAscii1d_opt2
		procedure tecplotAscii2d
	end interface

	! cat.f90
	interface cat
		procedure cat_vec
		procedure cat_arr
		procedure cat_mixed
	end interface

	! quadrature.f90
	interface simpQuad
		procedure simpQuad1d
		procedure simpQuad2d
		procedure simpQuad3d
	end interface
	interface trapQuad
		procedure trapQuad1d
		procedure trapQuad2d
		procedure trapQuad3d
	end interface

	! mesh.f90
	interface meshgrid
		procedure meshgrid2d
		procedure meshgrid3d
	end interface

contains

	include 'Lib/mean.f90'
	include 'Lib/io.f90'
	include 'Lib/tecplotAscii.f90'
	include 'Lib/cat.f90'
	include 'Lib/quadrature.f90'
	include 'Lib/mesh.f90'
	include 'Lib/misc.f90'

end module
