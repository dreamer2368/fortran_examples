module ModReadInputs

   use ModLib

   implicit none   

contains

   subroutine readInputsForQGrid(inputsFile,expSample,varSample,T0_eval,YH_eval,eqRat_eval,&
      nT0_eval,nYH_eval,nEqRat_eval,nThreads,nStepsModel,nModelParams,nParams,dtModel,verbModel)

      character(*), intent(in) :: inputsFile
      real, allocatable, intent(out) :: expSample(:)
      real, allocatable, intent(out) :: varSample(:)
      real, allocatable, intent(out) :: T0_eval(:)
      real, allocatable, intent(out) :: YH_eval(:)
      real, allocatable, intent(out) :: eqRat_eval(:)
      integer, intent(out) :: nT0_eval
      integer, intent(out) :: nYH_eval
      integer, intent(out) :: nEqRat_eval
      integer, intent(out) :: nThreads
      integer, intent(out) :: nModelParams
      integer, intent(out) :: nParams
      real, intent(out) :: dtModel
      integer, intent(out) :: nStepsModel
      integer, intent(out) :: verbModel
      real :: dtCantera
      integer :: nStepsCantera
      integer :: verbCantera
      integer :: nT0_calib
      integer :: nYH_calib
      integer :: nEqRat_calib
      integer :: nCalib
      integer :: nT0_fine
      integer :: nYH_fine
      integer :: nEqRat_fine
      integer :: nSamples
      integer :: nBurn
      real :: deltaMax_simple
      integer :: verbMCMC
      
      call readInputsFile(inputsFile,nT0_calib,nYH_calib,nEqRat_calib,nCalib,nT0_eval,nYH_eval,&
         nEqRat_eval,nT0_fine,nYH_fine,nEqRat_fine,nSamples,nBurn,deltaMax_simple,nModelParams,&
         nParams,nThreads,dtCantera,dtModel,nStepsCantera,nStepsModel,verbCantera,verbModel,&
         verbMCMC)

      allocate(expSample(nParams))
      allocate(varSample(nParams))
      allocate(T0_eval(nT0_eval))
      allocate(YH_eval(nYH_eval))
      allocate(eqRat_eval(nEqRat_eval))

      print *, '***READING FROM TEST3...***'
      call readArray(expSample,nParams,'test3/other/expSample.dat')
      call readArray(varSample,nParams,'test3/other/varSample.dat')
      call readArray(T0_eval,nT0_eval,'test3/other/T0_eval.dat')
      call readArray(YH_eval,nYH_eval,'test3/other/YH_eval.dat')
      call readArray(eqRat_eval,nEqRat_eval,'test3/other/eqRat_eval.dat')

   end subroutine

   subroutine readInputsProcessor(inputsFile,nT0_eval,nYH_eval,nEqRat_eval,nSamples,&
      nModelParams,nParams,nThreads,samples,verbModel)

      character(*), intent(in) :: inputsFile
      integer, intent(out) :: nT0_eval
      integer, intent(out) :: nYH_eval
      integer, intent(out) :: nEqRat_eval
      integer, intent(out) :: nSamples
      integer, intent(out) :: nModelParams
      integer, intent(out) :: nParams
      integer, intent(out) :: nThreads
      real, allocatable, intent(out) :: samples(:,:)
      integer, intent(out) :: verbModel
      integer :: nT0_calib
      integer :: nYH_calib
      integer :: nEqRat_calib
      integer :: nCalib
      integer :: nT0_fine
      integer :: nYH_fine
      integer :: nEqRat_fine
      integer :: nBurn
      real :: deltaMax_simple
      real :: dtCantera
      real :: dtModel
      integer :: nStepsCantera
      integer :: nStepsModel
      integer :: verbCantera
      integer :: verbMCMC

      call readInputsFile(inputsFile,nT0_calib,nYH_calib,nEqRat_calib,nCalib,nT0_eval,nYH_eval,&
         nEqRat_eval,nT0_fine,nYH_fine,nEqRat_fine,nSamples,nBurn,deltaMax_simple,nModelParams,&
         nParams,nThreads,dtCantera,dtModel,nStepsCantera,nStepsModel,verbCantera,verbModel,&
         verbMCMC)

      allocate(samples(nParams,nSamples))
 
      call readArray(samples,'samples.dat')

   end subroutine

   subroutine readInputsModel(inputsFile,SDataCalib,T0_calib,YH_calib,eqRat_calib,T0_eval,&
      YH_eval,eqRat_eval,T0_fine,YH_fine,eqRat_fine,nT0_calib,nYH_calib,nEqRat_calib,nCalib,&
      nT0_eval,nYH_eval,nEqRat_eval,nT0_fine,nYH_fine,nEqRat_fine,nSamples,nBurn,&
      deltaMax_simple,deltaMax,nModelParams,nParams,nThreads,nStepsModel,dtModel,sample0,&
      sampleMin,sampleMax,priorCoeffs,verbModel,verbMCMC)

      character(*), intent(in) :: inputsFile
      real, allocatable, intent(out) :: SDataCalib(:,:,:)
      real, allocatable, intent(out) :: T0_calib(:)
      real, allocatable, intent(out) :: YH_calib(:)
      real, allocatable, intent(out) :: eqRat_calib(:)
      real, allocatable, intent(out) :: T0_eval(:)
      real, allocatable, intent(out) :: YH_eval(:)
      real, allocatable, intent(out) :: eqRat_eval(:)
      real, allocatable, intent(out) :: T0_fine(:)
      real, allocatable, intent(out) :: YH_fine(:)
      real, allocatable, intent(out) :: eqRat_fine(:)
      integer, intent(out) :: nT0_calib
      integer, intent(out) :: nYH_calib
      integer, intent(out) :: nEqRat_calib
      integer, intent(out) :: nCalib
      integer, intent(out) :: nT0_eval
      integer, intent(out) :: nYH_eval
      integer, intent(out) :: nEqRat_eval
      integer, intent(out) :: nT0_fine
      integer, intent(out) :: nYH_fine
      integer, intent(out) :: nEqRat_fine
      integer, intent(out) :: nSamples
      integer, intent(out) :: nBurn
      real, intent(out) :: deltaMax_simple
      real, allocatable, intent(out) :: deltaMax(:)
      integer, intent(out) :: nModelParams
      integer, intent(out) :: nParams
      integer, intent(out) :: nThreads
      real, intent(out) :: dtModel
      real, allocatable, intent(out) :: sample0(:)
      real, allocatable, intent(out) :: sampleMin(:)
      real, allocatable, intent(out) :: sampleMax(:)
      integer, intent(out) :: nStepsModel
      real, allocatable, intent(out) :: priorCoeffs(:)
      integer, intent(out) :: verbModel
      integer, intent(out) :: verbMCMC
      real :: dtCantera
      integer :: nStepsCantera
      integer :: verbCantera

      call readInputsFile(inputsFile,nT0_calib,nYH_calib,nEqRat_calib,nCalib,nT0_eval,nYH_eval,&
         nEqRat_eval,nT0_fine,nYH_fine,nEqRat_fine,nSamples,nBurn,deltaMax_simple,nModelParams,&
         nParams,nThreads,dtCantera,dtModel,nStepsCantera,nStepsModel,verbCantera,verbModel,&
         verbMCMC)

      allocate(T0_calib(nT0_calib))
      allocate(YH_calib(nYH_calib))
      allocate(eqRat_calib(nEqRat_calib))
      allocate(T0_eval(nT0_eval))
      allocate(YH_eval(nYH_eval))
      allocate(eqRat_eval(nEqRat_eval))
      allocate(T0_fine(nT0_fine))
      allocate(YH_fine(nYH_fine))
      allocate(eqRat_fine(nEqRat_fine))
      allocate(deltaMax(nParams))
      allocate(sample0(nParams))
      allocate(sampleMin(nParams))
      allocate(sampleMax(nParams))
      allocate(SDataCalib(nT0_calib,nYH_calib,nEqRat_calib))
      allocate(priorCoeffs(3))

      call readArray(T0_calib,'T0_calib.dat')
      call readArray(YH_calib,'YH_calib.dat')
      call readArray(eqRat_calib,'eqRat_calib.dat')
      call readArray(T0_eval,'T0_eval.dat')
      call readArray(YH_eval,'YH_eval.dat')
      call readArray(eqRat_eval,'eqRat_eval.dat')
      call readArray(T0_fine,'T0_fine.dat')
      call readArray(YH_fine,'YH_fine.dat')
      call readArray(eqRat_fine,'eqRat_fine.dat')
      call readArray(deltaMax,'deltaMax.dat')
      call readArray(sample0,'sample0.dat')
      call readArray(sampleMin,'sampleMin.dat')
      call readArray(sampleMax,'sampleMax.dat')
      call readArray(SDataCalib,'SDataCalib.dat')
      call readArray(priorCoeffs,3,'priorCoeffs.txt')

   end subroutine

   subroutine readInputsData(inputsFile,T0_calib,YH_calib,eqRat_calib,T0_eval,YH_eval,&
      eqRat_eval,T0_fine,YH_fine,eqRat_fine,nT0_calib,nYH_calib,nEqRat_calib,nT0_eval,&
      nYH_eval,nEqRat_eval,nT0_fine,nYH_fine,nEqRat_fine,dtCantera,nStepsCantera,verbCantera)

      character(*), intent(in) :: inputsFile
      real, allocatable, intent(out) :: T0_calib(:)
      real, allocatable, intent(out) :: YH_calib(:)
      real, allocatable, intent(out) :: eqRat_calib(:)
      real, allocatable, intent(out) :: T0_eval(:)
      real, allocatable, intent(out) :: YH_eval(:)
      real, allocatable, intent(out) :: eqRat_eval(:)
      real, allocatable, intent(out) :: T0_fine(:)
      real, allocatable, intent(out) :: YH_fine(:)
      real, allocatable, intent(out) :: eqRat_fine(:)
      integer, intent(out) :: nT0_calib
      integer, intent(out) :: nYH_calib
      integer, intent(out) :: nEqRat_calib
      integer, intent(out) :: nT0_eval
      integer, intent(out) :: nYH_eval
      integer, intent(out) :: nEqRat_eval
      integer, intent(out) :: nT0_fine
      integer, intent(out) :: nYH_fine
      integer, intent(out) :: nEqRat_fine
      real, intent(out) :: dtCantera
      integer, intent(out) :: nStepsCantera
      integer, intent(out) :: verbCantera
      real :: deltaMax_simple
      integer :: nCalib
      integer :: nSamples
      integer :: nBurn
      integer :: nModelParams
      integer :: nParams
      integer :: nThreads
      real :: dtModel
      integer :: nStepsModel
      integer :: verbModel
      integer :: verbMCMC

      call readInputsFile(inputsFile,nT0_calib,nYH_calib,nEqRat_calib,nCalib,nT0_eval,nYH_eval,&
         nEqRat_eval,nT0_fine,nYH_fine,nEqRat_fine,nSamples,nBurn,deltaMax_simple,nModelParams,&
         nParams,nThreads,dtCantera,dtModel,nStepsCantera,nStepsModel,verbCantera,verbModel,&
         verbMCMC)

      allocate(T0_calib(nT0_calib))
      allocate(YH_calib(nYH_calib))
      allocate(eqRat_calib(nEqRat_calib))
      allocate(T0_eval(nT0_eval))
      allocate(YH_eval(nYH_eval))
      allocate(eqRat_eval(nEqRat_eval))
      allocate(T0_fine(nT0_fine))
      allocate(YH_fine(nYH_fine))
      allocate(eqRat_fine(nEqRat_fine))

      call readArray(T0_calib,'T0_calib.dat')
      call readArray(YH_calib,'YH_calib.dat')
      call readArray(eqRat_calib,'eqRat_calib.dat')
      call readArray(T0_eval,'T0_eval.dat')
      call readArray(YH_eval,'YH_eval.dat')
      call readArray(eqRat_eval,'eqRat_eval.dat')
      call readArray(T0_fine,'T0_fine.dat')
      call readArray(YH_fine,'YH_fine.dat')
      call readArray(eqRat_fine,'eqRat_fine.dat')

   end subroutine

   subroutine readInputsFile(inputsFile,nT0_calib,nYH_calib,nEqRat_calib,nCalib,nT0_eval,&
      nYH_eval,nEqRat_eval,nT0_fine,nYH_fine,nEqRat_fine,nSamples,nBurn,deltaMax_simple,&
      nModelParams,nParams,nThreads,dtCantera,dtModel,nStepsCantera,nStepsModel,verbCantera,&
      verbModel,verbMCMC)

      character(*), intent(in) :: inputsFile
      integer, intent(out) :: nT0_calib
      integer, intent(out) :: nYH_calib
      integer, intent(out) :: nEqRat_calib
      integer, intent(out) :: nCalib
      integer, intent(out) :: nT0_eval
      integer, intent(out) :: nYH_eval
      integer, intent(out) :: nEqRat_eval
      integer, intent(out) :: nT0_fine
      integer, intent(out) :: nYH_fine
      integer, intent(out) :: nEqRat_fine
      integer, intent(out) :: nSamples
      integer, intent(out) :: nBurn
      real, intent(out) :: deltaMax_simple
      integer, intent(out) :: nModelParams
      integer, intent(out) :: nParams
      integer, intent(out) :: nThreads
      real, intent(out) :: dtCantera
      real, intent(out) :: dtModel
      integer, intent(out) :: nStepsCantera
      integer, intent(out) :: nStepsModel
      integer, intent(out) :: verbCantera
      integer, intent(out) :: verbModel
      integer, intent(out) :: verbMCMC
      
      namelist /inputs/ nT0_calib,nYH_calib,nEqRat_calib,nCalib,nT0_eval,nYH_eval,nEqRat_eval,&
         nT0_fine,nYH_fine,nEqRat_fine,nSamples,nBurn,deltaMax_simple,nModelParams,nParams,&
         nThreads,dtCantera,dtModel,nStepsCantera,nStepsModel,verbCantera,verbModel,verbMCMC 

      open(unit=1,file=inputsFile,status='old',action='read')
      read(1,nml=inputs)
      close(1)

      print *, 'File Read: ', inputsFile

   end subroutine

end module
