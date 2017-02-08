module ModConstants

   implicit none

   ! Conversions
   real, parameter :: cal_to_joule = 4.184
   real, parameter :: kcal_to_joule = 4.184e3
   real, parameter :: joule_to_cal = 1.0/cal_to_joule
   real, parameter :: joule_to_kcal = 1.0/kcal_to_joule
   real, parameter :: mol_to_kmol = 1e-3
   real, parameter :: kmol_to_mol = 1.0/mol_to_kmol

   ! Indices
   integer, parameter :: iH2 = 1
   integer, parameter :: iO2 = 2
   integer, parameter :: iH = 3
   integer, parameter :: iH2O = 4
   integer, parameter :: iN2 = 5
   integer, parameter :: iA3 = 1
   integer, parameter :: iE3 = 2
   integer, parameter :: ib3 = 3
   integer, parameter :: isigma_S = 4
   
   ! Gas properties, ordered as H2, O2, H, H2O, N2
   integer, parameter :: nSpec = 5
   integer, parameter :: nReact = 3
   real, parameter :: Ru = 8314.0 ! J/(kmol*K)
   real, private, parameter :: hoRaw(nSpec) = (/0.0, 0.0, 52.098, -57.77, 0.0/) ! kcal/(mol*K)
!   real, private, parameter :: cpRaw(nSpec) = (/7.2, 8.3, 4.96, 9.87, 8.28/) ! cal/(mol*K)
   real, private, parameter :: cpRaw(nSpec) = (/7.890,8.825,4.968,11.525,8.374/) ! cal/(mol*K)
   real, parameter :: MWi(nSpec) = (/2.0, 32.0, 1.0, 18.0, 28.0/) ! kg/kmol
   real, parameter :: Ri(nSpec) = Ru/MWi ! J/(kg*K)
   real, parameter :: cpi(nSpec) = (cpRaw*cal_to_joule/mol_to_kmol)/MWi ! J/(kg*K)
   real, parameter :: cvi(nSpec) = cpi-Ri ! J/(kg*K)
   real, parameter :: hoi(nSpec) = (hoRaw*kcal_to_joule/mol_to_kmol)/MWi ! J/(kg*K)

   ! Other
   real, parameter :: stoichO2 = MWi(iO2)/(2*MWi(iH2))
   real, parameter :: stoichH2O = (2*MWi(iH2O))/(2*MWi(iH2))
   real, parameter :: atmospheric = 101325 ! Pa
   real, parameter :: N2_O2_airMoleRatio = 3.76
   real, parameter :: gamma = 1.4

   ! Cantera
   integer, parameter :: nSpecCantera = 53
   integer, parameter :: nVarsCantera = 57
   integer, parameter :: iT_cantera = 2
   integer, parameter :: iH2_Cantera = 5
   integer, parameter :: iO2_Cantera = 8
   integer, parameter :: iH_Cantera = 6
   integer, parameter :: iH2O_Cantera = 10
   integer, parameter :: iN2_Cantera = 52
   integer, parameter :: numberOfNonSpec = 4
   
   ! From Cantera using gas.getMolecularWeights(weights) 
   real, parameter :: MWiCantera(nSpec) = (/2.01588,31.9988,1.00794,18.01528,28.01348/)


end module
