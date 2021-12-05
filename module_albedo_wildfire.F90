uWILDFIRES WILDFIRES WILDFIRES WILDFIRES WILDFIRES WILDFIRES WILDFIRES WILDFIRES WILDFIRES 
!WILDFIRES WILDFIRES WILDFIRES WILDFIRES WILDFIRES WILDFIRES WILDFIRES WILDFIRES WILDFIRES 

 module module_albedo_wildfire
 implicit none

!---------------------------------------------------------------------------------------------------
! NASA GSFC makes no representations about the suitability of software for any purpose. 
! It is provided as is without express or implied warranty. Neither NASA GSFC (the US 
! government) nor Principal Developers (their organizations) shall be liable for any 
! damages suffered by the user of this software. In addition, please do not distribute 
! the software to third party.
!
! Comments:  
!   This module purturbe surface albedo for a given wildfire fraction. 
!   The Look-Up-Table (LUT) of buruned area albedo is provided by Dr. Charles Gatabe @ NASA GSFC.
!
!   Instrument: CAR (NASA's Cloud Absorption Radiometer)
!   Field campaign – Skukuza field campaign. 
!   Area: South Africa – savannah  [Lat:25.23260 deg South, Lon: 31:303376 deg East]
!   date: May 20, 2005
!  
!
! ---subroutine perturb_albedo_wildfire---
! Usage Derive spectrum burned albedo, which require following inputs/output. 
!
!  real :: wavel_in       !wavelength [um]
!  real :: albedo_in      ! normal albedo w/o wildfire  (0.~ 1.)
!  real :: frac_wildfire  ! pixel fraction of wildfires (0.~ 1.)
!  real :: albedo_out     ! purturbed albedo after wildfire (0.~ 1.)
!  real :: albedo_burned  ! albedo of just burned area (0.~ 1.)
!                     
!  call perturb_albedo_wildfire(wavel_in, albedo_in, frac_wildfire, albedo_out, albedo_burned)
!
! 
! -- subroutine perturb_albedo_wildfire_broadband ----
! Usage: Derive broadband burned albedo, which requires following inputs/outputs.
!
!  character(len=10) :: typ_band ! UV, VIS, NIR, UVVIS, VISNIR, or UVVISNIR depending on the radiation band you want. 
!  real :: wavel_in       !wavelength [um]
!  real :: albedo_in      ! normal albedo w/o wildfire  (0.~ 1.)
!  real :: frac_wildfire  ! pixel fraction of wildfires (0.~ 1.)
!  real :: albedo_out     ! purturbed albedo after wildfire (0.~ 1.)
!  real :: albedo_burned  ! albedo of just burned area (0.~ 1.)
!  For example if you want to broandband shortwave radiation (from UV to NIR spectrum) --> UVVISNIR
!              if you want to VIS band radiation --> VIS
!
!  call perturb_albedo_wildfire_broadband(trim(typ_band), albedo_in, frac_wildfire, albedo_out, albedo_burned)
!
!
! History: 
!  08/2015  Toshi Matsui@NASA GSFC ; Initial.
!           
! References: 
!---------------------------------------------------------------------------------------------------
 save     ! all module parameters will be saved

!
! Encapsulation control 
!
 private   ! encapsulate all variables and subourtines (non accessible)

!
! public (accessible) subroutines
!
 public :: perturb_albedo_wildfire, &
           perturb_albedo_wildfire_broadband 


!
! This Look-Up-Table (LUT) buruned area albedo is provided by Dr. Charles Gatabe @ NASA GSFC.
!
! Instrument: CAR (NASA's Cloud Absorption Radiometer)
! Field campaign – Skukuza field campaign. 
! Area: South Africa – savannah  [Lat:25.23260 deg South, Lon: 31:303376 deg East]
! date: May 20, 2005
!
! More details on the flight: http://car.gsfc.nasa.gov/data/index.php?id=131&mis_id=10&n=Skukuza
!

 integer,parameter :: nband_sw = 8 

 real,parameter :: lut_wavel_albedo(nband_sw) = &  ! anchor points wavelength [um]
    (/0.34, 0.381, 0.472, 0.682, 0.87, 1.036 , 1.219 , 1.273/)

 real,parameter :: lut_albedo_burned(nband_sw) = &  ! burned-area spectrum albedo [-]
    (/0.02041, 0.02615, 0.02602, 0.04159, 0.06536, 0.07777 , 0.08785 , 0.08511/)

! real,parameter :: lut_albedo_natural(nband_sw) = &  ! Savannah natural spectrum albedo w/o wildfire  [-]
!    (/0.02228, 0.03176, 0.03873, 0.09484, 0.18488, 0.21797 , 0.23876 , 0.24560/)

!
! Derive broadband burned albedo LUT using Plank's Law using above spectrum albedo
! for UV, VIS, NIR, UVVIS, VISNIR, UVVISNIR range
!

 contains

!WILDFIRES WILDFIRES WILDFIRES WILDFIRES WILDFIRES WILDFIRES WILDFIRES WILDFIRES WILDFIRES 
!WILDFIRES WILDFIRES WILDFIRES WILDFIRES WILDFIRES WILDFIRES WILDFIRES WILDFIRES WILDFIRES 

 subroutine perturb_albedo_wildfire_broadband ( type_band, albedo_in, frac_wildfire,  albedo_out , albedo_burned )
 implicit none
!---------------------------------------------------------------------------------------------------
! Comments:  
!   This module purturbe broadband surface albedo for a given wildfire fraction. 
!   Make sure input/output albedo units are fraction (0.0 ~ 1.0), NOT in %. 
!
! History: 
!  08/2015  Toshi Matsui@NASA GSFC ; Initial.
!           
! References: 
!---------------------------------------------------------------------------------------------------
 character(len=*),intent(in) :: type_band ! wavelength of albedo [um]
 real,intent(in) :: albedo_in      ! normal albedo w/o wildfire  (0.~ 1.)
 real,intent(in) :: frac_wildfire  ! pixel fraction of wildfires (0.~ 1.)
 real,intent(out) :: albedo_out    ! purturbed albedo after wildfire (0.~ 1.)
 real,intent(out) :: albedo_burned ! interpolated burned-area albedo at input wavelength [-]

 integer :: n
 integer :: nband_start , nband_end 
 real :: wave_m  ![m]
 real :: Bt, Bt_sum , alb_sum

!Boltzman's constant
 real,parameter :: c1 = 3.74e-16  !1st constant [W m2]
 real,parameter :: c2 = 1.439e-2  !2nd constant [m K]
 real,parameter :: t_sun = 6100.  !solar radiative temperature [K]

!
! default output equal to the input
!
  albedo_out = albedo_in

!
!
! check input first
!

 select case(trim(type_band)) 
 case('UV')
  nband_start = 1 ; nband_end = 2
 case('VIS')
  nband_start = 3 ; nband_end = 4
 case('NIR')
  nband_start = 5 ; nband_end = 8
 case('UVVIS')
  nband_start = 1 ; nband_end = 4 
 case('VISNIR')
  nband_start = 3 ; nband_end = 8
 case('UVVISNIR')
  nband_start = 1 ; nband_end = 8
 case default
   print*,'MSG perturb_albedo_wildfire_broad: Warning no such type of type_band. Return'
   return 
 end select

 if( albedo_in < 0.e0 ) then
    print*,'MSG perturb_albedo_wildfire_broad: Warning input albedo is negative. Return'
    return
 elseif( albedo_in > 1.e0 ) then
    print*,'MSG perturb_albedo_wildfire_broad: Warning input albedo is greater than 1.0. Return'
    return
 endif

 if( frac_wildfire < 0.e0 ) then
    print*,'MSG perturb_albedo_wildfire_broad: Warning input frac_wildfire is negative. Return'
    return
 elseif( frac_wildfire > 1.e0 ) then
    print*,'MSG perturb_albedo_wildfire_broad: Warning input frac_wildfire is greater than 1.0. Return'
    return
 endif

 if( frac_wildfire == 0.e0 ) then
   return
 endif

!
! integrate spectrum albedo with weight of solar radiation flux via Plank's Law
!
 Bt_sum = 0.  ; alb_sum = 0.
 sw_loop: do n = nband_start, nband_end
    wave_m = lut_wavel_albedo(n) * 1.e-6 ![m]
    Bt = ( c1 / ( wave_m ** 5 ) )/ ( exp(c2/(wave_m*t_sun))-1.e0 ) * 1.e-12  !Plank's Law [Mega Wm-2 um-1]
    Bt_sum = Bt_sum + Bt   !integrate weight 
    alb_sum = alb_sum + Bt * lut_albedo_burned(n) !integrate albedo
 enddo sw_loop

 albedo_burned = alb_sum / Bt_sum  ! broadband burned albedo [-]

!
! output perturbed albedo 
!
 if( frac_wildfire == 1.e0 ) then
   albedo_out = albedo_burned  ! [-]
 else
   albedo_out = frac_wildfire * albedo_burned + (1.-frac_wildfire) * albedo_in  ! [-]
 endif

 end subroutine perturb_albedo_wildfire_broadband

!WILDFIRES WILDFIRES WILDFIRES WILDFIRES WILDFIRES WILDFIRES WILDFIRES WILDFIRES WILDFIRES 
!WILDFIRES WILDFIRES WILDFIRES WILDFIRES WILDFIRES WILDFIRES WILDFIRES WILDFIRES WILDFIRES 

 subroutine perturb_albedo_wildfire ( wavel_in, albedo_in, frac_wildfire,  albedo_out , albedo_burned )
 implicit none
!---------------------------------------------------------------------------------------------------
! Comments:  
!   This module purturbe spectrum surface albedo for a given wildfire fraction. 
!   Make sure input/output albedo units are fraction (0.0 ~ 1.0), NOT in %. 
!
! History: 
!  08/2015  Toshi Matsui@NASA GSFC ; Initial.
!           
! References: 
!---------------------------------------------------------------------------------------------------
 real,intent(in) :: wavel_in       ! wavelength of albedo [um]
 real,intent(in) :: albedo_in      ! normal albedo w/o wildfire  (0.~ 1.)
 real,intent(in) :: frac_wildfire  ! pixel fraction of wildfires (0.~ 1.)
 real,intent(out) :: albedo_out    ! purturbed albedo after wildfire (0.~ 1.)
 real,intent(out) :: albedo_burned ! interpolated burned-area albedo at input wavelength [-]

 integer :: n
 real :: wgt1, wgt2   

!
! default output equal to the input
!
  albedo_out = albedo_in

!
!
! check input first
!
 if( wavel_in < 0.e0 ) then
    print*,'MSG perturb_albedo_wildfire: Warning input wavelength is negative. Return'
 endif

 if( albedo_in < 0.e0 ) then
    print*,'MSG perturb_albedo_wildfire: Warning input albedo is negative. Return'
    return
 elseif( albedo_in > 1.e0 ) then
    print*,'MSG perturb_albedo_wildfire: Warning input albedo is greater than 1.0. Return'
    return
 endif

 if( frac_wildfire < 0.e0 ) then
    print*,'MSG perturb_albedo_wildfire: Warning input frac_wildfire is negative. Return'
    return
 elseif( frac_wildfire > 1.e0 ) then
    print*,'MSG perturb_albedo_wildfire: Warning input frac_wildfire is greater than 1.0. Return'
    return
 endif

 if( frac_wildfire == 0.e0 ) then
   return
 endif


!
! interpolate burned area albedo for a given wavelength
!
 if( wavel_in <= lut_wavel_albedo(1) ) then 

    albedo_burned = lut_albedo_burned(1)

 elseif( wavel_in >= lut_wavel_albedo(nband_sw) )  then

    albedo_burned = lut_albedo_burned(nband_sw)

 else

    sw_loop: do n = 1, nband_sw-1

       if( wavel_in >= lut_wavel_albedo(n) .and. wavel_in <= lut_wavel_albedo(n+1) ) then
           wgt1 = ( lut_wavel_albedo(n+1) - wavel_in ) / ( lut_wavel_albedo(n+1) - lut_wavel_albedo(n) )
           wgt2 = 1.-wgt1
           albedo_burned = wgt1 * lut_albedo_burned(n) + wgt2 * lut_albedo_burned(n+1)
           exit sw_loop
       endif

    enddo sw_loop

 endif

!
! output perturbed albedo 
!
 if( frac_wildfire == 1.e0 ) then
   albedo_out = albedo_burned   ! [-]
 else
   albedo_out = frac_wildfire * albedo_burned + (1.-frac_wildfire) * albedo_in    ! [-]
 endif

 end subroutine perturb_albedo_wildfire

!WILDFIRES WILDFIRES WILDFIRES WILDFIRES WILDFIRES WILDFIRES WILDFIRES WILDFIRES WILDFIRES 
!WILDFIRES WILDFIRES WILDFIRES WILDFIRES WILDFIRES WILDFIRES WILDFIRES WILDFIRES WILDFIRES 

 end module module_albedo_wildfire

! Below is example main program to call these subroutines. T. Matsui

!WILDFIRES WILDFIRES WILDFIRES WILDFIRES WILDFIRES WILDFIRES WILDFIRES WILDFIRES WILDFIRES 
!WILDFIRES WILDFIRES WILDFIRES WILDFIRES WILDFIRES WILDFIRES WILDFIRES WILDFIRES WILDFIRES 

 program main
 use module_albedo_wildfire

 real :: wavel_in      !wavelength [um]
 real :: albedo_in      ! normal albedo w/o wildfire  (0.~ 1.)
 real :: frac_wildfire  ! pixel fraction of wildfires (0.~ 1.)
 real :: albedo_out    ! purturbed albedo after wildfire (0.~ 1.)
 real :: albedo_burned !burned albedo

 character(len=10) :: typ_band

 typ_band = 'VISNIR'
 wavel_in = 0.5 
 albedo_in = 0.2 
 frac_wildfire = 0.5 

 !
 ! units: wavel_in [um]; 
 ! albedo_in, frac_wildfire, albedo_out,albedo_burned are all fraction (0.0~1.0)
 ! 
 print*,'Call perturb_albedo_wildfire'
 call perturb_albedo_wildfire(wavel_in, albedo_in, frac_wildfire, albedo_out, albedo_burned)

 write(*,FMT='(5F10.4)') wavel_in, albedo_in, frac_wildfire, albedo_out, albedo_burned


 !
 ! typ_band:  UV, VIS, NIR, UVVIS, VISNIR, or UVVISNIR depending on the radiation band you want. 
 ! units: albedo_in, frac_wildfire, albedo_out, albedo_burned are all fraction (0.0~1.0)
 ! For example if you want to broandband shortwave radiation (from UV to NIR spectrum) --> UVVISNIR
 !             if you want to VIS band radiation --> VIS
 !
 print*, 'Call perturb_albedo_wildfire_broadband' 
 call perturb_albedo_wildfire_broadband(trim(typ_band), albedo_in, frac_wildfire, albedo_out, albedo_burned)

 write(*,FMT='(A10,1X,4F10.4)') trim(typ_band), albedo_in, frac_wildfire, albedo_out, albedo_burned


 end program main

!WILDFIRES WILDFIRES WILDFIRES WILDFIRES WILDFIRES WILDFIRES WILDFIRES WILDFIRES WILDFIRES 
!WILDFIRES WILDFIRES WILDFIRES WILDFIRES WILDFIRES WILDFIRES WILDFIRES WILDFIRES WILDFIRES 
