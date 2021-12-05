
!FIRE EMISS FIRE EMISS FIRE EMISS FIRE EMISS FIRE EMISS FIRE EMISS FIRE EMISS FIRE EMISS FIRE EMISS FIRE EMISS 
!FIRE EMISS FIRE EMISS FIRE EMISS FIRE EMISS FIRE EMISS FIRE EMISS FIRE EMISS FIRE EMISS FIRE EMISS FIRE EMISS


!=============================================================================================================
! LANGUAGE  : FORTRAN 90
! PROGRAM   : 
! MODULE    : Routines for gridding and outputting files for modeling from data 
!
! Description:
! There are three options for processes: SEVIRI, MODIS, or BOTH.  The processes read in data from
! a file, grid it, then output a binary file for modeling.
!
!=============================================================================================================
 
 module module_femission


!============================================================!
! Process statement:                                         !
!                                                            !
! Choose whether to process SEVIRI data, MODIS data, or BOTH !
! Choose whether to run program verbose for debugging or not !
!============================================================!

 character(len=*),parameter :: process = 'SEVIRI'  ! 'SEVIRI' , 'MODIS' , 'BOTH'

 logical,parameter :: verbose = .false. ! for debugging

!===================================================================================================!
! Global parameters for MODIS                                                                       !
!                                                                                                   !
! dir                  : directory of file                                                          !
! file_name            : name of file that contains a list of all the files that need to be read in !
! nmax_file            : length of file opened/read                                                 !
! emisfile             : 1D array to be filled with list of files to read in                        !
! frp                  : 2D array to be filled with frp data in correct lat/lon location            !
! max_lat to min_lon   : max/min latitude and longitude of domain                                   !
! grid_res             : grid resolution                                                            !
! lat, lon             : 1D arrays of lat/lon                                                       !
! lat_index, lon_index : 1D arrays to hold index values of lat/lon - for testing                    ! 
! nmaxsz_arry          : size of array                                                              !
! i_max, j_max         : latitude max and longitude max                                             !
!===================================================================================================!

  character (len = 100),parameter :: dir = '/Users/tdmichael/Desktop/Emissions/FRP_data/MODFIRE/MOD14/005/2009/'
  character (len = 100),parameter :: file_name = 'practice_inpfile'
  integer :: nmax_file 
  character (len = 100), allocatable, dimension(:) :: emisfile
  real, allocatable, dimension(:,:) :: frp
  real, parameter :: max_lat = 15.6, min_lat = 0.3, max_lon = 52.9 , min_lon = -18.5 
  real, parameter :: grid_res = 0.1
  real, allocatable, dimension(:) :: lat, lon
  integer, allocatable, dimension (:) :: lat_index, lon_index
  integer :: nmaxsz_arry
  integer :: i_max, j_max  
  
!==================================!
! User defined type for MODIS data !
!==================================!

  type femis_parameter
                                            ! Units:          ! Descriptions:
    character (len = 10) ::   Date          ! YYYY-MM-DD      ! Date
    character (len = 5)  ::   Time          ! HH:MM           ! Time
    character (len = 41) ::   Filename      ! .hdf            ! Filename
    integer              ::   FP_line       !
    integer              ::   FP_sample     !
    real                 ::   FP_latitude   ! Degrees         ! Longitude
    real                 ::   FP_longitude  ! Degrees         ! Latitude
    real                 ::   FP_R2         !
    real                 ::   FP_T21        !
    real                 ::   FP_T31        !
    real                 ::   FP_MeanT21    !
    real                 ::   FP_MeanT31    !
    real                 ::   FP_MeanDT     !
    real                 ::   FP_MAD_T21    !
    real                 ::   FP_MAD_T31    !
    real                 ::   FP_MAD_DT     !
    real                 ::   FP_power      ! MW              ! Fire Radiative Power
    integer              ::   FP_AdjCloud   !
    integer              ::   FP_AdjWater   !
    integer              ::   FP_WinSize    !
    integer              ::   FP_NumValid   ! 
    integer              ::   FP_confidence !

  end type femis_parameter

  type( femis_parameter ),allocatable,dimension(:) :: sample  ! Declare array for data contents
  integer,parameter :: nmax_line = 10000                      ! Declare a maximum line amount used to allocate for sample


!========================================================================================================!
! Global parameters for SEVIRI                                                                           !
!                                                                                                        !
! sev_dir                   : directory of file                                                          !
! sev_file_name             : name of file that contains a list of all the files that need to be read in !
! output_dir                : directory of output file                                                   !
! nmax_sevfile              : length of file opened/read                                                 !
! sevfile                   : 1D array, list of files to read in                                         !
! sev_frp                   : 2D array, frp data in correct lat/lon location                             !
! count_sev_frp             : 2D array, number of times a value is inserted into grid location           !
! sum_sev_frp               : 2D array, sum of frp values in a file                                      !
! max_slat to min_slon      : max/min latitude and longitude of domain                                   !
! sgrid_res                 : grid resolution                                                            !
! s_lat, s_lon              : 1D arrays of lat/lon                                                       !
! s_lat_index, s_lon_index  : 1D arrays to hold index values of lat/lon - for testing                    !
! nmaxsz_sevarry            : size of array                                                              !
!========================================================================================================!

  character (len = 100),parameter :: sev_dir = '/Users/tdmichael/Desktop/Emissions/FRP_data/LSASAF_MSG/&
    FRP-PIXEL/North_Africa/2009/12/'
  character (len = 100),parameter :: sev_file_name = 'inpfile'
  character (len = 100),parameter :: output_dir = '/Users/tdmichael/GRIDPROGRAM/Data_out/test1/'
  integer :: nmax_sevfile
  character (len = 100), allocatable, dimension(:) :: sevfile
  real, allocatable, dimension(:,:) :: sev_frp        
  real, allocatable, dimension(:,:) :: count_sev_frp  
  real, allocatable, dimension(:,:) :: sum_sev_frp     
  real, parameter :: max_slat = 15.6, min_slat = 0.3, max_slon = 52.9 , min_slon = -18.5
  real, parameter :: sgrid_res = 0.1
  real, allocatable, dimension(:) :: s_lat, s_lon
  integer, allocatable, dimension (:) :: s_lat_index, s_lon_index
  integer :: nmaxsz_sevarry


!===================================!
! User defined type for SEVIRI data !
!===================================!

  type sevi_parameter 
                                  ! Units                   ! Descriptions: 
    integer     ABS_LINE          ! p.n.
    integer     ABS_PIXEL         ! p.n.
    integer     ACQTIME           ! p.n.
    real        BT_MIR            ! K
    real        BT_TIR            ! K
    real        BW_BTD            ! K
    real        BW_BT_MIR         ! K
    integer     BW_NUMPIX         ! p.n.
    integer     BW_SIZE           ! p.n.
    real        ERR_ATM_TRANS     ! p.n.
    real        ERR_BACKGROUND    ! p.n.
    real        ERR_FRP_COEFF     ! p.n.
    real        ERR_RADIOMETRIC   ! p.n.
    real        ERR_VERT_COMP     ! p.n.
    real        FIRE_CONFIDENCE   ! p.n.
    real        FRP               ! MW                     ! Fire Radiative Power
    real        FRP_UNCERTAINTY   ! MW                   
    real        LATITUDE          ! Degree                 ! Latitude
    real        LONGITUDE         ! Degree                 ! Longitude
    real        PIXEL_ATM_TRANS   ! p.n.
    real        PIXEL_SIZE        ! Km
    real        PIXEL_VZA         ! Degree
    real        RAD_BCK           ! mW/(m^2 sr cm^-1)
    real        RAD_PIX           ! mW/(m^2 sr cm^-1)
    integer     REL_LINE          ! p.n.
    integer     REL_PIXEL         ! p.n.
    real        STD_BCK           ! mW/(m^2 sr cm^-1) 
    integer     QUALITYFLAG       ! p.n. 
    integer     DEM               ! m
    integer     LANDCOV           ! Adim.
    

  end type sevi_parameter

  type( sevi_parameter ),allocatable,dimension(:) :: sev_data  ! Declare array for data contents
  integer,parameter :: nmax_line_sev = 10000                   ! Declare a maximum line amount used to allocate for sev_data

!====================!
! Contains statement !
!====================!  

  contains 


!FIRE EMISS FIRE EMISS FIRE EMISS FIRE EMISS FIRE EMISS FIRE EMISS FIRE EMISS FIRE EMISS FIRE EMISS 
!FIRE EMISS FIRE EMISS FIRE EMISS FIRE EMISS FIRE EMISS FIRE EMISS FIRE EMISS FIRE EMISS FIRE EMISS 


!==========================================================================!
! Subroutine init_sevfile (Initialize SEVIRI file):                        !
! Use to open, allocate, and read in file list used for SEVIRI data        !
!==========================================================================!

  subroutine init_sevfile
  implicit none

!==============!
! Declarations !
!==============!
  
  character (len = 200) :: inp_file   ! input file name
  integer, parameter :: input = 105   ! input used as unit=105 for opening a file
  integer :: n = 0                    ! used for loop iterations
  integer :: ioer                     ! input/output error
  character (len = 10) :: dummy       ! dummy used in loop to determine number of files to be read in

!=================================!
! Define inp_file as file and dir !
!=================================!

  inp_file = trim(sev_dir)//trim(sev_file_name)
  if(verbose) print*, 'SEVIRI  ',inp_file

!================!
! Open statement !
!================!

  open (unit = input, file = trim(inp_file), status = 'old', action = 'read', iostat = ioer)

  if (ioer /= 0) then
    print*, 'Cannot open file'
  else
    if(verbose) print*,'success opening file'
  end if

!===============================================!
! Forever loop to determine length of file read !
!===============================================!

  n = 0
  forever : do
    n = n + 1
    read(unit = input, fmt = *, iostat = ioer, end = 999) dummy
  end do forever
  999 nmax_sevfile = n - 1
  if(verbose) print*, nmax_sevfile

!======================!
! Allocation statement !
!======================!

  allocate ( sevfile(1:nmax_sevfile) )


  rewind (unit = input)   !Go back to beginning of file

!========================================!
! Read in each file and store in sevfile !
!========================================!

  do n=1, nmax_sevfile
    read(unit = input, fmt = *, iostat = ioer) sevfile(n)
    if (verbose) print*, trim(sevfile(n))
  end do
  close (unit = input)

  end subroutine init_sevfile

!FIRE EMISS FIRE EMISS FIRE EMISS FIRE EMISS FIRE EMISS FIRE EMISS FIRE EMISS FIRE EMISS FIRE EMISS 
!FIRE EMISS FIRE EMISS FIRE EMISS FIRE EMISS FIRE EMISS FIRE EMISS FIRE EMISS FIRE EMISS FIRE EMISS 

!=====================================================!
! Subroutine allocate_sev (Allocate SEVIRI):          ! 
! Use to allocate memory for the data contents        !
!=====================================================!

 subroutine allocate_sev
   
   if (verbose) print*,'allocate seviri data parameter'
   allocate( sev_data(nmax_line_sev) )
   if (verbose) print*, sev_data(nmax_line_sev) 

 end subroutine allocate_sev


!FIRE EMISS FIRE EMISS FIRE EMISS FIRE EMISS FIRE EMISS FIRE EMISS FIRE EMISS FIRE EMISS FIRE EMISS 
!FIRE EMISS FIRE EMISS FIRE EMISS FIRE EMISS FIRE EMISS FIRE EMISS FIRE EMISS FIRE EMISS FIRE EMISS


!=====================================================!
! Subroutine read_sevfile (Read SEVIRI file):         !
! Use to read the contents of the data files listed   !
!=====================================================!

  subroutine read_sevfile(emisin)
  implicit none 

!==============!
! Declarations !
!==============!

   character ( len = 100), intent(in) :: emisin   ! data file to be read, passed in
   character (len = 200) :: inp_file              ! input file name
   integer :: input = 120                         ! input used as unit=120 for opening a file
   integer :: ioer                                ! input/output error
   character (len = 10) :: dummy                  ! dummy used in loop to determine number of files to be read in
   integer :: n                                   ! used for loop iterations

   if (verbose) print*, emisin,'Different files'

!=========================!
! Define I/O file and dir !
!=========================!

    inp_file = trim(sev_dir)//trim(emisin)
    if (verbose) print*, inp_file

!================!
! Open statement !
!================!
    open (unit = input, file = trim(inp_file), status = 'old', action = 'read', iostat = ioer)

    if (ioer /= 0) then
      print*, 'Cannot open file'
    else
      if(verbose) print*,'success opening file'
    end if

!===================================================!
! Forever loop to determine length of file contents !
!===================================================!

    n = 0
    forever : do
      n = n + 1
      read(unit = input, fmt = *, iostat = ioer, end = 999) dummy
    end do forever

    999 nmax_sevfile = n - 1 - 3
    if(verbose) print*, nmax_sevfile

!=================! 
! Go to beginning ! 
!=================!

    rewind(input)
    if (verbose) print*,'rewind file'

!==================================================================!
! Read first 3 lines as dummy (the first 3 lines are descriptions) !
!==================================================================!

    do n = 1, 3
      read(unit = input, fmt = *, iostat = ioer, end = 999) dummy
      if (verbose) print*,'dummy',dummy
    enddo

!=====================================================!
! Read in contents of each file and print (if needed) !
!=====================================================!

    file_read: do n = 1, nmax_sevfile
      read(unit = input, fmt = *, iostat = ioer, end = 999) sev_data(n)

      if (verbose) print*,inp_file,sev_data(n)
      if (verbose) print*,nmax_sevfile
    
    end do file_read

!    nmaxsz_sevarry = size(sev_data)
!    print*, nmaxsz_arry
!    stop


  end subroutine read_sevfile


!FIRE EMISS FIRE EMISS FIRE EMISS FIRE EMISS FIRE EMISS FIRE EMISS FIRE EMISS FIRE EMISS FIRE EMISS 
!FIRE EMISS FIRE EMISS FIRE EMISS FIRE EMISS FIRE EMISS FIRE EMISS FIRE EMISS FIRE EMISS FIRE EMISS

!===========================================================================!
! Subroutine define_sevgrid (Define SEVIRI grid):                           !
! Define a lat/lon 1D array within the boundaries set and resolution wanted !
! then populate a 2D array with selected data in lat/lon specific grid      !
!===========================================================================!

  subroutine define_sevgrid
  implicit none

!==============!
! Declarations !
!==============!

    integer :: n              ! used for loop iterations

!===============================================================!
! Define i_max, j_max from the domain boundaries and resolution !
!===============================================================!

    i_max = (max_slon - min_slon)/sgrid_res +1
    j_max = (max_slat - min_slat)/sgrid_res +1

    if(verbose) print*,'lon max  ', i_max,'lat max   ',j_max,'s_grid_res  ',sgrid_res

!==================================!
! Allocate frp, lat, & lon  arrays !
!==================================!

    allocate(s_lat(1:j_max))

    allocate(s_lon(1:i_max))
 
    if( .not. allocated(sev_frp) ) &
      allocate(sev_frp(1:i_max, 1:j_max), count_sev_frp(1:i_max, 1:j_max), sum_sev_frp(1:i_max, 1:j_max)  )

!=========================================================================================!
! Do loop to populate lat array at resolution degree intervals ending at defined boundary !
!=========================================================================================!

    init_lat: do n=1, j_max
      s_lat(n)= min_slat + sgrid_res*real(n)
      if(verbose) print*,'lat',n,' ',s_lat(n)
    end do init_lat

!=================================================================================!
! Do loop to populate lon array at resolution degree intervals ending at boundary !
!=================================================================================!

    init_lon: do n=1, i_max
      s_lon(n) = min_slon + sgrid_res*real(n)
      if(verbose) print*,'lon',n,' ',s_lon(n)
    end do init_lon

  end subroutine define_sevgrid

!FIRE EMISS FIRE EMISS FIRE EMISS FIRE EMISS FIRE EMISS FIRE EMISS FIRE EMISS FIRE EMISS FIRE EMISS 
!FIRE EMISS FIRE EMISS FIRE EMISS FIRE EMISS FIRE EMISS FIRE EMISS FIRE EMISS FIRE EMISS FIRE EMISS 
 
!=================================================================!
! Subroutine pop_grid (Populate grid):                            !
! Populate a 2D array with selected data in lat/lon specific grid ! 
!=================================================================! 
 
  subroutine pop_sevgrid (emisin)
  implicit none

!==============!
! Declarations !
!==============!

    integer :: n, m, i, j                         ! used for loop iterations
    integer :: i_grid, j_grid                     ! lat/lon indexes determined from lat/lon values
    character ( len = 100), intent(in) :: emisin  ! data file to be read, passed in
    character(len=200) :: out_file                ! output file
    integer, parameter :: io = 110                ! open/write designation

!===============================================================! 
! Initialize each array (0.) at the beginning of each iteration !
!===============================================================!

    sev_frp = 0. ; count_sev_frp = 0. ; sum_sev_frp = 0.

!============================================================================================!
! Do loop to populate count_sev_frp/sum_sev_frp with frp data in correct lat/lon location:   !
! First make sure value is in the domain.  If so, get lat/lon indices.                       !
! Then count/sum the values in a location.                                                   !
!============================================================================================!

    frp_fill: do n=1, nmax_sevfile
      if ((sev_data(n)%LATITUDE < min_slat) .or.(sev_data(n)%LATITUDE  > max_slat) .or. &
      (sev_data(n)%LONGITUDE < min_slon) .or. (sev_data(n)%LONGITUDE > max_slon)  ) then
        cycle frp_fill

      else
        i_grid =  ((sev_data(n)%LONGITUDE  - min_slon) / sgrid_res)+1
        j_grid =  ((sev_data(n)%LATITUDE - min_slat) / sgrid_res)+1

        count_sev_frp(i_grid, j_grid) = count_sev_frp(i_grid, j_grid) +1
        sum_sev_frp  (i_grid, j_grid) = sum_sev_frp  (i_grid, j_grid) +sev_data(n)%FRP

        if (verbose) print*,n,'count',count_sev_frp(i_grid,j_grid),'i ',i_grid,'j ',j_grid
        if (verbose) print*,'lat  ',sev_data(n)%LATITUDE,'index  ',j_grid,'lon  ',sev_data(n)%LONGITUDE,'index  ',i_grid
      end if
    
    end do frp_fill
 

!=======================================================================!
! Do loop to average multiple frp values for one location from one file !
! and store averaged frp value into sev_frp by lat/lon indices          !
!=======================================================================!

    average: do i=1, i_max 
      inna: do j=1, j_max

        if( count_sev_frp(i,j) > 0. ) then 
          sev_frp(i,j)=sum_sev_frp(i,j)/count_sev_frp(i,j) 

        else
          sev_frp(i,j) = 0.

        end if

        if ( (sev_frp(i,j) > 0) .and. (verbose) ) &
          print*, sev_frp(i,j), sum_sev_frp(i,j), count_sev_frp(i,j)

      end do inna
    end do average

!===================================================================!
! Define, open, and write output (gridded data) file to binary file !
!===================================================================!

    if (verbose) print*,emisin(44:55),i_max,j_max

    out_file = trim(output_dir)//'SEVIRI_FRP_'//emisin(44:55)//'.bin' 
    if (verbose) print*,trim(out_file)

    open (io, file = out_file, access = 'direct', status='replace',recl = i_max*j_max*4)

    write(io, rec = 1) sev_frp  !(1:i_max, 1:j_max)
    write(io, rec = 2) count_sev_frp !(1:i_max, 1:j_max)

    close(io)

  end subroutine pop_sevgrid

!FIRE EMISS FIRE EMISS FIRE EMISS FIRE EMISS FIRE EMISS FIRE EMISS FIRE EMISS FIRE EMISS FIRE EMISS 
!FIRE EMISS FIRE EMISS FIRE EMISS FIRE EMISS FIRE EMISS FIRE EMISS FIRE EMISS FIRE EMISS FIRE EMISS

!====================================================!
! Subroutine exec_sev (Execute SEVIRI):          
!
!====================================================!

  subroutine exec_sev
  implicit none

  end subroutine exec_sev


!FIRE EMISS FIRE EMISS FIRE EMISS FIRE EMISS FIRE EMISS FIRE EMISS FIRE EMISS FIRE EMISS FIRE EMISS 
!FIRE EMISS FIRE EMISS FIRE EMISS FIRE EMISS FIRE EMISS FIRE EMISS FIRE EMISS FIRE EMISS FIRE EMISS


!==========================================================================!
! Subroutine init_modfile (Initialize MODIS file):                         !
! Use to open, allocate, and read in file list used for MODIS data         !
!==========================================================================!

  subroutine init_modfile
  implicit none

!==============!
! Declarations !
!==============!

  character (len = 200) :: inp_file   ! input file name
  integer, parameter :: input = 100   ! input used as unit=100 for opening a file
  integer :: n = 0                    ! used for loop iterations 
  integer :: ioer                     ! input/output error
  character (len = 10) :: dummy       ! dummy used in loop to determine number of files to be read in


!=========================!
! Define I/O file and dir !
!=========================!

  inp_file = trim(dir)//trim(file_name)
  if(verbose) print*, 'MODIS   ',inp_file

!================!
! Open statement !
!================!

  open (unit = input, file = trim(inp_file), status = 'old', action = 'read', iostat = ioer)
  
  if (ioer /= 0) then
    print*, 'Cannot open file'
  else
    if(verbose) print*,'success opening file'
  end if


!===================================================!
! Forever loop to determine length of file contents !
!===================================================!

  n = 0
  forever : do
    n = n + 1
    read(unit = input, fmt = *, iostat = ioer, end = 999) dummy
  end do forever
  999 nmax_file = n - 1
  if(verbose) print*, nmax_file

!======================!
! Allocation statement !
!======================!

  allocate ( emisfile(1:nmax_file) )

 
  rewind (unit = input)   !Go back to beginning of file

!========================================!
! Read in each file and store in sevfile !
!========================================!

  do n=1, nmax_file
    read(unit = input, fmt = *, iostat = ioer) emisfile(n)
    if (verbose) print*, trim(emisfile(n))
  end do 
  close (unit = input)

  end subroutine init_modfile


!FIRE EMISS FIRE EMISS FIRE EMISS FIRE EMISS FIRE EMISS FIRE EMISS FIRE EMISS FIRE EMISS FIRE EMISS 
!FIRE EMISS FIRE EMISS FIRE EMISS FIRE EMISS FIRE EMISS FIRE EMISS FIRE EMISS FIRE EMISS FIRE EMISS 

!=====================================================!
! Subroutine allocate_mod (Allocate MODIS):           ! 
! Use to allocate memory for the data contents        !
!=====================================================!

 subroutine allocate_mod
   if(verbose) print*,'allocate sample parameter for MODIS'
   allocate( sample(nmax_line) )

 end subroutine allocate_mod 


!FIRE EMISS FIRE EMISS FIRE EMISS FIRE EMISS FIRE EMISS FIRE EMISS FIRE EMISS FIRE EMISS FIRE EMISS 
!FIRE EMISS FIRE EMISS FIRE EMISS FIRE EMISS FIRE EMISS FIRE EMISS FIRE EMISS FIRE EMISS FIRE EMISS 

!=====================================================!
! Subroutine read_modfile (Read MODIS file):          !
! Use to read the contents of the data files listed   !
!=====================================================!

 subroutine read_modfile(emisin)
 implicit none
 
!==============!
! Declarations !
!==============!
 
   character ( len = 100), intent(in) :: emisin   ! data file to be read, passed in
   character (len = 200) :: inp_file              ! input file name
   integer :: input = 120                         ! input used as unit=120 for opening a file
   integer :: ioer                                ! input/output error
   character (len = 10) :: dummy                  ! dummy used in loop to determine number of files to be read in 
   integer :: n                                   ! used for loop iterations

   if (verbose) print*, emisin,'Different files'
  
!=========================!
! Define I/O file and dir !
!=========================!

    inp_file = trim(dir)//trim(emisin)
    if (verbose) print*, inp_file

!================!
! Open statement !
!================!

    open (unit = input, file = trim(inp_file), status = 'old', action = 'read', iostat = ioer)

    if (ioer /= 0) then
      print*, 'Cannot open file'
    else
      if (verbose) print*,'success opening file'
    end if


!===================================================!
! Forever loop to determine length of file contents !
!===================================================!

    n = 0
    forever : do
      n = n + 1
      read(unit = input, fmt = *, iostat = ioer, end = 999) dummy
    end do forever
    
    999 nmax_file = n - 1 - 6 
    if (verbose) print*, nmax_file

!=================! 
! Go to beginning ! 
!=================!

    rewind(input)
    if (verbose) print*,'rewind file'


!==================================================================!
! Read first 6 lines as dummy (the first 6 lines are descriptions) !
!==================================================================!

    do n = 1, 6
      read(unit = input, fmt = *, iostat = ioer, end = 999) dummy
      if (verbose) print*,'dummy',dummy
    enddo

!=====================================================!
! Read in contents of each file and print (if needed) !
!=====================================================!

    file_read: do n = 1, nmax_file   
      read(unit = input, fmt = *, iostat = ioer, end = 999) sample(n)
      if (verbose) print*,sample(n)
        
    end do file_read
    
!    nmaxsz_arry = size(sample)
!    print*, nmaxsz_arry
!    stop
 
  end subroutine read_modfile

!FIRE EMISS FIRE EMISS FIRE EMISS FIRE EMISS FIRE EMISS FIRE EMISS FIRE EMISS FIRE EMISS FIRE EMISS 
!FIRE EMISS FIRE EMISS FIRE EMISS FIRE EMISS FIRE EMISS FIRE EMISS FIRE EMISS FIRE EMISS FIRE EMISS 

!===========================================================================!
! Subroutine define_modgrid (Define MODIS grid):                            !
! Define a lat/lon 1D array within the boundaries set and resolution wanted !
! then populate a 2D array with selected data in lat/lon specific grid      !
!===========================================================================!

  subroutine define_modgrid
  implicit none

!==============!
! Declarations !
!==============!
 
  integer :: i_max, j_max    ! latitude max and longitude max
  integer :: n               ! used for loop iterations
  integer :: j, k            ! lat/lon indexes determined from lat/lon values

!===============================================================!
! Define i_max, j_max from the domain boundaries and resolution !
!===============================================================!

  i_max = (max_lon - min_lon)/grid_res +1
  j_max = (max_lat - min_lat)/grid_res +1 

  if(verbose) print*, i_max,'   ',j_max

!==================================!
! Allocate frp, lat, & lon  arrays !
!==================================!

  allocate(frp(1:j_max, 1:i_max))
  
  allocate(lat(1:j_max))

  allocate(lon(1:i_max))
 
!=======================================================================================!
! Do loop to populate lat array at grid resolution intervals ending at defined boundary !
!=======================================================================================!

  n = 0
   init_lat: do n=1, j_max
     lat(n)= min_lat + grid_res*real(n) 
     if(verbose) print*,n,' ',lat(n)
  end do init_lat

!===============================================================================!
! Do loop to populate lon array at grid resolution intervals ending at boundary !
!===============================================================================!

  n=0
   init_lon: do n=1, i_max
     lon(n) = min_lon + grid_res*real(n)
     if(verbose) print*,n,' ',lon(n)
   end do init_lon
 
!===================================================================!
! Do loop to populate frp with frp data in correct lat/lon location !
!===================================================================!

  n=0
  k=0
  j=0
  frp_fill: do n=1, nmax_file
    if ((sample(n)%FP_latitude < min_lat) .or.(sample(n)%FP_latitude  > max_lat) .or. &
    (sample(n)%FP_longitude < min_lon) .or. (sample(n)%FP_longitude > max_lon)  ) then
      cycle
    else
      j  =  ((sample(n)%FP_latitude - min_lat) / grid_res)+1
      k =  ((sample(n)%FP_longitude - min_lon) / grid_res)+1
    end if 
    
    frp(j,k) = sample(n)%FP_power
    if(verbose) print*, sample(n)%Date, sample(n)%Time, j,' ', sample(n)%FP_latitude,' ',k,sample(n)%FP_longitude, &
    ' ',sample(n)%FP_power
   
  end do frp_fill
  end subroutine define_modgrid


!FIRE EMISS FIRE EMISS FIRE EMISS FIRE EMISS FIRE EMISS FIRE EMISS FIRE EMISS FIRE EMISS FIRE EMISS 
!FIRE EMISS FIRE EMISS FIRE EMISS FIRE EMISS FIRE EMISS FIRE EMISS FIRE EMISS FIRE EMISS FIRE EMISS 

!=============================================================================!
! Subroutine get_index (get index):                                           !
! A routine to test the expression used to take lat/lon and get an index that !
! matches the lat/lon grid index                                              !
!=============================================================================!

  subroutine get_index
  implicit none

!==============!
! Declarations !
!==============!

  integer :: n                          ! used for loop iterations
  integer :: lat_arry_sz, lon_arry_sz   ! size of lat array (index array)

!====================!
! Allocate statement !
!====================!

  allocate(lat_index(1:nmaxsz_arry))

  allocate(lon_index(1:nmaxsz_arry))

!====================================================!
! Do loop to get lat index numbers within boundaries !
!====================================================!

  n = 0
    lat_ind: do n=1, nmax_file
     if (verbose) print*, sample(n)%FP_latitude, 'new files'
      if ((sample(n)%FP_latitude < min_lat) .or.(sample(n)%FP_latitude  > max_lat)) then
        print*, n, '  Outside of domain, not added', sample(n)%FP_latitude 
        cycle 
      else
        lat_index(n) = ((sample(n)%FP_latitude - min_lat) / grid_res)+1
        print*, n,' ',lat_index(n),' ', sample(n)%FP_latitude   
      end if
    end do lat_ind

!====================================================!
! Do loop to get lon index numbers within boundaries !
!====================================================!

  n = 0
    lon_ind: do n=1, nmax_file
     if (verbose) print*, sample(n)%FP_latitude, 'new files'
      if ((sample(n)%FP_longitude < min_lon) .or.(sample(n)%FP_longitude  > max_lon)) then
        print*, n, '  Outside of domain, not added', sample(n)%FP_longitude
        cycle
      else
        lon_index(n) =  ((sample(n)%FP_longitude - min_lon) / 0.1)
        print*, n,' ',lon_index(n),' ', sample(n)%FP_longitude
      end if
    end do lon_ind

!=====================!
! Find size of arrays !
!=====================!
 
  lat_arry_sz = size(lat_index)
  if(verbose) print*, lat_arry_sz

! stop

  end subroutine get_index


!FIRE EMISS FIRE EMISS FIRE EMISS FIRE EMISS FIRE EMISS FIRE EMISS FIRE EMISS FIRE EMISS FIRE EMISS 
!FIRE EMISS FIRE EMISS FIRE EMISS FIRE EMISS FIRE EMISS FIRE EMISS FIRE EMISS FIRE EMISS FIRE EMISS 

!====================================================!
! Subroutine exec_mod (Execute MODIS):          
!
!====================================================!
 
  subroutine exec_mod
  implicit none

  end subroutine exec_mod

!FIRE EMISS FIRE EMISS FIRE EMISS FIRE EMISS FIRE EMISS FIRE EMISS FIRE EMISS FIRE EMISS FIRE EMISS 
!FIRE EMISS FIRE EMISS FIRE EMISS FIRE EMISS FIRE EMISS FIRE EMISS FIRE EMISS FIRE EMISS FIRE EMISS 

 end module module_femission

!FIRE EMISS FIRE EMISS FIRE EMISS FIRE EMISS FIRE EMISS FIRE EMISS FIRE EMISS FIRE EMISS FIRE EMISS 
!FIRE EMISS FIRE EMISS FIRE EMISS FIRE EMISS FIRE EMISS FIRE EMISS FIRE EMISS FIRE EMISS FIRE EMISS 


!===================================!
! Main program:                     !
! The subroutines needed are called !
!===================================!

 program main
  use module_femission
  implicit none

!==============!
! Declarations !
!==============!

  integer :: n


!======================================================!
! Select case for either MODIS, SEVIRI, OR BOTH to run !
!======================================================!

 select case(trim(process) ) 
 case('MODIS')

   ! initialize file name
   call init_modfile
   
   ! allocate memory
   call allocate_mod

  
   ! read through contents of each file
   read_proc: do n=1, nmax_file   
     
     call read_modfile ( emisfile(n) )
  

   end do read_proc

     ! grid data
     call define_modgrid
  
     ! get indexes for lat/lon from file data
     !call get_index 

     call exec_mod

 case('SEVIRI') 

   ! initialize file name   
   call init_sevfile

   ! allocate memory
   call allocate_sev
     
   ! grid data 
   call define_sevgrid

   ! read through contents of each file
   read_sev_data: do n=1, nmax_sevfile
 
     call read_sevfile ( sevfile(n) )
  
     call pop_sevgrid ( sevfile(n) )

   end do read_sev_data


   call exec_sev


 case('BOTH') 
   
   ! initialize SEVIRI and MODIS file names
   call init_sevfile
   call init_modfile

   ! allocate memory
   call allocate_sev
   call allocate_mod

   ! read through contents of each file
   read_proc_sev: do n=1, nmax_sevfile

     call read_sevfile ( sevfile(n) )
     if (verbose) print*, 'success',n
   end do read_proc_sev
   
   ! read through contents of each file
   read_proc_mod: do n=1, nmax_file

     call read_modfile ( emisfile(n) )
     if (verbose) print*, 'success',n
   end do read_proc_mod

   ! grid data
   
   call define_sevgrid   
   call define_modgrid


 case default
   stop 'wrong name of process'

 end select



 end program main

!FIRE EMISS FIRE EMISS FIRE EMISS FIRE EMISS FIRE EMISS FIRE EMISS FIRE EMISS FIRE EMISS FIRE EMISS 
!FIRE EMISS FIRE EMISS FIRE EMISS FIRE EMISS FIRE EMISS FIRE EMISS FIRE EMISS FIRE EMISS FIRE EMISS 
!FIRE EMISS FIRE EMISS FIRE EMISS FIRE EMISS FIRE EMISS FIRE EMISS FIRE EMISS FIRE EMISS FIRE EMISS
