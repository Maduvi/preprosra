PROGRAM sra2nc
  !-------------------------------------------------------------------
  !  This is the main interface for the f90 preprocessor designed to
  !  deal with .SRA files that are the "dat" component of PlaSim. This
  !  program will read an input file with specifications for the file 
  !  to preprocess and convert from .SRA to .NC.
  !
  !  USES:
  !        IOmod -> input/output subroutines module (uses netcdf).
  !        CODEmat -> big array w var attributes and latlon array
  !
  !  Created: Mateo Duque Villegas
  !  Last updated: 16-Nov-2017
  !
  !-------------------------------------------------------------------
  USE IOmod
  USE CODEmat

  IMPLICIT NONE

  CHARACTER(LEN=100) :: root    ! Dir where .SRA is
  CHARACTER(LEN=100) :: sra     ! .SRA file name
  CHARACTER(LEN=100) :: srafile ! .SRA whole file name
  CHARACTER(LEN=100) :: inpnml  ! Input file name
  CHARACTER(LEN=100) :: ncfile  ! Output nc file
  INTEGER(KIND=4)    :: kcode   ! ECHAM code
  INTEGER(KIND=4)    :: unit    ! Namelist unit
  INTEGER(KIND=4)    :: i       ! Counter
  INTEGER(KIND=4)    :: nmon    ! Number of months in file
  INTEGER(KIND=4)    :: nlon    ! Number of longitudes
  INTEGER(KIND=4)    :: nlat    ! Number of latitudes
  INTEGER(KIND=4)    :: hcols   ! Number of cols in hdr
  INTEGER(KIND=4)    :: dcols   ! Number of cols in data matrix
  INTEGER(KIND=4)    :: operr   ! Error variable
  INTEGER(KIND=4)    :: rperr   ! Error variable
  INTEGER(KIND=4), ALLOCATABLE, DIMENSION(:,:)   :: ihead ! headrs arr
  INTEGER(KIND=4), ALLOCATABLE, DIMENSION(:)     :: vtime ! times arr
  REAL(KIND=8),    ALLOCATABLE, DIMENSION(:,:,:) :: data  ! data arr
  REAL(KIND=8),    ALLOCATABLE, DIMENSION(:)     :: lat   ! lats arr
  REAL(KIND=8),    ALLOCATABLE, DIMENSION(:)     :: lon   ! longs arr
  
  NAMELIST /INPUT_INFO/ root, sra, kcode, nlat, nlon, hcols, dcols,&
       & nmon 

  ! Read input file with variable definitions
  inpnml  = "./sra2nc_namelist"
  OPEN (unit=1, file=inpnml)
  READ (1,nml=INPUT_INFO)
  
  ! Allocate memory
  ALLOCATE(lat(nlat))
  ALLOCATE(lon(nlon))  
  ALLOCATE(ihead(hcols,nmon))
  ALLOCATE(data(nlon,nlat,nmon))
  ALLOCATE(vtime(nmon))

  ! .SRA file name.
  srafile = TRIM(ADJUSTL(root))//"/"//TRIM(ADJUSTL(sra))
  
  ! Read info in .SRA and save in ihead and data
  CALL sraReader(srafile,nmon,nlon,nlat,hcols,dcols,ihead,data)

  ! Create time vector
  IF (nmon == 1) THEN
     vtime = (/0/)
  ELSE
     vtime = (/-1,0,1,2,3,4,5,6,7,8,9,10,11,12/)
  END IF
  
  ! Get lats and lons (fixed, not computed)
  CALL latlons(nlat,nlon,lat,lon)
  
  ! .SRA file name plus .CDL extension.
  ncfile = "./"//TRIM(ADJUSTL(sra))//".nc"

  ! Create ncfile using ncgen from IOmod
  CALL ncgen(ncfile,kcode,vtime,lon,lat,data,nmon,nlon,nlat)     
          
  DEALLOCATE(lon)
  DEALLOCATE(lat)
  DEALLOCATE(data)
  DEALLOCATE(ihead)
  DEALLOCATE(vtime)  

END PROGRAM sra2nc
