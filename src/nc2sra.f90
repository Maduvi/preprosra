PROGRAM nc2sra
  !-------------------------------------------------------------------
  !  This is the main interface for the f90 preprocessor designed to
  !  convert nc files to .SRA input files for PlaSim. This
  !  program will read a namelist file with specifications for the file 
  !  to preprocess and convert from .NC to .SRA.
  !
  !  USES:
  !        IOmod -> input/output subroutines module.
  !        CODEmat -> chooses lname,sname and units from kcode
  !
  !  Created: Mateo Duque Villegas
  !  Last updated: 17-Nov-2017
  !
  !-------------------------------------------------------------------
  USE IOmod
  USE CODEmat

  IMPLICIT NONE

  CHARACTER(LEN=100) :: root     ! Dir where .NC is
  CHARACTER(LEN=100) :: nc       ! .NC file name
  CHARACTER(LEN=100) :: ncfile   ! .NC whole file name
  CHARACTER(LEN=100) :: inpnml   ! Input file name
  CHARACTER(LEN=100) :: srafile  ! Output .SRA file
  INTEGER(KIND=4)    :: kcode    ! ECHAM code
  INTEGER(KIND=4)    :: unit     ! Namelist unit
  INTEGER(KIND=4)    :: nmon     ! Number of months in file
  INTEGER(KIND=4)    :: nlon     ! Number of longitudes
  INTEGER(KIND=4)    :: nlat     ! Number of latitudes
  INTEGER(KIND=4)    :: hcols    ! Number of cols in hdr
  INTEGER(KIND=4)    :: dcols    ! Number of cols in data matrix
  INTEGER(KIND=4)    :: operr    ! Open error variable
  INTEGER(KIND=4)    :: rderr    ! Read error variable
  INTEGER(KIND=4), ALLOCATABLE, DIMENSION(:,:)   :: ihead ! headrs arr
  INTEGER(KIND=4), ALLOCATABLE, DIMENSION(:)     :: vtime ! times arr
  REAL(KIND=8),    ALLOCATABLE, DIMENSION(:,:,:) :: data  ! data rr
  REAL(KIND=8),    ALLOCATABLE, DIMENSION(:)     :: lat   ! lats arr
  REAL(KIND=8),    ALLOCATABLE, DIMENSION(:)     :: lon   ! longs arr
  
  NAMELIST /INPUT_INFO/ root, nc, kcode, hcols, dcols

  ! Read namelist file name with input variables from command line
  CALL GET_COMMAND_ARGUMENT(1, inpnml)

  ! Open namelist file to get variables and check for error
  OPEN (unit=1, file=TRIM(inpnml),status='old',iostat=operr)
  IF(operr>0) THEN
     WRITE(*,'(A)') "nc2sra: error: could not open namelist file."
     CALL EXIT(0)
  END iF

  READ (1,nml=INPUT_INFO,iostat=rderr)
  ! Read namelist file and check for error
  IF(rderr>0) THEN
     WRITE(*,'(A)') "nc2sra: error: could not read namelist file."
  END IF

  ! .NC whole file name.
  ncfile = TRIM(ADJUSTL(root))//"/"//TRIM(ADJUSTL(nc))
  
  ! Get dimensions of netCDF
  CALL getdims(ncfile,nmon,nlon,nlat)

  ! Allocate memory
  ALLOCATE(lat(nlat))
  ALLOCATE(lon(nlon))  
  ALLOCATE(ihead(hcols,nmon))
  ALLOCATE(data(nlon,nlat,nmon))
  ALLOCATE(vtime(nmon))

  ! Read netCDF variables
  CALL ncread(ncfile,nmon,nlon,nlat,vtime,lon,lat,data)  
  
  ! Generate .SRA headers
  CALL genheadr(kcode,hcols,nmon,nlon,nlat,ihead)

  ! .NC file name plus .sra extension.
  srafile = "./"//TRIM(ADJUSTL(nc))//".sra"

  ! Create SRA file using headers and dcols
  CALL  sragen(srafile,kcode,hcols,dcols,nmon,nlon,nlat,ihead,data)  

END PROGRAM nc2sra
