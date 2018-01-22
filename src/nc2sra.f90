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
  INTEGER(KIND=4)    :: i        ! Loop counter
  INTEGER(KIND=4)    :: kcode    ! ECHAM code
  INTEGER(KIND=4)    :: unit     ! Namelist unit
  INTEGER(KIND=4)    :: ndims    ! Number of dimensions in netCDF
  INTEGER(KIND=4)    :: nvars    ! Number of variables in netCDF
  INTEGER(KIND=4)    :: nmon     ! Number of months in file
  INTEGER(KIND=4)    :: nlon     ! Number of longitudes
  INTEGER(KIND=4)    :: nlat     ! Number of latitudes
  INTEGER(KIND=4)    :: hcols    ! Number of cols in hdr
  INTEGER(KIND=4)    :: dcols    ! Number of cols in data matrix
  INTEGER(KIND=4)    :: operr    ! Open error variable
  INTEGER(KIND=4)    :: rderr    ! Read error variable
  CHARACTER(LEN=50), DIMENSION(3) :: namearr  ! Name dimension in netCDF
  INTEGER(KIND=4),   DIMENSION(3) :: dimarr   ! Dimension in netCDF
  INTEGER(KIND=4), ALLOCATABLE, DIMENSION(:,:)   :: ihead ! headrs arr
  INTEGER(KIND=4), ALLOCATABLE, DIMENSION(:)     :: vtime ! times arr
  REAL(KIND=8),    ALLOCATABLE, DIMENSION(:,:,:) :: data  ! data rr
  REAL(KIND=8),    ALLOCATABLE, DIMENSION(:,:,:) :: psac  ! Plasim ACyc
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
     CALL EXIT(0)
  END IF

  ! .NC whole file name.
  ncfile = TRIM(ADJUSTL(root))//"/"//TRIM(ADJUSTL(nc))
  
  ! Get dimensions of netCDF
  CALL getdims(ncfile,ndims,nvars,namearr,dimarr)

  DO i=1,ndims
     IF(namearr(i) == "time") THEN
        nmon = dimarr(i)
     ELSE IF(namearr(i) == "lon" .OR. namearr(i) == "longitude") THEN
        nlon = dimarr(i)
     ELSE IF(namearr(i) == "lat" .OR. namearr(i) == "latitude") THEN
        nlat = dimarr(i)
     ELSE
        WRITE(*,'(A)') "nc2sra: error: unknown dimensions in netCDF."
        CALL EXIT(0)
     END IF
  END DO

  ! Allocate memory
  ALLOCATE(lat(nlat))
  ALLOCATE(lon(nlon))
  ALLOCATE(data(nlon,nlat,nmon))
  ALLOCATE(vtime(nmon))

  ! Read netCDF variables
  CALL ncread(ncfile,ndims,nvars,nmon,nlon,nlat,vtime,lon,lat,data)  

  ! .NC file name plus .sra extension.
  srafile = "./"//TRIM(ADJUSTL(nc))//".sra"

  IF(nmon == 1 .OR. nmon == 14) THEN
     
     ALLOCATE(ihead(hcols,nmon))
     
     ! Generate .SRA headers
     CALL genheadr(kcode,hcols,nmon,nlon,nlat,ihead)

     ! Create SRA file using headers and dcols depending on whether 
     CALL sragen(srafile,kcode,hcols,dcols,nmon,nlon,nlat,ihead,data)
  ELSE IF(nmon == 12) THEN

     ALLOCATE(psac(nlon,nlat,14))
     ALLOCATE(ihead(hcols,14))

     ! Create array with 14 months
     psac(:,:,1) = data(:,:,12)
     psac(:,:,2:13) = data(:,:,:)
     psac(:,:,14) = data(:,:,1)
     
     ! Generate .SRA headers
     CALL genheadr(kcode,hcols,14,nlon,nlat,ihead)

     ! Create SRA file using headers and dcols depending on whether 
     CALL sragen(srafile,kcode,hcols,dcols,14,nlon,nlat,ihead,psac)
  ELSE
     WRITE(*,'(A)') "nc2sra: error: incorrect number of timesteps in nc."
     CALL EXIT(0)
  END IF
  
  ! Free memory
  DEALLOCATE(lon)
  DEALLOCATE(lat)
  DEALLOCATE(vtime)
  DEALLOCATE(data)
  DEALLOCATE(psac)
  DEALLOCATE(ihead)
  
END PROGRAM nc2sra
