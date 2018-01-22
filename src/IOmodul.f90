MODULE IOmod
  !-------------------------------------------------------------------
  !
  !  Module containing definitions of subroutines needed to read
  !  and write files.
  !
  !  USES:
  !            netcdf -> netCDF-Fortran lib
  !            CODEmat -> chooses lname,sname and units from kcode
  !
  !  CONTAINS: 
  !            sraReader -> reads .SRA files
  !            check     -> checks for errors in netCDF lib funcs
  !            ncgen     -> creates netCDF files from SRA files
  !            getdims   -> reads netCDF and gets dims
  !            ncread    -> reads netCDF files
  !            sragen    -> creates SRA files from netCDF files
  !
  !  Created: Mateo Duque Villegas
  !  Last updated: 17-Nov-2017
  !
  !-------------------------------------------------------------------
  IMPLICIT NONE

CONTAINS

  SUBROUTINE sraReader(srafile,nmon,nlon,nlat,hcols,dcols&
       &,ihead,data)

    IMPLICIT NONE

    ! Global namespace. Var description in main program.
    CHARACTER(LEN=*), INTENT(in)  :: srafile
    INTEGER(KIND=4),  INTENT(in)  :: nmon
    INTEGER(KIND=4),  INTENT(in)  :: nlon
    INTEGER(KIND=4),  INTENT(in)  :: nlat
    INTEGER(KIND=4),  INTENT(in)  :: hcols
    INTEGER(KIND=4),  INTENT(in)  :: dcols
    INTEGER(KIND=4), DIMENSION(hcols,nmon),     INTENT(out) :: ihead
    REAL(KIND=8),    DIMENSION(nlon,nlat,nmon), INTENT(out) :: data

    ! Local namespace.
    INTEGER(KIND=4) :: unit = 11  ! Reading unit
    INTEGER(KIND=4) :: i          ! Counter
    INTEGER(KIND=4) :: operr      ! Error variable
    INTEGER(KIND=4) :: rderr      ! Error variable

    ! Open .SRA file and check for problems.
    OPEN(unit,file=srafile,form='FORMATTED',status='old',action='read'&
         &,iostat=operr)
    IF(operr>0) THEN
       WRITE(*,'(A)') "sraReader: error: could not open .sra file."
       CALL EXIT(0)
    END IF

    DO i = 1,nmon
       ! Read header info and check for problems.
       READ(unit,'(8I10)',iostat=rderr) ihead(:,i)
       IF(rderr>0) THEN
          WRITE(*,'(A)') "sraReader: error: could not read .sra file."
          CALL EXIT(0)
       END IF
       ! Because formatting is different in some .SRA files
       IF (dcols == 4) THEN
          READ(unit,'(4E16.6)',iostat=rderr) data(:,:,i)
          IF(rderr>0) THEN
             WRITE(*,'(A)') "sraReader: error: wrong reading format."
             CALL EXIT(0)
          END IF
       ELSE IF (dcols == 8) THEN
          READ(unit,'(8F10.8)',iostat=rderr) data(:,:,i)
          IF(rderr>0) THEN
             WRITE(*,'(A)') "sraReader: error: wrong reading format."
             CALL EXIT(0)
          END IF      
       ELSE
          WRITE(*,'(A)') "IOmodul: error: unknown reading format."
          CALL EXIT(0)
       END IF
    END DO
    
    CLOSE(unit)

    RETURN
  END SUBROUTINE sraReader

  SUBROUTINE check(istatus)
    
    USE netcdf
    
    IMPLICIT NONE
    
    INTEGER(KIND=4), INTENT (IN) :: istatus

    ! Check for errors everytime run netcdf library stuff
    IF (istatus /= nf90_noerr) THEN   
       WRITE(*,*) TRIM(ADJUSTL(nf90_strerror(istatus)))
    END IF

    RETURN
  END SUBROUTINE check

  SUBROUTINE getdims(ncfile,nmon,nlon,nlat)

    USE netcdf

    IMPLICIT NONE

    ! Global namespace
    CHARACTER(LEN=*), INTENT(IN)  :: ncfile
    INTEGER(KIND=4),  INTENT(OUT) :: nmon
    INTEGER(KIND=4),  INTENT(OUT) :: nlon
    INTEGER(KIND=4),  INTENT(OUT) :: nlat

    ! Local namespace
    CHARACTER(LEN=50) :: tname
    CHARACTER(LEN=50) :: xname
    CHARACTER(LEN=50) :: yname
    INTEGER(KIND=4)   :: ncid

    ! Open netCDF and read
    CALL check(nf90_open(ncfile,nf90_nowrite,ncid))

    ! Inquire for dimensions
    CALL check(nf90_inquire_dimension(ncid,1,tname,nmon))
    CALL check(nf90_inquire_dimension(ncid,2,xname,nlon))
    CALL check(nf90_inquire_dimension(ncid,3,yname,nlat))

    ! Close netCDF file
    CALL check(nf90_close(ncid))

    RETURN
  END SUBROUTINE getdims
  
  SUBROUTINE ncread(ncfile,nmon,nlon,nlat,vtime,lon,lat,data)

    USE netcdf

    IMPLICIT NONE

    ! Global namespace
    CHARACTER(LEN=*), INTENT(IN) :: ncfile
    INTEGER(KIND=4),  INTENT(IN) :: nmon
    INTEGER(KIND=4),  INTENT(IN) :: nlon
    INTEGER(KIND=4),  INTENT(IN) :: nlat
    INTEGER(KIND=4), DIMENSION(nmon),           INTENT(OUT) :: vtime
    REAL(KIND=8),    DIMENSION(nlon),           INTENT(OUT) :: lon
    REAL(KIND=8),    DIMENSION(nlat),           INTENT(OUT) :: lat
    REAL(KIND=8),    DIMENSION(nlon,nlat,nmon), INTENT(OUT) :: data

    ! Local namespace
    CHARACTER(LEN=50) :: tname
    CHARACTER(LEN=50) :: xname
    CHARACTER(LEN=50) :: yname
    CHARACTER(LEN=50) :: vname
    INTEGER(KIND=4)   :: ncid
    INTEGER(KIND=4)   :: type
    INTEGER(KIND=4)   :: ndims
    INTEGER(KIND=4)   :: varid
    INTEGER(KIND=4), DIMENSION(3) :: dimids

    ! Open netcdf
    CALL check(nf90_open(ncfile,nf90_nowrite,ncid))

    ! Get values for time coordinate
    CALL check(nf90_inquire_variable(ncid,1,vname,type,ndims,dimids))
    CALL check(nf90_inq_varid(ncid,vname,varid))
    CALL check(nf90_get_var(ncid,varid,vtime))

    ! Get values for x coordinate
    CALL check(nf90_inquire_variable(ncid,2,vname,type,ndims,dimids))
    CALL check(nf90_inq_varid(ncid,vname,varid))
    CALL check(nf90_get_var(ncid,varid,lon))

    ! Get values for y coordinate
    CALL check(nf90_inquire_variable(ncid,3,vname,type,ndims,dimids))
    CALL check(nf90_inq_varid(ncid,vname,varid))
    CALL check(nf90_get_var(ncid,varid,lat))
    
    ! Get values for data
    CALL check(nf90_inquire_variable(ncid,4,vname,type,ndims,dimids))
    CALL check(nf90_inq_varid(ncid,vname,varid))
    CALL check(nf90_get_var(ncid,varid,data))
    
    RETURN
  END SUBROUTINE ncread
  
  SUBROUTINE ncgen(ncfile,kcode,vtime,lon,lat,data,nmon,nlon,nlat)

    USE CODEmat
    USE netcdf
    
    IMPLICIT NONE

    ! Global namespace. Var description in main program
    CHARACTER(LEN=*), INTENT(IN) :: ncfile
    INTEGER(KIND=4),  INTENT(IN) :: kcode
    INTEGER(KIND=4),  INTENT(IN) :: nmon
    INTEGER(KIND=4),  INTENT(IN) :: nlon
    INTEGER(KIND=4),  INTENT(IN) :: nlat
    INTEGER(KIND=4), DIMENSION(nmon),           INTENT(IN) :: vtime
    REAL(KIND=8),    DIMENSION(nlon),           INTENT(IN) :: lon
    REAL(KIND=8),    DIMENSION(nlat),           INTENT(IN) :: lat
    REAL(KIND=8),    DIMENSION(nlon,nlat,nmon), INTENT(IN) :: data

    ! Local namespace
    CHARACTER(LEN=50) :: lname
    CHARACTER(LEN=50) :: timestr
    CHARACTER(LEN=15) :: units
    CHARACTER(LEN=10) :: time
    CHARACTER(LEN=8)  :: date  
    CHARACTER(LEN=4)  :: sname
        
    ! NetCDF stuff
    INTEGER(KIND=4) :: ncid     ! Unit ID for netCDF
    INTEGER(KIND=4) :: t_dimid  ! ID for time dimension
    INTEGER(KIND=4) :: x_dimid  ! ID for X dimension
    INTEGER(KIND=4) :: y_dimid  ! ID for Y dimension
    INTEGER(KIND=4) :: varid    ! ID for data variable
    INTEGER(KIND=4) :: t_varid  ! ID for time variable
    INTEGER(KIND=4) :: x_varid  ! ID for X variable
    INTEGER(KIND=4) :: y_varid  ! ID for Y variable
    INTEGER(KIND=4), DIMENSION(3) :: dimids  ! ID for all dimensions
    
    ! Create the netCDF file and assign unit.
    CALL check(nf90_create(ncfile, NF90_CLOBBER, ncid))

    ! Define the dimensions numbers.
    CALL check(nf90_def_dim(ncid, "time", 0, t_dimid))
    CALL check(nf90_def_dim(ncid, "lon", nlon, x_dimid))
    CALL check(nf90_def_dim(ncid, "lat", nlat, y_dimid))

    ! Define coordinate variable LON and attributes
    CALL check(nf90_def_var(ncid, "time", NF90_INT, t_dimid,&
         & t_varid))
    CALL check(nf90_put_att(ncid, t_varid, "standard_name","time"))
    CALL check(nf90_put_att(ncid, t_varid, "units", "months since 001&
         &-01-01 12:00:00"))
    CALL check(nf90_put_att(ncid, t_varid, "calendar","360_day"))
    CALL check(nf90_put_att(ncid, t_varid, "axis","T"))

    ! Define coordinate variable LON and attributes
    CALL check(nf90_def_var(ncid, "lon", NF90_REAL, x_dimid, x_varid))
    CALL check(nf90_put_att(ncid, x_varid, "standard_name", "longitude"))
    CALL check(nf90_put_att(ncid, x_varid, "long_name", "longitude"))
    CALL check(nf90_put_att(ncid, x_varid, "units", "degrees_east"))
    CALL check(nf90_put_att(ncid, x_varid, "axis", "X"))
    
    ! Define coordinate variable LAT and attributes
    CALL check(nf90_def_var(ncid, "lat", NF90_REAL, y_dimid, y_varid))
    CALL check(nf90_put_att(ncid, y_varid, "standard_name", "latitude"))
    CALL check(nf90_put_att(ncid, y_varid, "long_name", "latitude"))
    CALL check(nf90_put_att(ncid, y_varid, "units", "degrees_north"))
    CALL check(nf90_put_att(ncid, y_varid, "axis", "Y"))

    ! Dims for data array
    dimids = (/ x_dimid, y_dimid, t_dimid /)

    ! Get stuff for variable attributes
    CALL kcoder(kcode,lname,sname,units)
    
    ! Define variable
    CALL check(nf90_def_var(ncid, sname, NF90_DOUBLE, dimids&
         &, varid))
    CALL check(nf90_put_att(ncid, varid, "standard_name", lname))
    CALL check(nf90_put_att(ncid, varid, "long_name", sname))
    CALL check(nf90_put_att(ncid, varid, "units", units))
    CALL check(nf90_put_att(ncid, varid, "code", kcode))
    CALL check(nf90_put_att(ncid, varid, "grid_type", "gaussian"))

    ! Global attributes
    CALL DATE_AND_TIME(date,time)
    
    timestr = "Created on " //date(1:4) //"-"//date(5:6)//"-"//&
         &date(7:8)//" "//time(1:2)//":"//time(3:4)//":"//time(5:6)
    
    CALL check(nf90_put_att(ncid,NF90_GLOBAL, "history", timestr))
    CALL check(nf90_put_att(ncid,NF90_GLOBAL, "Conventions", "CF&
         &-1.0"))
    
    ! End definitions
    CALL check(nf90_enddef(ncid))
    
    ! Write Data
    CALL check(nf90_put_var(ncid, t_varid, vtime))
    CALL check(nf90_put_var(ncid, x_varid, lon))
    CALL check(nf90_put_var(ncid, y_varid, lat))
    CALL check(nf90_put_var(ncid, varid, data))
    CALL check(nf90_close(ncid))

    RETURN    
  END SUBROUTINE ncgen

  SUBROUTINE sragen(srafile,kcode,hcols,dcols,nmon,nlon,nlat,ihead&
       &,data)

    IMPLICIT NONE

    ! Global namespace
    CHARACTER(LEN=*), INTENT(IN) :: srafile
    INTEGER(KIND=4),  INTENT(IN) :: kcode
    INTEGER(KIND=4),  INTENT(IN) :: hcols
    INTEGER(KIND=4),  INTENT(IN) :: dcols
    INTEGER(KIND=4),  INTENT(IN) :: nmon
    INTEGER(KIND=4),  INTENT(IN) :: nlon
    INTEGER(KIND=4),  INTENT(IN) :: nlat
    INTEGER(KIND=4), DIMENSION(hcols,nmon),     INTENT(IN) :: ihead
    REAL(KIND=8),    DIMENSION(nlon,nlat,nmon), INTENT(IN) :: data

    ! Local namespace
    INTEGER(KIND=4) :: i
    INTEGER(KIND=4) :: operr
    INTEGER(KIND=4) :: unit = 11

    ! Open .SRA file
    OPEN(unit,file=srafile,form='FORMATTED',action='write',iostat&
         &=operr)
    IF(operr>0) THEN
       WRITE(*,'(A)') "sragen: error: could not create .sra file."
       CALL EXIT(0)
    END IF
    
    ! Write for every month
    DO i = 1,nmon
       WRITE(unit,'(8I10)') ihead(:,i)
       IF (dcols == 4) THEN
          WRITE(unit,'(4E16.6)') data(:,:,i)
       ELSE IF (dcols == 8) THEN
          ! LSmask is the only with 6 decimal places
          IF (kcode == 172) THEN
             WRITE(unit,'(8F10.6)') data(:,:,i)
          ELSE
             WRITE(unit,'(8F10.3)') data(:,:,i)
          END IF
       END IF
    END DO

    RETURN
  END SUBROUTINE sragen
  
END MODULE IOmod
