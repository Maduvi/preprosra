MODULE CODEmat
  !-------------------------------------------------------------------
  !
  !  Module containing a subroutine that creates a big array with
  !  shortnames, longnames and units for common PlaSim variables.
  !  
  !  CONTAINS:
  !           kcoder -> chooses lname,sname,units from kcode
  !           genhdr -> creates headers for SRA files
  !           latlons -> gives lat and lot arrays got elsewhere
  !
  !  Created: Mateo Duque Villegas
  !  Last updated: 17-Nov-2017
  !
  !-------------------------------------------------------------------
  IMPLICIT NONE

CONTAINS

  SUBROUTINE kcoder(kcode,lname,sname,units)

    IMPLICIT NONE

    ! Global namespace
    INTEGER(KIND=4),  INTENT(IN)  :: kcode
    CHARACTER(LEN=*), INTENT(OUT) :: lname
    CHARACTER(LEN=*), INTENT(OUT) :: sname
    CHARACTER(LEN=*), INTENT(OUT) :: units

    IF (kcode == 110) THEN
       lname = "Mixed Layer Depth"
       sname = "mld"
       units = "m"
    ELSE IF (kcode == 129) THEN
       lname = "Surface Geopotential Orography"
       sname = "sg"
       units = "m2/s2"
    ELSE IF (kcode == 130) THEN
       lname = "Temperature"
       sname = "ta"
       units = "K"
    ELSE IF (kcode == 131) THEN
       lname = "Zonal Wind"
       sname = "ua"
       units = "m/s"
    ELSE IF (kcode == 132) THEN
       lname = "Meridional Wind"
       sname = "va"
       units = "m/s"
    ELSE IF (kcode == 133) THEN
       lname = "Specific Humidity"
       sname = "hus"
       units = "kg/kg"
    ELSE IF (kcode == 134) THEN
       lname = "Surface Pressure"
       sname = "ps"
       units = "hPa"
    ELSE IF (kcode == 135) THEN
       lname = "Vertical Velocity"
       sname = "wap"
       units = "Pa/s"
    ELSE IF (kcode == 137) THEN
       lname = "Vertical Wind"
       sname = "wa"
       units = "m/s"
    ELSE IF (kcode == 138) THEN
       lname = "Vorticity"
       sname = "zeta"
       units = "1/s"
    ELSE IF (kcode == 139) THEN
       lname = "Surface Temperature"
       sname = "ts"
       units = "K"
    ELSE IF (kcode == 140) THEN
       lname = "Soil Wetness"
       sname = "mrso"
       units = "m"
    ELSE IF (kcode == 141) THEN
       lname = "Snow Depth"
       sname = "snd"
       units = "m"
    ELSE IF (kcode == 142) THEN
       lname = "Large Scale Precipitation"
       sname = "prl"
       units = "m/s"
    ELSE IF (kcode == 143) THEN
       lname = "Convective Precipitation"
       sname = "prc"
       units = "m/s"
    ELSE IF (kcode == 144) THEN
       lname = "Snow Fall"
       sname = "prsn"
       units = "m/s"
    ELSE IF (kcode == 145) THEN
       lname = "Boundary Layer Dissipation"
       sname = "bld"
       units = "W/m2"
    ELSE IF (kcode == 146) THEN
       lname = "Surface Sensible Heat Flux"
       sname = "hfss"
       units = "W/m2"
    ELSE IF (kcode == 147) THEN
       lname = "Surface Latent Heat Flux"
       sname = "hfls"
       units = "W/m2"
    ELSE IF (kcode == 148) THEN
       lname = "Streamfunction"
       sname = "stf"
       units = "m2/s"
    ELSE IF (kcode == 149) THEN
       lname = "Velocity Potential"
       sname = "psi"
       units = "m2/s"
    ELSE IF (kcode == 151) THEN
       lname = "Mean Sea Level Pressure"
       sname = "psl"
       units = "hPa"
    ELSE IF (kcode == 152) THEN
       lname = "Log Surface Pressure"
       sname = "pl"
       units = "1"
    ELSE IF (kcode == 155) THEN
       lname = "Divergence"
       sname = "d"
       units = "1/s"
    ELSE IF (kcode == 156) THEN
       lname = "Geopotential Height"
       sname = "zg"
       units = "gpm"
    ELSE IF (kcode == 157) THEN
       lname = "Relative Humidity"
       sname = "hur"
       units = "%"
    ELSE IF (kcode == 158) THEN
       lname = "Tendency of Surface Pressure"
       sname = "tps"
       units = "Pa/s"
    ELSE IF (kcode == 159) THEN
       lname = "ustar**3"
       sname = "u3"
       units = "m3/s3"
    ELSE IF (kcode == 160) THEN
       lname = "Surface Runoff"
       sname = "mrro"
       units = "m/s"
    ELSE IF (kcode == 161) THEN
       lname = "Liquid Water Content"
       sname = "clw"
       units = "kg/kg"
    ELSE IF (kcode == 162) THEN
       lname = "Cloud Cover"
       sname = "cl"
       units = "1"
    ELSE IF (kcode == 163) THEN
       lname = "Total Cloud Cover"
       sname = "tcc"
       units = "1"
    ELSE IF (kcode == 164) THEN
       lname = "Total Cloud Cover (Mean)"
       sname = "clt"
       units = "1"
    ELSE IF (kcode == 165) THEN
       lname = "Eastward Wind 10m"
       sname = "uas"
       units = "m/s"
    ELSE IF (kcode == 166) THEN
       lname = "Northward Wind 10m"
       sname = "vas"
       units = "m/s"
    ELSE IF (kcode == 167) THEN
       lname = "2m Temperature"
       sname = "tas"
       units = "K"
    ELSE IF (kcode == 168) THEN
       lname = "2m Dew Point Temperature"
       sname = "td2m"
       units = "K"
    ELSE IF (kcode == 169) THEN
       lname = "Surface Temperature Accumulated"
       sname = "tsa"
       units = "K"
    ELSE IF (kcode == 170) THEN
       lname = "Deep Soil Temperature"
       sname = "tsod"
       units = "K"
    ELSE IF (kcode == 171) THEN
       lname = "Deep Soil Wetness"
       sname = "dsw"
       units = "1"
    ELSE IF (kcode == 172) THEN
       lname = "Land Sea Mask"
       sname = "lsm"
       units = "1"
    ELSE IF (kcode == 173) THEN
       lname = "Surface Roughness"
       sname = "z0"
       units = "m"
    ELSE IF (kcode == 174) THEN
       lname = "Surface Albedo"
       sname = "alb"
       units = "1"
    ELSE IF (kcode == 176) THEN
       lname = "Surface Solar Radiation"
       sname = "ssr"
       units = "W/m2"
    ELSE IF (kcode == 177) THEN
       lname = "Surface Thermal Radiation"
       sname = "rss"
       units = "W/m2"
    ELSE IF (kcode == 178) THEN
       lname = "Top Solar Radiation"
       sname = "rst"
       units = "W/m2"
    ELSE IF (kcode == 179) THEN
       lname = "Top Thermal Radiation"
       sname = "rlut"
       units = "W/m2"
    ELSE IF (kcode == 180) THEN
       lname = "U-Stress"
       sname = "tauu"
       units = "Pa"
    ELSE IF (kcode == 181) THEN
       lname = "V-Stress"
       sname = "tauv"
       units = "Pa"
    ELSE IF (kcode == 182) THEN
       lname = "Evaporation"
       sname = "evap"
       units = "m/s"
    ELSE IF (kcode == 183) THEN
       lname = "Soil Temperature"
       sname = "tso"
       units = "K"
    ELSE IF (kcode == 184) THEN
       lname = "Soil Wetness"
       sname = "wsoi"
       units = "1"
    ELSE IF (kcode == 199) THEN
       lname = "Vegetation Cover"
       sname = "vegc"
       units = "1"
    ELSE IF (kcode == 200) THEN
       lname = "Leaf Area Index"
       sname = "dlai"
       units = "1"
    ELSE IF (kcode == 203) THEN
       lname = "Tp Solar Radiation Upward"
       sname = "rsdt"
       units = "W/m2"
    ELSE IF (kcode == 204) THEN
       lname = "Surface Solar Radiation Upward"
       sname = "ssru"
       units = "W/m2"
    ELSE IF (kcode == 205) THEN
       lname = "Surface Thermal Radiation Upward"
       sname = "stru"
       units = "W/m2"
    ELSE IF (kcode == 207) THEN
       lname = "Soil Temperature Level 2"
       sname = "tso2"
       units = "K"
    ELSE IF (kcode == 208) THEN
       lname = "Soil Temperature Level 3"
       sname = "tso3"
       units = "K"
    ELSE IF (kcode == 209) THEN
       lname = "Soil Temperature Level 4"
       sname = "tso4"
       units = "K"
    ELSE IF (kcode == 210) THEN
       lname = "Sea Ice Cover"
       sname = "sic"
       units = "1"
    ELSE IF (kcode == 211) THEN
       lname = "Sea Ice Thickness"
       sname = "sit"
       units = "m"
    ELSE IF (kcode == 212) THEN
       lname = "Forest Cover"
       sname = "vegf"
       units = "1"
    ELSE IF (kcode == 218) THEN
       lname = "Snow Melt"
       sname = "snm"
       units = "m/s"
    ELSE IF (kcode == 221) THEN
       lname = "Snow Depth Change"
       sname = "sndc"
       units = "m/s"
    ELSE IF (kcode == 229) THEN
       lname = "Max. Soil Water H. Capacity"
       sname = "mrfc"
       units = "m"
    ELSE IF (kcode == 230) THEN    
       lname = "Vert. Integrated Spec. Hum."
       sname = "prw"
       units = "kg/m2"
    ELSE IF (kcode == 232) THEN
       lname = "Glacier Cover"
       sname = "glac"
       units = "1"
    ELSE IF (kcode == 238) THEN
       lname = "Snow Temperature"
       sname = "tsn"
       units = "K"
    ELSE IF (kcode == 259) THEN
       lname = "Wind Speed"
       sname = "spd"
       units = "m/s"
    ELSE IF (kcode == 260) THEN
       lname = "Total Precipitation"
       sname = "pr"
       units = "m/s"
    ELSE IF (kcode == 261) THEN
       lname = "Net Top Radiation"
       sname = "ntr"
       units = "W/m2"
    ELSE IF (kcode == 262) THEN
       lname = "Net Bottom Radiation"
       sname = "nbr"
       units = "W/m2"
    ELSE IF (kcode == 263) THEN
       lname = "Net Heat Flux"
       sname = "hfns"
       units = "W/m2"
    ELSE IF (kcode == 264) THEN
       lname = "Net Water Flux"
       sname = "wfn"
       units = "m/s"
    ELSE IF (kcode == 273) THEN
       lname = "d(ps)/dx"
       sname = "dpdx"
       units = "Pa m-1"
    ELSE IF (kcode == 274) THEN
       lname = "d(ps)/dy"
       sname = "dpdy"
       units = "Pa m-1"
    ELSE IF (kcode == 277) THEN
       lname = "Half Level Pressure"
       sname = "hlpr"
       units = "Pa"
    ELSE IF (kcode == 278) THEN
       lname = "Full Level Pressure"
       sname = "flpr"
       units = "Pa"
    ELSE IF (kcode == 300) THEN
       lname = "Gross Primary Production"
       sname = "agpp"
       units = "kgC m-2 s-1"
    ELSE IF (kcode == 301) THEN
       lname = "Net Primary Production"
       sname = "anpp"
       units = "kgC m-2 s-1"
    ELSE IF (kcode == 302) THEN
       lname = "Light Limited GPP"
       sname = "agppl"
       units = "kgC m-2 s-1"
    ELSE IF (kcode == 303) THEN
       lname = "Water Limited GPP"
       sname = "agppw"
       units = "kgC m-2 s-1"
    ELSE IF (kcode == 304) THEN
       lname = "Vegetation Carbon"
       sname = "dcveg"
       units = "kgC m-2"
    ELSE IF (kcode == 305) THEN
       lname = "Soil Carbon"
       sname = "dcsoil"
       units = "kgC m-2 s-1"
    ELSE IF (kcode == 306) THEN
       lname = "No Growth Allocation"
       sname = "anogrow"
       units = "kgC m-2 s-1"
    ELSE IF (kcode == 307) THEN
       lname = "Heterotrophic Respiration"
       sname = "aresh"
       units = "kgC m-2 s-1"
    ELSE IF (kcode == 308) THEN
       lname = "Litter Production"
       sname = "alitter"
       units = "kgC m-2 s-1"
    ELSE IF (kcode == 1730) THEN
       lname = "Roughness Length Topography"
       sname = "z0t"
       units = "m"
    ELSE IF (kcode == 1731) THEN
       lname = "Roughness Length Vegetation"
       sname = "z0v"
       units = "m"
    ELSE IF (kcode == 1740) THEN
       lname = "Albedo Bare Soil"
       sname = "albs"
       units = "1"
    ELSE IF (kcode == 1741) THEN
       lname = "Albedo Vegetation"
       sname = "albv"
       units = "1"
    END IF

    RETURN  
  END SUBROUTINE kcoder

  SUBROUTINE genheadr(kcode,hcols,nmon,nlon,nlat,ihead)

    IMPLICIT NONE

    ! Global namespace
    INTEGER(KIND=4), INTENT(IN)  :: kcode
    INTEGER(KIND=4), INTENT(IN)  :: hcols
    INTEGER(KIND=4), INTENT(IN)  :: nmon
    INTEGER(KIND=4), INTENT(IN)  :: nlat
    INTEGER(KIND=4), INTENT(IN)  :: nlon
    INTEGER(KIND=4), DIMENSION(hcols,nmon), INTENT(OUT) :: ihead

    ! Local namespace
    INTEGER(KIND=4) :: i
    INTEGER(KIND=4) :: ilevel = 0
    INTEGER(KIND=4) :: idate
    INTEGER(KIND=4) :: itime
    INTEGER(KIND=4) :: idispo1 = 0
    INTEGER(KIND=4) :: idispo2 = 0

    IF (kcode == 129) THEN
       idate = 20070101
       itime = 0
    ELSE IF (kcode == 169) THEN
       idate = 20090000
       itime = -1
    ELSE IF (kcode == 172) THEN
       idate = 20090101
       itime = 0
    ELSE IF (kcode == 173) THEN
       idate = 990100
       itime = -1
    ELSE IF (kcode == 174) THEN
       idate = 20090001
       itime = 0
    ELSE IF (kcode == 199) THEN
       idate = 20090001
       itime = 0
    ELSE IF (kcode == 200) THEN
       idate = 20090001
       itime = 0
    ELSE IF (kcode == 210) THEN
       idate = 20090000
       itime = -1
    ELSE IF (kcode == 212) THEN
       idate = 990100
       itime = -1
    ELSE IF (kcode == 229) THEN
       idate = 990100
       itime = -1
    ELSE IF (kcode == 232) THEN
       idate = 990100
       itime = -1
    ELSE IF (kcode == 1730) THEN
       idate = 0
       itime = 0
    ELSE IF (kcode == 1731) THEN
       idate = 990100
       itime = -1
    ELSE IF (kcode == 1740) THEN
       idate = 20090101
       itime = 0
    ELSE IF (kcode == 1741) THEN
       idate = 20090101
       itime = 0
    END IF
    
    ! Fill header array
    DO i=0,nmon
       ihead(1,i) = kcode
       ihead(2,i) = ilevel
       ! These dates need a bit of pampering
       IF (i == 1) THEN
          ihead(3,i) = idate
       ELSE   
          IF (itime == 0) THEN
             ihead(3,i) = idate + ((i-1)*100) ! Add 100 when i=2
          ELSE IF (itime == -1) THEN
             ihead(3,i) = idate + ((i-1)*100) + 1 ! Add 100 when i=2 plus 1
          END IF
       END IF
       ihead(4,i) = itime
       ihead(5,i) = nlon
       ihead(6,i) = nlat
       ihead(7,i) = idispo1
       ihead(8,i) = idispo2
    END DO
   
    RETURN
  END SUBROUTINE genheadr
  
  SUBROUTINE latlons32(nlat,nlon,lat,lon)
    
    INTEGER(KIND=4), INTENT(IN)  :: nlat
    INTEGER(KIND=4), INTENT(IN)  :: nlon    
    REAL(KIND=8), DIMENSION(nlat), INTENT(out) :: lat
    REAL(KIND=8), DIMENSION(nlon), INTENT(out) :: lon

    ! Solved Legendre polynomials (not here)
    lat = (/85.7605871204438,&
    &80.26877907225, 74.7445403686358, 69.2129761693708,&
    &63.6786355610969, 58.1429540492033, 52.6065260343453,&
    &47.0696420596877, 41.5324612466561, 35.9950784112716,&
    &30.4575539611521, 24.9199286299486, 19.3822313464344,&
    &13.8444837343849, 8.30670285651881, 2.76890300773601,&
    &-2.76890300773601, -8.30670285651881, -13.8444837343849,&
    &-19.3822313464344, -24.9199286299486, -30.4575539611521,&
    &-35.9950784112716, -41.5324612466561, -47.0696420596877,&
    &-52.6065260343453, -58.1429540492033, -63.6786355610969,&
    &-69.2129761693708, -74.7445403686358, -80.26877907225,&
    &-85.7605871204438/)

    lon = (/0.0, 5.625, 11.25, 16.875, 22.5, 28.125, 33.75, 39.375, 45.0,&
    &50.625, 56.25, 61.875, 67.5, 73.125, 78.75, 84.375, 90.0, 95.625,&
    &101.25, 106.875, 112.5, 118.125, 123.75, 129.375, 135.0, 140.625,&
    &146.25, 151.875, 157.5, 163.125, 168.75, 174.375, 180.0, 185.625,&
    &191.25, 196.875, 202.5, 208.125, 213.75, 219.375, 225.0, 230.625,&
    &236.25, 241.875, 247.5, 253.125, 258.75, 264.375, 270.0, 275.625,&
    &281.25, 286.875, 292.5, 298.125, 303.75, 309.375, 315.0, 320.625,&
    &326.25, 331.875, 337.5, 343.125, 348.75, 354.375/)

    RETURN    
  END SUBROUTINE latlons32

  SUBROUTINE latlons64(nlat,nlon,lat,lon)
    
    INTEGER(KIND=4), INTENT(IN)  :: nlat
    INTEGER(KIND=4), INTENT(IN)  :: nlon    
    REAL(KIND=8), DIMENSION(nlat), INTENT(out) :: lat
    REAL(KIND=8), DIMENSION(nlon), INTENT(out) :: lon

    ! Solved Legendre polynomials (not here)
    lat = (/87.8637988392326, 85.0965269883174,&
    &82.3129129478863, 79.5256065726594, 76.7368996803683,&
    &73.9475151539896, 71.1577520115873, 68.3677561083132,&
    &65.5776070108278, 62.7873517989631, 59.9970201084913,&
    &57.2066315276433, 54.4161995260862, 51.6257336749383,&
    &48.8352409662506, 46.0447266311017, 43.2541946653509,&
    &40.463648178115, 37.6730896290453, 34.8825209937735,&
    &32.091943881744, 29.3013596217627, 26.510769325211,&
    &23.7201739335347, 20.9295742544895, 18.1389709902393,&
    &15.3483647594915, 12.5577561152307, 9.76714555919557,&
    &6.97653355394864, 4.18592053318915, 1.3953069108195,&
    &-1.3953069108195, -4.18592053318915, -6.97653355394864,&
    &-9.76714555919557, -12.5577561152307, -15.3483647594915,&
    &-18.1389709902393, -20.9295742544895, -23.7201739335347,&
    &-26.510769325211, -29.3013596217627, -32.091943881744,&
    &-34.8825209937735, -37.6730896290453, -40.463648178115,&
    &-43.2541946653509, -46.0447266311017, -48.8352409662506,&
    &-51.6257336749383, -54.4161995260862, -57.2066315276433,&
    &-59.9970201084913, -62.7873517989631, -65.5776070108278,&
    &-68.3677561083132, -71.1577520115873, -73.9475151539896,&
    &-76.7368996803683, -79.5256065726594, -82.3129129478863,&
    &-85.0965269883174, -87.8637988392326/)

    lon = (/0.0, 2.8125, 5.625, 8.4375, 11.25, 14.0625,&
    &16.875, 19.6875, 22.5, 25.3125, 28.125, 30.9375, 33.75, 36.5625,&
    &39.375, 42.1875, 45.0, 47.8125, 50.625, 53.4375, 56.25, 59.0625,&
    &61.875, 64.6875, 67.5, 70.3125, 73.125, 75.9375, 78.75, 81.5625,&
    &84.375, 87.1875, 90.0, 92.8125, 95.625, 98.4375, 101.25, 104.0625,&
    &106.875, 109.6875, 112.5, 115.3125, 118.125, 120.9375, 123.75,&
    &126.5625, 129.375, 132.1875, 135.0, 137.8125, 140.625, 143.4375,&
    &146.25, 149.0625, 151.875, 154.6875, 157.5, 160.3125, 163.125,&
    &165.9375, 168.75, 171.5625, 174.375, 177.1875, 180.0, 182.8125,&
    &185.625, 188.4375, 191.25, 194.0625, 196.875, 199.6875, 202.5,&
    &205.3125, 208.125, 210.9375, 213.75, 216.5625, 219.375, 222.1875,&
    &225.0, 227.8125, 230.625, 233.4375, 236.25, 239.0625, 241.875,&
    &244.6875, 247.5, 250.3125, 253.125, 255.9375, 258.75, 261.5625,&
    &264.375, 267.1875, 270.0, 272.8125, 275.625, 278.4375, 281.25,&
    &284.0625, 286.875, 289.6875, 292.5, 295.3125, 298.125, 300.9375,&
    &303.75, 306.5625, 309.375, 312.1875, 315.0, 317.8125, 320.625,&
    &323.4375, 326.25, 329.0625, 331.875, 334.6875, 337.5, 340.3125,&
    &343.125, 345.9375, 348.75, 351.5625, 354.375, 357.1875/)

    RETURN    
  END SUBROUTINE latlons64

END MODULE CODEmat
