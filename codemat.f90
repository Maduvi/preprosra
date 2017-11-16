MODULE CODEmat
  !-------------------------------------------------------------------
  !
  !  Module containing a subroutine that creates a big array with
  !  shortnames, longnames and units for common PlaSim variables.
  !  
  !  CONTAINS:
  !           kcodematrix -> creates big matrix with codes as indices
  !           latlons -> gives lat and lot arrays got elsewhere
  !
  !  Created: Mateo Duque Villegas
  !  Last updated: 16-Nov-2017
  !
  !-------------------------------------------------------------------
  IMPLICIT NONE

CONTAINS

  SUBROUTINE kcodematrix(kcodemat)
 
    ! Big array with kcodes for indexes
    ! Dimension is so that we can use kcode as indexes
    CHARACTER(LEN=*), DIMENSION(100:1800,3), INTENT(out) :: KCODEMAT

    KCODEMAT(110,1) = "Mixed Layer Depth"
    KCODEMAT(110,2) = "mld"
    KCODEMAT(110,3) = "m"

    KCODEMAT(129,1) = "Surface Geopotential Orography"
    KCODEMAT(129,2) = "sg"
    KCODEMAT(129,3) = "m2/s2"

    KCODEMAT(130,1) = "Temperature"
    KCODEMAT(130,2) = "ta"
    KCODEMAT(130,3) = "K"

    KCODEMAT(131,1) = "Zonal Wind"
    KCODEMAT(131,2) = "ua"
    KCODEMAT(131,3) = "m/s"

    KCODEMAT(132,1) = "Meridional Wind"
    KCODEMAT(132,2) = "va"
    KCODEMAT(132,3) = "m/s"

    KCODEMAT(133,1) = "Specific Humidity"
    KCODEMAT(133,2) = "hus"
    KCODEMAT(133,3) = "kg/kg"

    KCODEMAT(134,1) = "Surface Pressure"
    KCODEMAT(134,2) = "ps"
    KCODEMAT(134,3) = "hPa"

    KCODEMAT(135,1) = "Vertical Velocity"
    KCODEMAT(135,2) = "wap"
    KCODEMAT(135,3) = "Pa/s"

    KCODEMAT(137,1) = "Vertical Wind"
    KCODEMAT(137,2) = "wa"
    KCODEMAT(137,3) = "m/s"

    KCODEMAT(138,1) = "Vorticity"
    KCODEMAT(138,2) = "zeta"
    KCODEMAT(138,3) = "1/s"

    KCODEMAT(139,1) = "Surface Temperature"
    KCODEMAT(139,2) = "ts"
    KCODEMAT(139,3) = "K"

    KCODEMAT(140,1) = "Soil Wetness"
    KCODEMAT(140,2) = "mrso"
    KCODEMAT(140,3) = "m"

    KCODEMAT(141,1) = "Snow Depth"
    KCODEMAT(141,2) = "snd"
    KCODEMAT(141,3) = "m"

    KCODEMAT(142,1) = "Large Scale Precipitation"
    KCODEMAT(142,2) = "prl"
    KCODEMAT(142,3) = "m/s"

    KCODEMAT(143,1) = "Convective Precipitation"
    KCODEMAT(143,2) = "prc"
    KCODEMAT(143,3) = "m/s"

    KCODEMAT(144,1) = "Snow Fall"
    KCODEMAT(144,2) = "prsn"
    KCODEMAT(144,3) = "m/s"

    KCODEMAT(145,1) = "Boundary Layer Dissipation"
    KCODEMAT(145,2) = "bld"
    KCODEMAT(145,3) = "W/m2"

    KCODEMAT(146,1) = "Surface Sensible Heat Flux"
    KCODEMAT(146,2) = "hfss"
    KCODEMAT(146,3) = "W/m2"

    KCODEMAT(147,1) = "Surface Latent Heat Flux"
    KCODEMAT(147,2) = "hfls"
    KCODEMAT(147,3) = "W/m2"

    KCODEMAT(148,1) = "Streamfunction"
    KCODEMAT(148,2) = "stf"
    KCODEMAT(148,3) = "m2/s"

    KCODEMAT(149,1) = "Velocity Potential"
    KCODEMAT(149,2) = "psi"
    KCODEMAT(149,3) = "m2/s"

    KCODEMAT(151,1) = "Mean Sea Level Pressure"
    KCODEMAT(151,2) = "psl"
    KCODEMAT(151,3) = "hPa"

    KCODEMAT(152,1) = "Log Surface Pressure"
    KCODEMAT(152,2) = "pl"
    KCODEMAT(152,3) = "1"

    KCODEMAT(155,1) = "Divergence"
    KCODEMAT(155,2) = "d"
    KCODEMAT(155,3) = "1/s"

    KCODEMAT(156,1) = "Geopotential Height"
    KCODEMAT(156,2) = "zg"
    KCODEMAT(156,3) = "gpm"

    KCODEMAT(157,1) = "Relative Humidity"
    KCODEMAT(157,2) = "hur"
    KCODEMAT(157,3) = "%"

    KCODEMAT(158,1) = "Tendency of Surface Pressure"
    KCODEMAT(158,2) = "tps"
    KCODEMAT(158,3) = "Pa/s"

    KCODEMAT(159,1) = "ustar**3"
    KCODEMAT(159,2) = "u3"
    KCODEMAT(159,3) = "m3/s3"

    KCODEMAT(160,1) = "Surface Runoff"
    KCODEMAT(160,2) = "mrro"
    KCODEMAT(160,3) = "m/s"

    KCODEMAT(161,1) = "Liquid Water Content"
    KCODEMAT(161,2) = "clw"
    KCODEMAT(161,3) = "kg/kg"

    KCODEMAT(162,1) = "Cloud Cover"
    KCODEMAT(162,2) = "cl"
    KCODEMAT(162,3) = "1"

    KCODEMAT(163,1) = "Total Cloud Cover"
    KCODEMAT(163,2) = "tcc"
    KCODEMAT(163,3) = "1"

    KCODEMAT(164,1) = "Total Cloud Cover (Mean)"
    KCODEMAT(164,2) = "clt"
    KCODEMAT(164,3) = "1"

    KCODEMAT(165,1) = "Eastward Wind 10m"
    KCODEMAT(165,2) = "uas"
    KCODEMAT(165,3) = "m/s"

    KCODEMAT(166,1) = "Northward Wind 10m"
    KCODEMAT(166,2) = "vas"
    KCODEMAT(166,3) = "m/s"

    KCODEMAT(167,1) = "2m Temperature"
    KCODEMAT(167,2) = "tas"
    KCODEMAT(167,3) = "K"

    KCODEMAT(168,1) = "2m Dew Point Temperature"
    KCODEMAT(168,2) = "td2m"
    KCODEMAT(168,3) = "K"

    KCODEMAT(169,1) = "Surface Temperature Accumulated"
    KCODEMAT(169,2) = "tsa"
    KCODEMAT(169,3) = "K"

    KCODEMAT(170,1) = "Deep Soil Temperature"
    KCODEMAT(170,2) = "tsod"
    KCODEMAT(170,3) = "K"

    KCODEMAT(171,1) = "Deep Soil Wetness"
    KCODEMAT(171,2) = "dsw"
    KCODEMAT(171,3) = "1"

    KCODEMAT(172,1) = "Land Sea Mask"
    KCODEMAT(172,2) = "lsm"
    KCODEMAT(172,3) = "1"

    KCODEMAT(173,1) = "Surface Roughness"
    KCODEMAT(173,2) = "z0"
    KCODEMAT(173,3) = "m"

    KCODEMAT(174,1) = "Surface Albedo"
    KCODEMAT(174,2) = "alb"
    KCODEMAT(174,3) = "1"

    KCODEMAT(176,1) = "Surface Solar Radiation"
    KCODEMAT(176,2) = "ssr"
    KCODEMAT(176,3) = "W/m2"

    KCODEMAT(177,1) = "Surface Thermal Radiation"
    KCODEMAT(177,2) = "rss"
    KCODEMAT(177,3) = "W/m2"

    KCODEMAT(178,1) = "Top Solar Radiation"
    KCODEMAT(178,2) = "rst"
    KCODEMAT(178,3) = "W/m2"

    KCODEMAT(179,1) = "Top Thermal Radiation"
    KCODEMAT(179,2) = "rlut"
    KCODEMAT(179,3) = "W/m2"

    KCODEMAT(180,1) = "U-Stress"
    KCODEMAT(180,2) = "tauu"
    KCODEMAT(180,3) = "Pa"

    KCODEMAT(181,1) = "V-Stress"
    KCODEMAT(181,2) = "tauv"
    KCODEMAT(181,3) = "Pa"

    KCODEMAT(182,1) = "Evaporation"
    KCODEMAT(182,2) = "evap"
    KCODEMAT(182,3) = "m/s"

    KCODEMAT(183,1) = "Soil Temperature"
    KCODEMAT(183,2) = "tso"
    KCODEMAT(183,3) = "K"

    KCODEMAT(184,1) = "Soil Wetness"
    KCODEMAT(184,2) = "wsoi"
    KCODEMAT(184,3) = "1"

    KCODEMAT(199,1) = "Vegetation Cover"
    KCODEMAT(199,2) = "vegc"
    KCODEMAT(199,3) = "1"

    KCODEMAT(200,1) = "Leaf Area Index"
    KCODEMAT(200,2) = "dlai"
    KCODEMAT(200,3) = "1"

    KCODEMAT(203,1) = "Tp Solar Radiation Upward"
    KCODEMAT(203,2) = "rsdt"
    KCODEMAT(203,3) = "W/m2"

    KCODEMAT(204,1) = "Surface Solar Radiation Upward"
    KCODEMAT(204,2) = "ssru"
    KCODEMAT(204,3) = "W/m2"

    KCODEMAT(205,1) = "Surface Thermal Radiation Upward"
    KCODEMAT(205,2) = "stru"
    KCODEMAT(205,3) = "W/m2"

    KCODEMAT(207,1) = "Soil Temperature Level 2"
    KCODEMAT(207,2) = "tso2"
    KCODEMAT(207,3) = "K"

    KCODEMAT(208,1) = "Soil Temperature Level 3"
    KCODEMAT(208,2) = "tso3"
    KCODEMAT(208,3) = "K"

    KCODEMAT(209,1) = "Soil Temperature Level 4"
    KCODEMAT(209,2) = "tso4"
    KCODEMAT(209,3) = "K"

    KCODEMAT(210,1) = "Sea Ice Cover"
    KCODEMAT(210,2) = "sic"
    KCODEMAT(210,3) = "1"

    KCODEMAT(211,1) = "Sea Ice Thickness"
    KCODEMAT(211,2) = "sit"
    KCODEMAT(211,3) = "m"

    KCODEMAT(212,1) = "Forest Cover"
    KCODEMAT(212,2) = "vegf"
    KCODEMAT(212,3) = "1"

    KCODEMAT(218,1) = "Snow Melt"
    KCODEMAT(218,2) = "snm"
    KCODEMAT(218,3) = "m/s"

    KCODEMAT(221,1) = "Snow Depth Change"
    KCODEMAT(221,2) = "sndc"
    KCODEMAT(221,3) = "m/s"

    KCODEMAT(229,1) = "Max. Soil Water H. Capacity"
    KCODEMAT(229,2) = "mrfc"
    KCODEMAT(229,3) = "m"
    
    KCODEMAT(230,1) = "Vert. Integrated Spec. Hum."
    KCODEMAT(230,2) = "prw"
    KCODEMAT(230,3) = "kg/m2"

    KCODEMAT(232,1) = "Glacier Cover"
    KCODEMAT(232,2) = "glac"
    KCODEMAT(232,3) = "1"

    KCODEMAT(238,1) = "Snow Temperature"
    KCODEMAT(238,2) = "tsn"
    KCODEMAT(238,3) = "K"

    KCODEMAT(259,1) = "Wind Speed"
    KCODEMAT(259,2) = "spd"
    KCODEMAT(259,3) = "m/s"

    KCODEMAT(260,1) = "Total Precipitation"
    KCODEMAT(260,2) = "pr"
    KCODEMAT(260,3) = "m/s"

    KCODEMAT(261,1) = "Net Top Radiation"
    KCODEMAT(261,2) = "ntr"
    KCODEMAT(261,3) = "W/m2"

    KCODEMAT(262,1) = "Net Bottom Radiation"
    KCODEMAT(262,2) = "nbr"
    KCODEMAT(262,3) = "W/m2"

    KCODEMAT(263,1) = "Net Heat Flux"
    KCODEMAT(263,2) = "hfns"
    KCODEMAT(263,3) = "W/m2"

    KCODEMAT(264,1) = "Net Water Flux"
    KCODEMAT(264,2) = "wfn"
    KCODEMAT(264,3) = "m/s"

    KCODEMAT(273,1) = "d(ps)/dx"
    KCODEMAT(273,2) = "dpdx"
    KCODEMAT(273,3) = "Pa m-1"

    KCODEMAT(274,1) = "d(ps)/dy"
    KCODEMAT(274,2) = "dpdy"
    KCODEMAT(274,3) = "Pa m-1"

    KCODEMAT(277,1) = "Half Level Pressure"
    KCODEMAT(277,2) = "hlpr"
    KCODEMAT(277,3) = "Pa"

    KCODEMAT(278,1) = "Full Level Pressure"
    KCODEMAT(278,2) = "flpr"
    KCODEMAT(278,3) = "Pa"

    KCODEMAT(300,1) = "Gross Primary Production"
    KCODEMAT(300,2) = "agpp"
    KCODEMAT(300,3) = "kgC m-2 s-1"

    KCODEMAT(301,1) = "Net Primary Production"
    KCODEMAT(301,2) = "anpp"
    KCODEMAT(301,3) = "kgC m-2 s-1"

    KCODEMAT(302,1) = "Light Limited GPP"
    KCODEMAT(302,2) = "agppl"
    KCODEMAT(302,3) = "kgC m-2 s-1"

    KCODEMAT(303,1) = "Water Limited GPP"
    KCODEMAT(303,2) = "agppw"
    KCODEMAT(303,3) = "kgC m-2 s-1"

    KCODEMAT(304,1) = "Vegetation Carbon"
    KCODEMAT(304,2) = "dcveg"
    KCODEMAT(304,3) = "kgC m-2"

    KCODEMAT(305,1) = "Soil Carbon"
    KCODEMAT(305,2) = "dcsoil"
    KCODEMAT(305,3) = "kgC m-2 s-1"

    KCODEMAT(306,1) = "No Growth Allocation"
    KCODEMAT(306,2) = "anogrow"
    KCODEMAT(306,3) = "kgC m-2 s-1"

    KCODEMAT(307,1) = "Heterotrophic Respiration"
    KCODEMAT(307,2) = "aresh"
    KCODEMAT(307,3) = "kgC m-2 s-1"

    KCODEMAT(308,1) = "Litter Production"
    KCODEMAT(308,2) = "alitter"
    KCODEMAT(308,3) = "kgC m-2 s-1"

    KCODEMAT(1730,1) = "Roughness Length Topography"
    KCODEMAT(1730,2) = "z0t"
    KCODEMAT(1730,3) = "m"

    KCODEMAT(1731,1) = "Roughness Length Vegetation"
    KCODEMAT(1731,2) = "z0v"
    KCODEMAT(1731,3) = "m"

    KCODEMAT(1740,1) = "Albedo Bare Soil"
    KCODEMAT(1740,2) = "albs"
    KCODEMAT(1740,3) = "1"

    KCODEMAT(1741,1) = "Albedo Vegetation"
    KCODEMAT(1741,2) = "albv"
    KCODEMAT(1741,3) = "1"

    RETURN
    
  END SUBROUTINE kcodematrix

  SUBROUTINE latlons(nlat,nlon,lat,lon)
    
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
  END SUBROUTINE latlons
  
END MODULE CODEmat
