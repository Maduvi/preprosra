# preproSRA: modify dat (surface.txt) files for PlaSim

A set of tools is being developed to be able to modify SURF files that are input to Planet Simulator. One tool can convert from .sra to .nc, and the other one from .nc to .sra. This way one can use the great amount of tools available to modify netCDF files and then generate new surf files as desired.

## sra2nc
Converts .SRA files to netCDF using the Fortran and the netCDF-fortran library.

## nc2sra
Converts .nc files to SERVICE format as in ECHAM.
