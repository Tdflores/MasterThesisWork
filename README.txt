Gridprogram - module_femission.f90

A Fortran 90 module used for gridding Fire Radiative Power (FRP) satellite data (.dat 
files) into a specific domainat a specific resolution, for the purpose 
of input into NASA's Unified Weather and Research Forecasting (NUWRF) 
model coupled with LIS.  The program can use data from MODIS or SEVIRI instruments.

User needs to specify:

Location of input files 
Location of output directory
SEVIRI or MODIS data
Grid resolution in degrees
Lat/lon of domain

May need to alter subroutines reading in data to account for header lines (accounting
for 3 header lines currently)

The output is a gridded .bin file for each input file.
