#! /bin/bash

# Download Rtopo2 here: https://doi.pangaea.de/10.1594/PANGAEA.856844
# put the bedrock topography grid here

first_run=y
bed_topo_rtopo=RTopo-2.0.1_30sec_bedrock_topography.nc

resolution=0.25
half_resolution=0.125

bed_topo=bed_topo.nc

# it is only necessary to run this once. Rtopo2 does not use COARDS compliant variable names
if [ "${first_run}" == "y" ]
then
ncrename -O -d londim,x -d latdim,y -v lon,x -v lat,y ${bed_topo_rtopo} ${bed_topo}

fi

grdfilter ${bed_topo} -Fb${resolution} -Gfiltered_bed_topo_${resolution}.nc -I${resolution} -D3
