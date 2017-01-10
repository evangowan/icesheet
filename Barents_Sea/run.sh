#! /bin/bash


# make sure you create the polygons.csv file in the shear_stress folder before running this script. Also make sure
# you have the base topography grid somewhere (see options below to set the location)

# this particular setup assumes that the input shear stress polygons, margin and topography files are in lat/long.
# If you already have them in a projected coordinate system, comment out the commands that do the coordinate transformation


###############################################################################################################################
#
# Make sure you check the variables below, particularly the path to the topography file and ICESHEET executable are correct!
#
###############################################################################################################################



# Resolution of the input grids (shear stress and base topography) for icesheet
grid_spacing=5000 # in m

# Resolution for the final grids, probably should be greater than grid_spacing if you want it to be smooth
# The file is stores as a binary file as "grids/${simulation}_thickness.nc"
# To get a text file, run this command after running this script: "grd2xyz grids/${simulation}_thickness.nc > file.xyz"
final_grid_spacing=20000 # in m

# For binary file of ice thickness values, will be stored as "${simulation}_thickness_ll.nc"
# To get a text file, run this command after running this script: "grd2xyz grids/${simulation}_thickness_ll.nc > file.xyz"
final_lat_spacing=0.25
final_long_spacing=0.5


# Parameters for calculation of the ice sheet with ICESHEET

# Minimum spacing between points along the contour where flowline calculation is initiated
initiation_distance=5000 # in m

# contour interval
contour_interval=20 # in m

# path to the icesheet exectuable, leave blank if you have it somewhere in ${PATH}
icesheet_path="../"


# lat/long file of the margin

# This particular one is the DATED most likely margin for 20000 years before present, cut off so that it is only for the Barents Sea
margin="margins/20000/dated_mostlikely.txt" 

# base name for files
simulation="barents_sea_${contour_interval}_${initiation_distance}_modtopo"

# Input parameters for the shear stress map
shear_stress_polygons=shear_stress/gmt_file.txt
shear_stress_domains="shear_stress/shear_stress_domain_values.txt" # can make this variable for different times

# Change the location to whatever netcdf file contains your topography
# For Barents Sea, I am using the IBCAO 3.0 grid (Jakobsson et al., 2012)
topo_location="IBCAO/IBCAO_V3_30arcsec_SM.grd"

# it can be computationally costly to run the grid each time, change this to "n" after the first time you run this
create_base_topo="y"

# if you want to get rid of the temp files, set this to "y".
delete_temp_files="y"



# Parameters for doing transformation to/from projected and geographic coordinates

# a good set of parameters to capture the center of the Barents Sea Ice Sheet using an Albers projection.
# If you want to use a different projection, change the -J options

center_longitude=47.4
center_latitude=76.2

southern_standard_parallel=70
northern_standard_parallel=80

map_width=20c # likely arbitrary

J_options="-Jb${center_longitude}/${center_latitude}/${southern_standard_parallel}/${northern_standard_parallel}/${map_width}"

# Longitude and latitude of region of interest, make sure it encompasses the provided outline at a minimum

long_min=0
long_max=110
lat_min=55
lat_max=85

R_options="-Rd${long_min}/${long_max}/${lat_min}/${lat_max}"




# Projected map parameters

# projected map boundary, this set of parameters are large enough to cover the Barents Sea Ice Sheet at the LGM
# If you use a different area, you should change these. Might be easiest to pick them off from a GIS software, otherwise
# you will have to get ideal values by trial and error

xmin=-2000000
xmax=1500000
ymin=-2000000
ymax=1500000

R_projected_options="-R${xmin}/${xmax}/${ymin}/${ymax}"

# if you are not using a rectangular grid, you should change the dimensions here
J_projected_options="-JX15c/15c"

xshift=3
yshift=9

# axis labeling
xtext="x (m)"
xtickint=500000
xsubtickint=250000

ytext="y (m)"
ytickint=500000
ysubtickint=250000


######################################################################
#
# You shouldn't need to change anything below here, unless you add GIA
#
######################################################################

mkdir plots
mkdir grids

# transform margin to projected coordinates

projected_margin="margin.xyz"

# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# comment out the mapproject line if the margin is already projected (but make sure ${projected_margin} has the right file name) 
# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!
mapproject ${margin}  ${R_options} -F -C  ${J_options}  > ${projected_margin}

# transform the shear stress polygons to projected coordinates

projected_file_ss="shear_stress_transformed.gmt"

# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# comment out the mapproject line if the margin is already projected (but make sure ${projected_file_ss} has the right file name) 
# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!
mapproject ${shear_stress_polygons}  ${R_options} -F -C  ${J_options}  > ${projected_file_ss}




# create shear stress parameters file


if [ ! -f "${shear_stress_polygons}" ]; then
	cd shear_stress
	bash create_shear_stress_file.sh 
	cd ..
fi


if [ ! -f "create_ss_grid" ]; then
    make create_ss_grid
fi

cat << END_CAT > ss_grid_params.txt
${projected_file_ss}
${xmin}
${xmax}
${ymin}
${ymax}
${grid_spacing}
${shear_stress_domains}
END_CAT

./create_ss_grid 


xyz2grd   shear_stress.xyz -I${grid_spacing} ${R_projected_options} -Ggrids/shear_stress_poly.nc 
grdconvert grids/shear_stress_poly.nc grids/shear_stress_poly.bin=bf 


cat << END_CAT > grids/ss_parameters.txt
grids/shear_stress_poly.bin
${xmin}
${xmax}
${ymin}
${ymax}
${grid_spacing}
END_CAT

makecpt -Cwysiwyg -T0/120000/5000 > shades_shearstress.cpt

# plot shear stress on map
plot="plots/shear_stress_poly.ps"

grdimage grids/shear_stress_poly.nc -Cshades_shearstress.cpt -X${xshift} -Y${yshift} -JX15c/15c ${R_projected_options}  -Ba"${xtickint}"f"${xsubtickint}":"${xtext}":/a"${ytickint}"f"${ysubtickint}":"${ytext}":WeSn -V -P -nb+a+bg+t0.1 -K > ${plot}

psxy ${projected_file_ss} -R -JX -Wthin,black -O -K >> ${plot}

psxy ${projected_margin}  -R -JX -Wthickest,white -O -K >> ${plot}
psxy ${projected_margin}  -R -JX -Wthin,black -O -K >> ${plot}



psscale -O  -Y-11 -Cshades_shearstress.cpt -JX -R -D0/0/14/0.5h -P  -Ba20000:"shear stress (Pa)":/:: >> ${plot}


# make topography file

topo_grid_transformed=grids/topo_transformed.bin

if [ "${create_base_topo}" = "y" ]
then

	# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	#
	# comment out the grdcut and grdproject line if the margin is already projected (but make sure to change grids/topo_transformed_temp.nc) 
	#
	# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!

	grdcut ${topo_location} -Ggrids/smaller_grid.nc -N-9999 ${R_options}
	grdproject grids/smaller_grid.nc ${R_options}  ${J_options} -Ggrids/topo_transformed_temp.nc -D${grid_spacing} -Fe -C -V  -nc+c

	grd2xyz grids/topo_transformed_temp.nc > temp.out

	surface temp.out -Ggrids/topo_transformed.nc   -Rgrids/shear_stress_poly.nc -T0.35  

	grdconvert grids/topo_transformed.nc ${topo_grid_transformed}=bf 

fi




#######################################################
# If you want to add GIA to the topography, add it here
# should take a GIA grid, and use grdmath to add it to  grids/topo_transformed.nc, and then replace ${topo_grid_transformed}

# alternatively, make a separate script to do this

########################################################


cat << END_CAT > grids/elev_parameters.txt
${topo_grid_transformed}
${xmin}
${xmax}
${ymin}
${ymax}
${grid_spacing}
END_CAT

# plot base topography

plot="plots/base_topo.ps"

makecpt -Cglobe -T-10000/10000/50   > elev_shades.cpt

grdimage ${topo_grid_transformed}=bf -Celev_shades.cpt -X${xshift} -Y${yshift} -JX15c/15c ${R_projected_options}  -Ba"${xtickint}"f"${xsubtickint}":"${xtext}":/a"${ytickint}"f"${ysubtickint}":"${ytext}":WeSn -V -P -nb+a+bg+t0.1 -K > ${plot}


psxy ${projected_margin}  -R -JX -Wthickest,white -O -K >> ${plot}
psxy ${projected_margin}  -R -JX -Wthin,black -O -K >> ${plot}



psscale -Y-13 -X-1 -D0/0/14/0.5h -O -JX -R -Ba2000:"Topography (m)":/:: -Celev_shades.cpt  >> ${plot}



# run icesheet

cat << END_params > params.txt
${projected_margin}
grids/elev_parameters.txt
grids/ss_parameters.txt
${contour_interval}
${initiation_distance}
END_params

mkdir results

echo ${simulation}  > results/${simulation}
echo "" >> results/${simulation}
cat params.txt >> results/${simulation}

echo "" >> results/${simulation}
echo "Start time" >>  results/${simulation}
date >>  results/${simulation}
echo "" >> results/${simulation}

${icesheet_path}icesheet

echo "" >> results/${simulation}
echo "End time" >>  results/${simulation}
date >>  results/${simulation}
echo "" >> results/${simulation}

results_file="results/${simulation}_results.txt"

awk '{if ($1 != ">") print $0}' contours.txt > ${results_file}


# plot ice sheet surface

plot=plots/${simulation}_elev.ps

awk '{print $1, $2, $3}' ${results_file} > awk.out


blockmean ${R_projected_options} -I${final_grid_spacing}= awk.out > mean_surface.out

ice_surface_grid="grids/${simulation}_ice_elev_surface.nc"

surface mean_surface.out -G${ice_surface_grid}  ${R_projected_options} -I${final_grid_spacing}= -T0.35  

makecpt -Cjet -T-3500/3500/250 -G0/3500 -I  > iceshades.cpt


grdimage ${topo_grid_transformed}=bf -Celev_shades.cpt -X${xshift} -Y${yshift} -JX15c/15c ${R_projected_options}  -Ba"${xtickint}"f"${xsubtickint}":"${xtext}":/a"${ytickint}"f"${ysubtickint}":"${ytext}":WeSn -V -P -nb+a+bg+t0.1 -K > ${plot}


psclip ${projected_margin} -K -O -R -JX >> $plot

grdimage ${ice_surface_grid} -Ciceshades.cpt  -JX -R  -V -P -nb+a+bg+t0.1 -K -O >> ${plot}

grdcontour ${ice_surface_grid} -Ciceshades.cpt -R -JX -K -O -W0.75p,darkblue -A+f8p,white >> ${plot}

psclip -K -O -C >> $plot

psxy ${projected_margin} -K -O -R -JX -W1p,brown >> ${plot} 

psscale -Y-11 -X-1 -D0/0/14/0.5h -G0/3500 -O -JX -R -Ba1000:"Topography (m)":/:: -Ciceshades.cpt  >> ${plot}


# plot ice sheet thickness

ice_thickness_grid="grids/${simulation}_thickness.nc"

plot=plots/${simulation}_thickness.ps
makecpt -Cjet -T0/3500/50   > shades.cpt
makecpt -Cjet -T0/3500/250   > shades_coarse.cpt

awk '{print $1, $2, $4}' ${results_file} > awk.out

blockmean ${R_projected_options} -I${final_grid_spacing}= awk.out > mean.out



surface mean.out -Ggrids/thickness_surface_temp.grd  ${R_projected_options} -I${final_grid_spacing}= -T0.35  

grdmask ${R_projected_options} -I${final_grid_spacing}= ${projected_margin} -Ggrids/mask.grd -N0/0/1

grdmath grids/thickness_surface_temp.grd grids/mask.grd MUL = ${ice_thickness_grid}


psclip ${projected_margin} -K -O -R -JX >> $plot

grdimage  ${ice_thickness_grid}  -Cshades.cpt -X${xshift} -Y${yshift} -JX15c/15c ${R_projected_options}  -Ba"${xtickint}"f"${xsubtickint}":"${xtext}":/a"${ytickint}"f"${ysubtickint}":"${ytext}":WeSn -V -P -nb+a+bg+t0.1 -K > ${plot}


psclip -K -O -C >> $plot

psclip ${projected_margin} -K -O -R -JX >> $plot

grdcontour ${ice_thickness_grid} -Cshades_coarse.cpt -R -JX -K -O -W0.75p,darkblue -A500+f9p,black+gwhite >> ${plot}

psclip -K -O -C >> $plot

psxy ${projected_margin} -K -O -R -JX -W2p,brown >> ${plot}

psscale -Y-11 -X-1 -D0/0/14/0.5h  -O -JX -R -Ba1000:"Ice thickness (m)":/:: -Cshades_coarse.cpt  >> ${plot}



# finally, make a lat/long file with the thickness data

mapproject awk.out -I  ${R_options} -F -C  ${J_options}  > thickness.ll

grdmask  ${R_options} -I${final_long_spacing}=/${final_lat_spacing}= ${margin} -Ggrids/mask_ll.grd -N0/0/1
blockmean ${R_options} -I${final_long_spacing}=/${final_lat_spacing}= thickness.ll > mean.out

surface mean.out -Ggrids/${simulation}_thickness_ll_temp.nc ${R_options} -I${final_long_spacing}=/${final_lat_spacing}= -T0.35  
grdmath grids/${simulation}_thickness_ll_temp.nc grids/mask_ll.grd MUL = grids/${simulation}_thickness_ll.nc


# clean up files
if [ "${delete_temp_files}" = "y" ]
then

rm awk.out
rm contours.txt
rm contours-rejected.txt
rm elev_shades.cpt
rm gmt.history
rm iceshades.cpt
rm margin.xyz
rm mean.out
rm mean_surface.out
rm oversample_points.txt
rm params.txt
rm shades.cpt
rm shades_coarse.cpt
rm shades_shearstress.cpt
rm shear_stress.xyz
rm shear_stress_transformed.gmt
rm ss_grid_params.txt
rm temp.out
rm grids/mask.grd
rm grids/smaller_grid.nc
rm grids/thickness_surface_temp.grd
rm grids/topo_transformed_temp.nc
rm grids/barents_sea_20_10000_modtopo_thickness_ll_temp.nc
rm grids/mask_ll.grd
rm thickness.ll

fi
