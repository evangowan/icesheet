#! /bin/bash

# create input files for the ice sheet program. 

# you need to have GMT >=4.1 and netcdf tools (nco) installed for this to work

# you also must compile the "nearest_int" and "reduce_dem" programs, which are included in the source directory

# Run the script in the shear stress folder first, or else this won't work


# make sure you change the path to the location of the Greenland dataset
# download from here: https://nsidc.org/data/IDBMG4/versions/2
greenland_netcdf=MCdataset-2015-04-27.nc

# note that the process of extracting the outline takes a while, so only set this to "y" the first time you run the script
execute_outline=y

# if you haven't made a change to the topography (i.e. after a GIA step) you can turn off the shear stress file calculation.
# It doesn't really take a huge amount of time, so you can leave it on if you want.
execute_ss=y

binary_path="../" # leave blank if your binaries are already in $PATH


# note that the grid spacing must be an integer! (in metres)


resolution=1000
# make contour

outline_file=outline.xyz




if [ "${execute_outline}" = "y" ]
then

# extract outlines

ncks -v mask ${greenland_netcdf}  out.nc
makecpt -Cgray -T0/1/0.1  > shades.cpt
grdmath out.nc 2 EQ = out2.nc

grdsample out2.nc -Gout3.nc -nn -I${resolution} 
#grdcontour out3.nc -Cshades.cpt -Q$(echo "${resolution} * 2" | bc) -L0.45/0.55 -JX-12.8/-24 -R-700000/900000/-3600000/-600000 -P  > testplot.ps
grdcontour out3.nc -Cshades.cpt -Q$(echo "${resolution} * 2" | bc) -D -L0.45/0.55 -JX-12.8/-24 -R-700000/900000/-3600000/-600000 -P  > contours_outline.txt

# this converts the outline file to be in cartesian coordinates


awk --field-separator='\t' '{if (NR > 1) print $1, $2}' contours_outline.txt > ${outline_file}

fi

min_ss=50000 # basal shear stress for areas outside of the domain
resolution=5000 # spatial resolution
x_resolution=${resolution}
y_resolution=${resolution}


# first, make the basal shear stress file


if [ "${execute_ss}" = "y" ]
then

shear_stress_xyz_file=shear_stress/domains_ss.txt






xmin=$(sort -nk 1 ${outline_file} | head -n 1 | awk -v res=${resolution} '{print int(($1 -20*res) / res)*res}' )
xmax=$(sort -nk 1 ${outline_file} | tail -n 1 | awk -v res=${resolution} '{print int(($1 +20*res) / res)*res}')
ymin=$(sort -nk 2 ${outline_file} | head -n 1 | awk -v res=${resolution} '{print int(($2 -20*res) / res)*res}')
ymax=$(sort -nk 2 ${outline_file} | tail -n 1 | awk -v res=${resolution} '{print int(($2 +20*res) / res)*res}')

echo ${xmin}
echo ${xmax}
echo ${ymin}
echo ${ymax}

#blockmean -R${xmin}/${xmax}/${ymin}/${ymax} -I${resolution} ${shear_stress_xyz_file} > bm.out

ss_grid="ss.grd"


surface ${shear_stress_xyz_file} -G${ss_grid} -I${resolution} -Re${xmin}/${xmax}/${ymin}/${ymax} -T0.35  -V 


 grdimage ${ss_grid} -JX6i+ -P > quick2.ps


num_x=$( ncdump -h ${ss_grid} | grep $'\tx = ' | sed -e 's/ //g' | sed 's/.*=//' | sed 's/;//g' )
num_y=$( ncdump -h ${ss_grid} | grep $'\ty = ' | sed -e 's/ //g' | sed 's/.*=//' | sed 's/;//g' )

echo $num_x $num_y

ncdump  -v x  ${ss_grid}  | awk '/data:/{y=1;next}y' | sed -e 's/[x={}; ]//g' | tr -d '\n' | sed -e 's/,/\n/g' > x_values.txt

ncdump  -v y  ${ss_grid}  | awk '/data:/{y=1;next}y' | sed -e 's/[y={}; ]//g' | tr -d '\n' | sed -e 's/,/\n/g' > y_values.txt

xyz_file="index_ss.xyz"

# the -Cf option outputs it into array indices for reading into a Fortran program

grd2xyz ${ss_grid} -V -sa -Cf -N${min_ss} > ${xyz_file}

even_grid_file="even_grid_ss.dat"

${binary_path}nearest_int ${num_x} ${num_y} ${resolution} ${xyz_file} ${even_grid_file}

reduced_ss_file="reduced_ss.xyz"

# code is in the same directory as the main icesheet program
${binary_path}reduce_dem ${even_grid_file} ${outline_file} ${reduced_ss_file}


binary_ss_file="ss.bin"

sed -e "s/${reduced_ss_file}/${binary_ss_file}/g" elev_parameters.txt | awk '{ if (NR > 1) print int($1); else print $0}' > ss_parameters.txt

xmin=$( awk '{ if (NR == 2) print $1}' ss_parameters.txt )
xmax=$( awk '{ if (NR == 3) print $1}' ss_parameters.txt )
ymin=$( awk '{ if (NR == 4) print $1}' ss_parameters.txt )
ymax=$( awk '{ if (NR == 5) print $1}' ss_parameters.txt )



xyz2grd ${reduced_ss_file} -G${binary_ss_file}=bf -I${resolution} -R${xmin}/${xmax}/${ymin}/${ymax} -N${min_ss}

grdimage ${binary_ss_file}=bf -P -JX6i+ > quick.ps



#rm ${even_grid_file} ${reduced_ss_file} ${ss_grid} ${xyz_file} x_values.txt y_values.txt ${shear_stress_xyz_file} bm.out

fi


# now make the topo grid

resolution=5000



# change the location to whatever netcdf file contains your topography


# uncomment the first time you run this




# output grid is in metres (change the -A option to -Ak for kilometers)

area_grid=area.grd

echo ${x_resolution}/${y_resolution}

# uncomment the first time you run this, it takes a lot of time!

execute_area=n

if [ "${execute_area}" = "y" ]
then
ncks -v bed ${greenland_netcdf}  bed.nc
grd2xyz bed.nc -bo -sa -R${xmin}/${xmax}/${ymin}/${ymax} > bed.bin

blockmean bed.bin -bi -I${resolution} -R${xmin}/${xmax}/${ymin}/${ymax}  > area.xyz

fi

surface area.xyz -G${area_grid} -I${resolution} -R${xmin}/${xmax}/${ymin}/${ymax} -T0.35  -V 

makecpt -Cglobe -T-10000/10000/50   > shades.cpt
grdimage -Cshades.cpt -JX6i+ area.grd -P > modern_topo.ps



# the problem with grdproject is that the dx and dy are not exactly the correct x/y spacings, probably due to a precision error. It also
# doesn't center things on the origin. I've created a program that will fix these issues

# get the x and y values. Note that this will not work in DASH shell (which is the default shell in Ubuntu and Linux Mint)

num_x=$( ncdump -h ${area_grid} | grep $'\tx = ' | sed -e 's/ //g' | sed 's/.*=//' | sed 's/;//g' )
num_y=$( ncdump -h ${area_grid} | grep $'\ty = ' | sed -e 's/ //g' | sed 's/.*=//' | sed 's/;//g' )

echo $num_x $num_y

ncdump  -v x  ${area_grid}  | awk '/data:/{y=1;next}y' | sed -e 's/[x={}; ]//g' | tr -d '\n' | sed -e 's/,/\n/g' > x_values.txt

ncdump  -v y  ${area_grid}  | awk '/data:/{y=1;next}y' | sed -e 's/[y={}; ]//g' | tr -d '\n' | sed -e 's/,/\n/g' > y_values.txt




# output to an ASCII file (warning, might be huge)

xyz_file="index_topo.xyz"



# the -Cf option outputs it into array indices for reading into a Fortran program

grd2xyz ${area_grid} -V -sa -Cf > ${xyz_file}



# fSince things are not in a nice grid starting at 0,0, this program converts it.

even_grid_file="even_grid.dat"

${binary_path}nearest_int ${num_x} ${num_y} ${resolution} ${xyz_file} ${even_grid_file}

reduced_dem_file="modern_topo.xyz"


${binary_path}reduce_dem ${even_grid_file} ${outline_file} ${reduced_dem_file}

binary_dem_file="modern_topo.bin"

grdreformat ${area_grid} ${binary_dem_file}=bf 

sed -e "s/${reduced_dem_file}/${binary_dem_file}/g" elev_parameters.txt | awk  '{ if (NR > 1) print int($1); else print $0}' > elev_parameters_temp.txt



mv -f elev_parameters_temp.txt elev_parameters.txt

xmin=$( awk '{ if (NR == 2) print $1}' elev_parameters.txt )
xmax=$( awk '{ if (NR == 3) print $1}' elev_parameters.txt )
ymin=$( awk '{ if (NR == 4) print $1}' elev_parameters.txt )
ymax=$( awk '{ if (NR == 5) print $1}' elev_parameters.txt )






# test plot

plot_topo="base_topo3.ps"

#makecpt -Cetopo1 -T-11000/8500/50   > shades.cpt

#blockmean ${topo_file} -I${grid_spacing} -R${xmin}/${xmax}/${ymin}/${ymax} > mean_data.txt

smoothing_factor=1

echo "${resolution} * ${smoothing_factor}" | bc 

#surface  ${area_grid3} -I$(echo "${resolution} * ${smoothing_factor}" | bc )/$(echo "${resolution} * ${smoothing_factor}" | bc ) -R${xmin}/${xmax}/${ymin}/${ymax}   -T1 -Gtopography_smooth.grd

#grd2xyz topography_smooth.grd  > mean_data.txt


#triangulate  mean_data.txt -Gtopo.grd -I${grid_spacing} -R${xmin}/${xmax}/${ymin}/${ymax} 

map_width=0.5c # likely arbitrary
xtext="x"

xtickint=200000
xsubtickint=100000

ytext="y"

ytickint=200000
ysubtickint=100000


grdimage ${binary_dem_file} -Cshades.cpt -X5 -Y7 -JX15/15 -K -R${xmin}/${xmax}/${ymin}/${ymax}  -Ba"${xtickint}"f"${xsubtickint}":"${xtext}":/a"${ytickint}"f"${ysubtickint}":"${ytext}":WeSn -V -P  > $plot_topo


psxy ${outline_file} -K -O -R -JX -Wthickest,yellow >> ${plot_topo}
psscale -X-3 -Y-1 -D9c/-2c/15c/0.5ch -O -Ba1000:"Topography (m)": -Cshades.cpt  >> $plot_topo

exit 0

exit 0
rm ${reduced_dem_file} ${even_grid_file} ${smaller_grid_file} x_values.txt y_values.txt ${xyz_file} area.grd
