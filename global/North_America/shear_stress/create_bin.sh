#! /bin/bash

# Ice margin file for the desired time
margin_file=/scratch/users/egowan-local/topo/RTopo/18k.gmt

bin_path="../../../" # leave blank if create_ss_grid is in ${PATH}

# The GMT formatted text file needs to be created before running this script.
# Steps:
# 1) Open shear_stress_domains_reproj.shp in QGIS
# 2) Right click on "shear_stress_domains_reproj " in the Layers dialog and click on "save as"
# 3) In format, select "Generic Mapping Tools (GMT)"; in save as, find the path to the current directory and use the file name "shear_stress_domains.gmt"; 
#    in Symbology export, select "feature symbology"; then hit save 

domain_gmt_file=shear_stress_domains.gmt

# For Lambert azimuthal projection. These parameters cover the entire range of places where North American ice sheets covered, so it shouldn't need to be changed.
# If you do change it, you need to re-run the topography map as well!

center_longitude=-94
center_latitude=60
resolution=5 # grid resolution, in km!

# corner points of the grid (if we don't use this, gmt assumes a global grid, which will be huge!
# west corresponds to the bottom left corner, east corresponds to the top right corner
# probably easiest to pick off the cordinates off Google Earth, in a really zoomed out view
west_latitude=25
west_longitude=-135
east_latitude=58
east_longitude=3

map_width=15c

mapproject << END    -R${west_longitude}/${west_latitude}/${east_longitude}/${east_latitude}r -JA${center_longitude}/${center_latitude}/${map_width} -F  > corners.txt
${west_longitude} ${west_latitude}
${east_longitude} ${east_latitude}
END

spacing=${resolution}000

r1=$(awk '{if (NR==1) print $1}' corners.txt)
r2=$(awk '{if (NR==2) print $1}' corners.txt)
r3=$(awk '{if (NR==1) print $2}' corners.txt)
r4=$(awk '{if (NR==2) print $2}' corners.txt)

# round the numbers, should only need to do this for the top left corner, really

x_min=${r1}
y_min=${r3}
x_max_temp=$(printf '%.0f\n' $(echo "scale=2; ${r2} / ${spacing}" | bc ) )
x_max=$(echo "${x_max_temp} * ${spacing}" | bc)
y_max_temp=$(printf '%.0f\n' $(echo "scale=2; ${r4} / ${spacing}" | bc ) )
y_max=$(echo "${y_max_temp} * ${spacing}" | bc)





bin_file="shear_stress.bin"

makecpt -Cwysiwyg -T0/200000/10000 -I > shades_shearstress.cpt

cat << END_CAT > ss_parameters.txt
shear_stress.bin
${x_min}
${x_max}
${y_min}
${y_max}
${spacing}
END_CAT

mapproject << END    -R${west_longitude}/${west_latitude}/${east_longitude}/${east_latitude}r -JA${center_longitude}/${center_latitude}/${map_width} -F >> ss_parameters.txt
${center_longitude} ${center_latitude}
END



# convert the GMT file into a binary grid

${bin_path}create_ss_grid ${domain_gmt_file} domains_max.txt #adjust_0.txt 25000

nc_file=shear_stress.nc

xyz2grd shear_stress_grid.txt -I${spacing} -R${x_min}/${x_max}/${y_min}/${y_max} -G${nc_file}

# plot the file

plot="shear_stress.ps"

grdimage ${nc_file} -Y12  -R${x_min}/${x_max}/${y_min}/${y_max}  -JX${map_width}/0 -K -P -Cshades_shearstress.cpt -V -nb > ${plot}


mapproject << END    -R${west_longitude}/${west_latitude}/${east_longitude}/${east_latitude}r -JA${center_longitude}/${center_latitude}/${map_width} -F -C > corners.txt
${west_longitude} ${west_latitude}
${east_longitude} ${east_latitude}
END

r1=$(awk '{if (NR==1) print $1}' corners.txt)
r2=$(awk '{if (NR==2) print $1}' corners.txt)
r3=$(awk '{if (NR==1) print $2}' corners.txt)
r4=$(awk '{if (NR==2) print $2}' corners.txt)

psxy ${domain_gmt_file}  -R${r1}/${r2}/${r3}/${r4} -JX -K -O -P -V -Wthin >> ${plot}

pscoast -Bafg -O -K -R${west_longitude}/${west_latitude}/${east_longitude}/${east_latitude}r -JA${center_longitude}/${center_latitude}/${map_width} -P -Wthin -Di -A5000 -Wthinnest,grey >> ${plot}

psxy ${margin_file}  -R${west_longitude}/${west_latitude}/${east_longitude}/${east_latitude}r -JA${center_longitude}/${center_latitude}/${map_width} -K -O -P -V -Wthickest,white >> ${plot}
psxy ${margin_file}  -R${west_longitude}/${west_latitude}/${east_longitude}/${east_latitude}r -JA${center_longitude}/${center_latitude}/${map_width} -K -O -P -V -Wthin,blue >> ${plot}

psscale -X-1 -Y-3.5 -Dx9c/2c/9c/0.5ch -P -O -Bx100000f20000+l"Shear Stress (Pa)" --FONT_LABEL=14p -Cshades_shearstress.cpt -V  >> $plot



# convert the grid to a format used by ICESHEET (GMT binary)

grdconvert ${nc_file} ${bin_file}=bf 
