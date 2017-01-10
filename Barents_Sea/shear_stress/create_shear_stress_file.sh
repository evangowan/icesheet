#! /bin/bash

# The easiest way to create a shear stress file is to partition the area of interest into places where you want to be able to vary the shear stress.

# The way I have done this is to create a shapefile that partitions the area as desired. I then exported the shapefile into a CSV file.
# from QGIS:
# > Open shapefile (barents_sea_shear_stress.shp, in this folder)
# > Layer dropdown menu > save as
# > in the options, change "Symbology Export" to "feature symbology", and "Geometry" as "AS_WKT".
# > save file as "polygons.csv"

binary_path="."

csv_file=polygons.csv

if [ ! -f "${csv_file}" ]; then
	echo "missing shear_stress/polygons.csv"
	echo "follow the instructions in shear_stress/create_shear_stress_file.sh"
	exit 0
fi

gawk 'BEGIN { FPAT = "([^,]+)|(\"[^\"]+\")"} {if(NR > 1) {print ">", NR-1; print $1} }' ${csv_file} | sed -e 's/\"MULTIPOLYGON (((//g'  | sed -e 's/)))\"//g' | sed -e 's/,/\n/g' | sed -e 's/(//g' | sed -e 's/)//g' |  sed -e 's/\"POLYGON //g'  |  sed -e 's/\"//g' > gmt_file.txt

gawk 'BEGIN { FPAT = "([^,]+)|(\"[^\"]+\")"} {if(NR > 1) {print NR-1, $4} }' ${csv_file}  > shear_stress_domain_values.txt




#min_shear_stress=5000

#filter_width=5000

#${binary_path}/create_ss_grid ${filter_width} ${filter_width}

#${binary_path}/convert_grid shear_stress_domain_values.txt



