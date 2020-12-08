#! /bin/bash

# This projection is the Antarctica Polar Stereographic projection, which is used by BEDMAP2
# If you do change it, you need to re-run the topography map as well!

#WGS84 Bounds: -180.0000, -90.0000, 180.0000, -60.0000
#Projected Bounds: -0.0000, -3333134.0276, 0.0000, 0.0000

center_longitude=0
center_latitude=-90
resolution=5 # grid resolution, in km!.


# corner points of the grid (if we don't use this, gmt assumes a global grid, which will be huge!
# west corresponds to the bottom left corner, east corresponds to the top right corner
# probably easiest to pick off the cordinates off Google Earth, in a really zoomed out view


#west_latitude=-1
#west_longitude=-12
#east_latitude=65
#east_longitude=108

map_width=14c

shift_up="-Y5"

scale_x_shift="-X-2"

#J_options="-JA${center_longitude}/${center_latitude}/${map_width}"
#R_options="-R${west_longitude}/${west_latitude}/${east_longitude}/${east_latitude}r"

# for transforming to lat/long

min_lat=-90
max_lat=-60 # was -40 in old script
min_lon=-180
max_lon=180

horizon=-71

map_scale_factor=48000000 # should be roughly 14 cm

J_options="-JS${center_longitude}/${center_latitude}/${map_width}"
#J_options="-Js${center_longitude}/${center_latitude}/-71/1:1"
J_options_project="-Js${center_longitude}/${center_latitude}/${horizon}/1:${map_scale_factor}"

special_projection="y"

R_options="-R${min_lon}/${max_lon}/${min_lat}/${max_lat}"

xmin=-3335000
xmax=3335000
ymin=-3335000
ymax=3335000
echo ${J_options}
echo ${R_options}

echo ${xmin} ${ymin}
echo ${xmax} ${ymax}

mapproject << END    ${R_options} ${J_options_project} -Fe -I -C
${xmin} ${ymin}
${xmax} ${ymax}
0 0
END

# derived from the above mapproject call
west_latitude=-48.4415197517
west_longitude=-135
east_latitude=-48.4415197517
east_longitude=45
#R_options="-R${west_longitude}/${west_latitude}/${east_longitude}/${east_latitude}r"



#mapproject << END    ${R_options} ${J_options} -Fe  -C
#${west_longitude} ${west_latitude}
#${east_longitude} ${east_latitude}
#0 -90
#END


###############

# shear stress domain file

shear_stress_domain_file=shear_stress_domains3.gmt
