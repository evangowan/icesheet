#! /bin/bash

# For Lambert azimuthal projection. These parameters cover the entire range of places where North American ice sheets covered, so it shouldn't need to be changed.
# If you do change it, you need to re-run the topography map as well!

center_longitude=-73
center_latitude=-45
resolution=5 # grid resolution, in km!


# corner points of the grid (if we don't use this, gmt assumes a global grid, which will be huge!
# west corresponds to the bottom left corner, east corresponds to the top right corner
# probably easiest to pick off the cordinates off Google Earth, in a really zoomed out view
west_latitude=-57
west_longitude=-80
east_latitude=-26
east_longitude=-65

# cut region, just for the topo map creation

extreme_west=-80
extreme_east=-55
extreme_south=-60
extreme_north=-20

map_width=8c

shift_up="-Y5"

scale_x_shift="-X-5"

J_options="-JA${center_longitude}/${center_latitude}/${map_width}"
J_options_project="-JA${center_longitude}/${center_latitude}/${map_width}"
R_options="-R${west_longitude}/${west_latitude}/${east_longitude}/${east_latitude}r"

#special_projection="y" # just to make sure it is relative to the center of the domain

# shear stress domain file

shear_stress_domain_file=shear_stress_domains.gmt
