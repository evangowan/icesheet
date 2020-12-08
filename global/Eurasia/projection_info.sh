#! /bin/bash

# For Lambert azimuthal projection. These paramters approximately match what Hughes et al (2016) used for the dated margins
# If you do change it, you need to re-run the topography map as well!

center_longitude=0
center_latitude=90
resolution=5 # grid resolution, in km!.


# corner points of the grid (if we don't use this, gmt assumes a global grid, which will be huge!
# west corresponds to the bottom left corner, east corresponds to the top right corner
# probably easiest to pick off the cordinates off Google Earth, in a really zoomed out view

# note that this range may need to be modified if the MIS6 limits are used, this has not been digitized yet (from Svendsen et al 2004)

west_latitude=47
west_longitude=-12
east_latitude=65
east_longitude=108

map_width=14c

shift_up="-Y5"

scale_x_shift="-X-2"

J_options="-JA${center_longitude}/${center_latitude}/${map_width}"
R_options="-R${west_longitude}/${west_latitude}/${east_longitude}/${east_latitude}r"


# shear stress domain file

shear_stress_domain_file=shear_stress_domains.gmt
