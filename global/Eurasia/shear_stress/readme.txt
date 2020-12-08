--------------------------------
2019/08/13
--------------------------------

I had to do some major fixes to the shapefile, the nodes were not properly aligned. It took some time, but I figured out a way to do it. The new shear stress file is called shear_stress2.shp, which is now in projected coordinates. This is fine because the shear stress domain file is in projected coordinates anyways. The key problem I was having was that the coordinate system for the map and the shapefile were "different" even though they had the same definition. Rounding errors.


I have made some adjustments to the file to make use of Richard Gyllencreutz sediment thickness maps. The maps are fairly crude, but give a rough estimate of where continuous sediments are (and therefore where it is likely will have lower shear stress. Most changes are only in Scandinavia, and are relatively minor.

New shear stress domains start at 160
