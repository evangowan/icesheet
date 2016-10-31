!	copyright Evan. J. Gowan, 2013, 2016
!
!	This file is part of ICESHEET 1.0
!
!	ICESHEET is free software: you can redistribute it and/or modify
!	it under the terms of the GNU General Public License as published by
!	the Free Software Foundation, version 3 of the License
!
!	ICESHEET is distributed in the hope that it will be useful,
!	but WITHOUT ANY WARRANTY; without even the implied warranty of
!	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!	GNU General Public License for more details.
!
!	You should have received a copy of the GNU General Public License
!	along with ICESHEET.  If not, see <http://www.gnu.org/licenses/>.

PROGRAM icesheet

	! last change: January 11, 2016

	! Written by Evan J. Gowan (evangowan@gmail.com)


	! This program had its beginnings in early 2011, and became a major part of my PHD project at the Australian National University, working on the
	! Laurentide Ice Sheet. I had to overcome a lot of hurdles, such as my inexperience with programming, and
	! some major problems related to detecting when the individual flowlines overlapped. I finally got a fairly
	! stable version of the program working in August 2013, and that was used for completing my PHD project.
	! After the finishing the corrections of my thesis (May 2015), I went back to this program to get it in a nice
	! enough shape to publish. I fixed several bugs in it, including a bug in the ice thickness at the margin
	! if it is below sea level, and some problems related to places where the ice sheet is extremely flat.

	! I hope that this program proves useful to make quick estimations of ice sheet geometry through a glacial cycle.
	
	! if you use this program, cite:

	! ICESHEET 1.0: A program to produce paleo-ice sheet models with minimal assumptions
	! Evan J. Gowan, Paul Tregoning, Anthony Purcell, James Lea, Oscar J. Fransner, Riko Noormets, and J. A. Dowdeswell
	! submitted to Geoscience Model Development

	use global_parameters
	
	use grids
	IMPLICIT NONE

	CHARACTER*80 :: filename, argname
	integer :: counter1, counter2, counter3, counter4, istat, boundary_counter
	double precision :: grid_x, grid_y, latitude, longitude, x, y


	open(unit = 11, file="params.txt", form="formatted", access="sequential", status="old")


	read(11,'(A)') filename ! ice margin file name
	read(11,'(A)') elevation_parameters_filename ! for elevation file
	read(11,'(A)') shear_stress_parameters_filename ! for shear stress file
	read(11,*) elevation_interval ! interval for elevation contours
	read(11,*) minimum_spacing ! minimum space between points on the contour must be in km

	close(unit=11)

	write(6,*) "working on: ", filename





	write(6,*) "before read_elevation_files"
	call read_elevation_files() ! initialize elevation file parameters

	write(6,*) "before read_ss_params"
	call read_ss_files()


	!stop


	! subroutine that reads in the ice file, converts the boundary to distance from the central point, and find the local slope
	write(6,*) "before read_icefile"
	call read_icefile(filename)

	
	write(6,*) "before read_icefile"
	call find_flowline()

	call end_elev()
	call end_ss()

	deallocate( interior_direction, x_distance, y_distance, stat=istat)
	if (istat /= 0) THEN
		WRITE(6,*) "deallocation error for in icesheet"
		stop
	ENDIF

	STOP
END PROGRAM icesheet
