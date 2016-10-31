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

program reduce_dem

! this program reads in the DEM and outline from the previous steps, and outputs the parameters needed for the ice sheet program (assuming you want
! to use Grid2DInterpolator, which is a bilinear interpolation method

! input parameters include the contour file (in Cartesian coordinates), and the headered xyz large scale dem

! requires files READ_DEM.o 

	use DEMVar
	implicit none
	character(len=255) :: filename_contour

	integer :: istat, start_x_counter, start_y_counter, end_x_counter, end_y_counter, x_counter, y_counter

	real :: min_contour_x, min_contour_y, max_contour_x, max_contour_y, x, y, min_output_x, min_output_y, max_output_x, &
		  max_output_y, x_offset, y_offset


	character(len=255), parameter :: elev_parameter_file = "elev_parameters.txt"
	character(len=255) :: output_dem
	! first, read in the DEM. the DEM file is the first command line argument

	call READ_DEM

	! second, read in the contour file, and find the minimum and maximum x and y points

	min_contour_x = 9.e10
	min_contour_y = 9.e10
	max_contour_x = -9.e10
	max_contour_y = -9.e10


	call getarg(2,filename_contour)
	call getarg(3,output_dem)

	open(unit=30, file=filename_contour, access="sequential", form="formatted", status="old")

	read_contour: do

		read(30,*,iostat=istat) x, y
		if(istat /=0) THEN
			exit read_contour
		end if

		if(x > max_contour_x) THEN
			max_contour_x = x
		endif

		if(x < min_contour_x) THEN
			min_contour_x = x
		endif

		if(y > max_contour_y) THEN
			max_contour_y = y
		endif

		if(y < min_contour_y) THEN
			min_contour_y = y
		endif


	end do read_contour

!	min_contour_x = min_contour_x - 3*dx
!	max_contour_x = max_contour_x - 3*dx
!	min_contour_y = min_contour_y - 3*dy
!	max_contour_y = max_contour_y - 3*dy
	close(unit=30)

	x_offset = xmin - real(floor((xmin / dx)))*dx
	y_offset = ymin - real(floor((ymin / dy)))*dy


	! find the range of interest
	write(6,*) dx
	min_output_x = floor((min_contour_x-x_offset) / real(dx)) * dx + x_offset - 10*dx
	min_output_y = floor((min_contour_y-y_offset) / real(dy)) * dy + y_offset - 10*dx
	max_output_x = ceiling((max_contour_x-x_offset) / real(dx)) * dx + x_offset + 10*dy
	max_output_y = ceiling((max_contour_y-y_offset) / real(dy)) * dy + y_offset + 10*dy




	open(unit=50, file=output_dem, access="sequential", form="formatted", status="replace")

	start_x_counter = nint((min_output_x - xmin)/dx) + 1
	start_y_counter = nint((min_output_y - ymin)/dy) + 1
	end_x_counter = nint((max_output_x - xmin)/dx) + 1
	end_y_counter = nint((max_output_y - ymin)/dy) + 1




	do x_counter = start_x_counter, end_x_counter, 1
		do y_counter = start_y_counter, end_y_counter, 1

			if(x_counter <= 0 .or. y_counter <= 0 .or. x_counter > nx .or. y_counter > ny ) THEN
				write(50,*) real(x_counter-1)*dx + xmin, real(y_counter-1)*dy+ymin, noval
			else

				write(50,*) real(x_counter-1)*dx + xmin, real(y_counter-1)*dy+ymin, DEM(x_counter,y_counter)

			endif

		end do
	end do

	close(unit=50)
	!write out the parameters that need to go into the elev file

	open(unit=60, file=elev_parameter_file, access="sequential", form="formatted", status="replace")


	write(60,*) trim(adjustl(output_dem)) 
	write(60,*)  min_output_x
	write(60,*)  max_output_x
	write(60,*)  min_output_y 
	write(60,*)  max_output_y 
	write(60,*)  nint(dx) ! must be an integer

	close(unit=60)



end program reduce_dem
