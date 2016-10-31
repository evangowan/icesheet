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

program nearest_int


	use bicubic
	implicit none

	double precision :: x, y, z, y_store, x_min, x_max, x_store, x_sum, y_sum, elevation_read
	


	character(len=120) :: read_param
	character(len=255) :: infile, outfile

	integer :: istat, counter,  i, j, x_counter, y_counter, x_out_count, y_out_count







	call getarg(1,read_param)
	read(read_param,*) x_count
	call getarg(2,read_param)
	read(read_param,*) y_count

	call getarg(3,read_param)
	read(read_param,*) grid_spacing

	call getarg(4,infile)
	call getarg(5,outfile)	

	allocate(x_values(x_count), y_values(y_count), elevation_values(x_count,y_count), stat=istat)
	if(istat /=0) THEN
		write(6,*) "problem allocating x and y values arrays"
		stop
	endif


	open(unit=10, file="x_values.txt", form="formatted", access="sequential", status="old")

	do counter = 1, x_count
		read(10,*) x_values(counter)
	end do

	close(unit=10)


	open(unit=20, file="y_values.txt", form="formatted", access="sequential", status="old")

	do counter = 1, y_count
		read(20,*) y_values(counter)
	end do

	close(unit=20)


	! find dx and dy

	x_sum = 0

	do counter = 2, x_count

		x_sum = x_sum + abs(x_values(counter)-x_values(counter-1))

	end do

	dx = x_sum / dble(x_count-1)

	y_sum = 0

	do counter = 2, y_count

		y_sum = y_sum + abs(y_values(counter)-y_values(counter-1))

	end do

	dy = y_sum / dble(y_count-1)

	write(6,*) dx, dy

	! for the purposes interpolation, use the average spacing
	bi_grid_spacing = (dx + dy) / 2.

	! read in the grid file

	open(unit=30, file=infile, form="formatted", access="sequential", status="old")

	elevation_values = noval

	do

		read(30,*,iostat=istat) i, j, elevation_read
		if(istat /=0) THEN
			exit
		endif

		! GMT outputs the y index in reverse order for some reason (i.e. the largest y value is given index 1)

		elevation_values(i,y_count-j+1) = elevation_read



	end do
	
	close(unit=30)

	open(unit=40, file=outfile, form="formatted", access="sequential", status="replace")

	
	x_out_count = int((x_values(x_count) - x_values(1)) / dble(grid_spacing)) - 1
	y_out_count = int((y_values(y_count) - y_values(1)) / dble(grid_spacing)) - 1

!	write(6,*) x_out_count, y_out_count


	x = dble(ceiling(( x_values(1)) / dble(grid_spacing)) * grid_spacing) -  dble(grid_spacing)


	! write file header

	! nx,ny,xmin,ymin,dx,dy,noval

	write(40,*) x_out_count, y_out_count, x +  dble(grid_spacing), &
		dble(ceiling(( y_values(1)) / dble(grid_spacing)) * grid_spacing), grid_spacing, grid_spacing, noval

	do x_counter = 1, x_out_count

		x = x +  dble(grid_spacing)
		y = dble(ceiling(( y_values(1)) / dble(grid_spacing)) * grid_spacing) -  dble(grid_spacing)

		do y_counter = 1,y_out_count 


			y = y + dble(grid_spacing)

			write(40,*)  nint(elevation(x,y))
		end do
	end do

	close(unit=40)

	deallocate(x_values, y_values,elevation_values)

end program nearest_int
		

