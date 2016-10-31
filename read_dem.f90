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

      module DEMVar
         real,allocatable :: DEM(:,:)
         real :: xmin,ymin,dx,dy,noval,R, threshold
         integer :: nx,ny
      end module



      subroutine  READ_DEM()
      use DEMVar


      implicit none

      integer :: i,j
      real :: u,v, elevation

	character(len=255) :: filename

	call getarg(1,filename)

	open(unit=40, file=filename, access="sequential", form="formatted", status="old")

	! header of the even grid file
	read(40,*) nx, ny, xmin, ymin, dx, dy, noval



     Allocate(dem(nx,ny))



      Do i=1,nx
     		Do j=1,ny

           read(40,*) elevation
           if (elevation==noval) then
                   dem(i,j)=noval
           else
                    dem(i,j)=elevation

           endif



        End do

     End do


     close(40)

    End
