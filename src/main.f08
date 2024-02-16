!
! Copyright (C) 2024  Dotz0cat
!
! This program is free software: you can redistribute it and/or modify
! it under the terms of the GNU General Public License as published by
! the Free Software Foundation, either version 3 of the License, or
! (at your option) any later version.
!
! This program is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU General Public License for more details.
!
! You should have received a copy of the GNU General Public License
! along with this program.  If not, see <http://www.gnu.org/licenses/>.
!

program plot
        use image_out
        use domain_color, only: domain_color_gamma_srgb
        use test_functions
        implicit none
       
        real :: x
        real :: y
        complex :: z
        real :: im

        real :: r, g, b

        real :: start = -2
        real :: quit = 2

        integer :: i
        integer :: j
        integer :: k
        integer :: steps = 2000
        
        real :: step
        real :: max_color
        real :: min_color

        type(tiff), allocatable :: tiff_obj

        max_color = 0
        min_color = 0
        
        step = (abs(start) + abs(quit)) / steps
        
        !$OMP PARALLEL DO PRIVATE (x, y, i, j, tiff_obj) FIRSTPRIVATE(min_color, max_color)
        do k=1, 1

        allocate(tiff_obj)

        tiff_obj%x_size = steps
        tiff_obj%y_size = steps

        !im = start + (k * step)
        im = 0
        
        call tiff_obj%open(k)
        call tiff_obj%header()
        
        do i=1, steps
                y = quit - (i * step)

                call tiff_obj%begin()

                do j=1, steps
                        x = start + (j * step)
                        
                        !z = complex(x, y)
                        
                        z = taubin_heart(complex(x, im), complex(y, im))
                        !z = taubin_heart_gradient(complex(x, im), complex(y, im))

                        if (x .gt. -0.01 .and. x .lt. 0.01) then
                                call tiff_obj%write(0.0, 0.0, 0.0)

                        else if (y .gt. -0.01 .and. y .lt. 0.01) then
                                call tiff_obj%write(0.0, 0.0, 0.0)

                        else
                                call domain_color_gamma_srgb(abs(z), atan2(z%im, z%re), r, g, b)

                                call tiff_obj%write(r, g, b)
                                
                                max_color = max(max_color, r)
                                max_color = max(max_color, g)
                                max_color = max(max_color, b)
                                
                                min_color = min(min_color, r)
                                min_color = min(min_color, g)
                                min_color = min(min_color, b)
                        end if        
                end do

                call tiff_obj%commit()
        end do

        call tiff_obj%end()

        call tiff_obj%close()

        print *, tiff_obj%filename

        print *, 'max: ', max_color
        print *, 'min: ', min_color

        print *, ''
        
        deallocate(tiff_obj)

        end do
        !$OMP END PARALLEL DO
end program plot

