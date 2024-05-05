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
    use domain_color
    use test_functions
    implicit none
       
    real :: x
    real :: y
    complex :: z
    real :: im

    real :: c1, c2, c3

    real :: start = -2
    real :: quit = 2

    integer :: i
    integer :: j
    integer :: k
    integer :: steps = 2000
        
    real :: step
    real :: max_color
    real :: min_color

    type(tiff_logluv), allocatable :: tiff_obj

    max_color = 0
    min_color = 0
        
    step = (abs(start) + abs(quit)) / steps
        
    !$OMP PARALLEL DO PRIVATE (x, y, i, j, im, tiff_obj) FIRSTPRIVATE(min_color, max_color)
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
                
            !$OMP PARALLEL DO PRIVATE (x, j, z, c1, c2, c3) SHARED (y, tiff_obj) &
            !$OMP & REDUCTION(min: min_color) REDUCTION(max: max_color)
            do j=1, steps
                x = start + (j * step)
                        
                !z = identity(x, y)
                        
                z = taubin_heart(complex(x, im), complex(y, im))
                !z = taubin_heart_gradient(complex(x, im), complex(y, im))

                c1 = 0.0
                c2 = 0.0
                c3 = 0.0

                if (x .gt. -0.01 .and. x .lt. 0.01) then
                    call tiff_obj%write(0.0, 0.0, 0.0, j)
                else if (y .gt. -0.01 .and. y .lt. 0.01) then
                    call tiff_obj%write(0.0, 0.0, 0.0, j)
                else
                    call domain_color_luv(abs(z), atan2(z%im, z%re), c1, c2, c3)

                    call tiff_obj%write(c1, c2, c3, j)
                end if

                    max_color = max(max_color, c1)
                    max_color = max(max_color, c2)
                    max_color = max(max_color, c3)
                        
                    min_color = min(min_color, c1)
                    min_color = min(min_color, c2)
                    min_color = min(min_color, c3)
            end do
            !$OMP END PARALLEL DO

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

