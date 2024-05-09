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
    use iso_fortran_env, only: int64
    implicit none
       
    real :: x
    real :: y
    complex :: z
    real :: im

    real :: c1, c2, c3

    real, parameter :: x_start = -2
    real, parameter :: x_quit = 2
    real, parameter :: y_start = -2
    real, parameter :: y_quit = 2
    real, parameter :: im_start = -2
    real, parameter :: im_quit = 2

    integer :: i
    integer :: j
    integer :: k

    integer, parameter :: x_steps = 2000
    integer, parameter :: y_steps = 2000
    integer, parameter :: im_steps = 2000

    real, parameter :: x_step = (abs(x_start) + abs(x_quit)) / x_steps
    real, parameter :: y_step = (abs(y_start) + abs(y_quit)) / y_steps
    real, parameter :: im_step = (abs(im_start) + abs(im_quit)) / im_steps

    integer(kind=int64) :: start_time
    integer(kind=int64) :: end_time
    integer(kind=int64) :: tick_rate

    type(tiff_logluv), allocatable :: tiff_obj

    call system_clock(count_rate=tick_rate)
        
    !$OMP PARALLEL DO PRIVATE (x, y, i, j, im, tiff_obj, start_time, end_time)
    do k=1, 1

        allocate(tiff_obj)

        call system_clock(start_time)

        tiff_obj%x_size = x_steps
        tiff_obj%y_size = y_steps

        !im = im_start + (k * im_step)
        im = 0
        
        call tiff_obj%open(k)
        call tiff_obj%header()
        
        do i=1, y_steps
            y = y_quit - (i * y_step)

            call tiff_obj%begin()
                
            !$OMP PARALLEL DO PRIVATE (x, j, z, c1, c2, c3) SHARED (y, tiff_obj)
            do j=1, x_steps
                x = x_start + (j * x_step)
                        
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
            end do
            !$OMP END PARALLEL DO

            call tiff_obj%commit()
        end do

        call tiff_obj%end()

        call tiff_obj%close()

        call system_clock(end_time)

        print *, tiff_obj%filename
        print *, 'elapsed: ', real(end_time-start_time)/real(tick_rate), ' seconds'
        print *, ''

        deallocate(tiff_obj)

    end do
    !$OMP END PARALLEL DO
end program plot

