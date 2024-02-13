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

        character(len=10) :: header
        character(len=25) :: filename

        integer :: unit

        max_color = 0
        min_color = 0
        
        step = (abs(start) + abs(quit)) / steps
        
        !call pfm_header_compute(header, steps)

        ! $OMP PARALLEL DO PRIVATE (x, y, i, j, filename, unit) FIRSTPRIVATE(header, min_color, max_color)
        do k=1, 1

        !im = start + (k * step)
        im = 0
        
        !call pfm_open(filename, k, unit)
        call tiff_open(filename, k, unit)
        !call pfm_header(unit, header)
        call tiff_header(unit, steps)
        
        do i=1, steps
                y = quit - (i * step)

                call tiff_begin(unit, steps)

                do j=1, steps
                        x = start + (j * step)
                        
                        !z = complex(x, y)
                        
                        z = taubin_heart(complex(x, im), complex(y, im))

                        if (x .gt. -0.01 .and. x .lt. 0.01) then
                                !call pfm_write(unit, 0.0, 0.0, 0.0)
                                call tiff_write(0.0, 0.0, 0.0)

                        else if (y .gt. -0.01 .and. y .lt. 0.01) then
                                !call pfm_write(unit, 0.0, 0.0, 0.0)
                                call tiff_write(0.0, 0.0, 0.0)

                        else
                                call domain_color_luv(abs(z), atan2(z%im, z%re), r, g, b)

                                !call pfm_write(unit, r, g, b)
                                call tiff_write(r, g, b)
                                
                                max_color = max(max_color, r)
                                max_color = max(max_color, g)
                                max_color = max(max_color, b)
                                
                                min_color = min(min_color, r)
                                min_color = min(min_color, g)
                                min_color = min(min_color, b)
                        end if        
                end do

                call tiff_commit(unit)
        end do

        !call pfm_end(unit)
        call tiff_end(unit, steps)

        !call pfm_close(unit)
        call tiff_close(unit)

        print *, filename

        print *, 'max: ', max_color
        print *, 'min: ', min_color

        print *, ''

        end do
        ! $OMP END PARALLEL DO
contains
        subroutine domain_color_lab(mod, arg, r, g, b)
                real, intent(in) :: mod
                real, intent(in) :: arg
                real, intent(out) :: r, g, b

                real :: hue
                real :: chroma
                real :: lum
                real :: a_vec
                real :: b_vec
                real, dimension(3) :: d65 = &
                        & [0.95047, 1.0, 1.08883]
                real, dimension(3) :: d65_scaled = &
                        & [95.047, 100.0, 108.883]

                real, dimension(3) :: rgb_vec, xyz_vec

                real, dimension(3, 3) :: conv_vec = reshape( &
                & [3.2404542, -1.5371385, -0.4985314, &
                & -0.9692660, 1.8760108, 0.0415560, &
                & 0.0556434, -0.2040259, 1.0572252], &
                & [3, 3] )

                hue = arg

                chroma = mod

                a_vec = chroma * cos(hue)
                b_vec = chroma * sin(hue)

                lum = chroma

                xyz_vec(2) = piecewise_lab_y(lum)
                xyz_vec(1) = piecewise_lab(a_vec/500.0 + xyz_vec(2))
                xyz_vec(3) = piecewise_lab(xyz_vec(2) - b_vec/200.0)
                
                xyz_vec(1) = d65_scaled(1) * xyz_vec(1)
                xyz_vec(2) = d65_scaled(2) * xyz_vec(2)
                xyz_vec(3) = d65_scaled(3) * xyz_vec(3)

                rgb_vec = matmul(xyz_vec,  conv_vec)

                call srgb_companding(rgb_vec(1))
                call srgb_companding(rgb_vec(2))
                call srgb_companding(rgb_vec(3))
                
                r = rgb_vec(1)
                g = rgb_vec(2)
                b = rgb_vec(3)

        end subroutine domain_color_lab

        subroutine domain_color_luv(mod, arg, r, g, b)
                real, intent(in) :: mod
                real, intent(in) :: arg
                real, intent(out) :: r, g, b

                real :: hue
                real :: chroma
                real :: lum
                real :: u_vec
                real :: v_vec
                real :: u_naught
                real :: v_naught
                real :: a, b2, c, d
                real, dimension(3) :: d65 = &
                        & [0.95047, 1.0, 1.08883]
                real, dimension(3) :: d65_scaled = &
                        & [95.047, 100.0, 108.883]

                real, dimension(3) :: rgb_vec, xyz_vec

                real, dimension(3, 3) :: conv_vec = reshape( &
                & [3.2404542, -1.5371385, -0.4985314, &
                & -0.9692660, 1.8760108, 0.0415560, &
                & 0.0556434, -0.2040259, 1.0572252], &
                & [3, 3] )

                hue = arg

                chroma = 50.0 * (sin(mod)**2.0) + 50.0

                u_vec = chroma * cos(hue)
                v_vec = chroma * sin(hue)

                lum = 25.0 * (sin(mod)**2.0) + 25.0

                u_naught = (4.0 * d65(1))/(d65(1) + (15.0 * d65(2)) + (3.0 * d65(3)))
                v_naught = (9.0 * d65(2))/(d65(1) + (15.0 * d65(2)) + (3.0 * d65(3)))

                xyz_vec(2) = piecewise_lab_y(lum)
                
                d = xyz_vec(2) * ((39.0 * lum)/(v_vec + (13.0 * lum * v_naught)) - 5)
                b2 = -5.0 * xyz_vec(2)
                a = (1.0/3.0) * ((52.0 * lum)/(u_vec + (13.0 * lum * u_naught)) - 1)
                c = -(1.0/3.0)

                xyz_vec(1) = (d - b2)/(a - c)
                xyz_vec(3) = (xyz_vec(1) * a) + b2
                
                rgb_vec = matmul(xyz_vec,  conv_vec)

                !call srgb_companding(rgb_vec(1))
                !call srgb_companding(rgb_vec(2))
                !call srgb_companding(rgb_vec(3))

                r = rgb_vec(1)
                g = rgb_vec(2)
                b = rgb_vec(3)

        end subroutine domain_color_luv

        real function piecewise_lab(t) result(c)
                real, intent(in) :: t
                
                real :: kappa = 24389.0/27.0
                real :: delta = 216.0/24389.0
                if (t**3.0 .gt. delta) then
                        c = t**3.0
                else
                        c = ((116.0 * t) - 16)/kappa
                end if
        end function piecewise_lab

        real function piecewise_lab_y(L) result(y)
                real, intent(in) :: L
 
                real :: kappa = 24389.0/27.0
                real :: delta = 216.0/24389.0
                if (L .gt. (kappa * delta)) then
                        y = ((L + 16.0)/116.0)**3
                else
                        y = L/kappa
                end if
        end function piecewise_lab_y

        real function piecewise_luv(lum) result(y)
                real, intent(in) :: lum 

                if (lum .gt. 8) then
                        y = ((lum + 16.0)/116.0)**3.0
                else
                        y = lum * (27.0/24389.0)
                end if
        end function piecewise_luv

        subroutine srgb_companding(v)
                real, intent(inout) :: v

                if (v .gt. 0.0031308) then
                        v = ((1.055 * v)**(1.0/2.4)) - 0.055
                else
                        v = 12.92 * v
                end if
        end subroutine srgb_companding

        complex function taubin_heart(x, y) result(z)
                complex, intent(in) :: x, y

                z = ((x**2) + (y**2) - 1)**3 - ((x**2) * (y**3))

        end function taubin_heart

        complex function unit_circle(x, y) result(z)
                complex, intent(in) :: x, y

                z = x**2 + y**2

        end function unit_circle
end program plot

