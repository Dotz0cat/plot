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

submodule (domain_color) domain_color_luv_space
    implicit none

    real, save, dimension(3) :: d65 = &
        & [0.95047, 1.0, 1.08883]

    real, save, dimension(3, 3) :: srgb_conversion_matrix = reshape( &
        & [3.2404542, -1.5371385, -0.4985314, &
        & -0.9692660, 1.8760108, 0.0415560, &
        & 0.0556434, -0.2040259, 1.0572252], &
        & [3, 3] )


contains
    module procedure domain_color_luv
        real :: hue
        real :: chroma
        real :: lum
        real :: u_vec
        real :: v_vec

        hue = arg

        chroma = 50.0 * (sin(mod)**2.0) + 50.0

        u_vec = chroma * cos(hue)
        v_vec = chroma * sin(hue)

        lum = 25.0 * (sin(mod)**2.0) + 25.0
                
        l = lum
        u = u_vec
        v = v_vec
    end procedure domain_color_luv

    module procedure domain_color_srgb
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

        xyz_vec(2) = piecewise_luv(lum)
                
        d = xyz_vec(2) * ((39.0 * lum)/(v_vec + (13.0 * lum * v_naught)) - 5)
        b2 = -5.0 * xyz_vec(2)
        a = (1.0/3.0) * ((52.0 * lum)/(u_vec + (13.0 * lum * u_naught)) - 1)
        c = -(1.0/3.0)

        xyz_vec(1) = (d - b2)/(a - c)
        xyz_vec(3) = (xyz_vec(1) * a) + b2
                
        rgb_vec = matmul(xyz_vec,  conv_vec)

        r = rgb_vec(1)
        g = rgb_vec(2)
        b = rgb_vec(3)

    end procedure domain_color_srgb

    module procedure domain_color_gamma_srgb
        call domain_color_srgb(mod, arg, r, g, b)

        call srgb_companding(r)
        call srgb_companding(g)
        call srgb_companding(b)
    end procedure domain_color_gamma_srgb

    module real function piecewise_luv(L) result(y)
        real, intent(in) :: L
 
        real :: kappa = 24389.0/27.0
        real :: delta = 216.0/24389.0
        if (L .gt. (kappa * delta)) then
            y = ((L + 16.0)/116.0)**3
        else
            y = L/kappa
        end if
    end function piecewise_luv

    module subroutine srgb_companding(v)
        real, intent(inout) :: v

        if (v .gt. 0.0031308) then
            v = ((1.055 * v)**(1.0/2.4)) - 0.055
        else
            v = 12.92 * v
        end if
    end subroutine srgb_companding

end submodule domain_color_luv_space
