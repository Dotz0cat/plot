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

module domain_color
    implicit none

    private

    public :: domain_color_luv
    public :: domain_color_srgb
    public :: domain_color_gamma_srgb

    interface domain_color_luv
        module subroutine domain_color_luv(mod, arg, l, u, v)
            real, intent(in) :: mod
            real, intent(in) :: arg
            real, intent(out) :: l, u, v
            end subroutine domain_color_luv
    end interface domain_color_luv

    interface domain_color_srgb
        module subroutine domain_color_srgb(mod, arg, r, g, b)
            real, intent(in) :: mod
            real, intent(in) :: arg
            real, intent(out) :: r, g, b
        end subroutine domain_color_srgb
    end interface domain_color_srgb

    interface domain_color_gamma_srgb
        module subroutine domain_color_gamma_srgb(mod, arg, r, g, b)
            real, intent(in) :: mod
            real, intent(in) :: arg
            real, intent(out) :: r, g, b
        end subroutine domain_color_gamma_srgb
    end interface domain_color_gamma_srgb
end module domain_color
