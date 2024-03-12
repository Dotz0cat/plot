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

module test_functions
    implicit none

    private

    public :: identity
    public :: taubin_heart
    public :: taubin_heart_gradient
    public :: unit_circle

    interface identity
        module complex function identity(x, y) result(z)
            real, intent(in) :: x, y
        end function identity
    end interface identity

    interface taubin_heart
        module complex function taubin_heart(x, y) result(z)
            complex, intent(in) :: x, y
        end function taubin_heart
    end interface taubin_heart

    interface taubin_heart_gradient
        module complex function taubin_heart_gradient(x, y) result(z)
            complex, intent(in) :: x, y
        end function taubin_heart_gradient
    end interface taubin_heart_gradient

    interface unit_circle
        module complex function unit_circle(x, y) result(z)
            complex, intent(in) :: x, y
        end function unit_circle
    end interface unit_circle

contains
    module procedure identity
        z = complex(x, y)
    end procedure identity

    module procedure taubin_heart
        z = ((x**2) + (y**2) - 1)**3 - ((x**2) * (y**3))
    end procedure taubin_heart

    module procedure taubin_heart_gradient
        z%re = 65.0*(x**5.0) + 12.0 * ((x**3.0) * (y**2.0)) - 12.0 * (y**3.0) &
            & + 6.0 * x + 6.0 * (x * (y**4.0)) + 12.0 * (x * (y**2.0)) &
            & - 2.0 * (x * (y**3.0))
                
        z%im = 65.0*(y**5.0) + 12.0 * ((y**3.0) * (x**2.0)) - 12.0 * (x**3.0) &
            & + 6.0 * y + 6.0 * (y * (x**4.0)) + 12.0 * (y * (x**2.0)) &
            & - 3.0 * ((x**2.0) * (y**2.0))
    end procedure taubin_heart_gradient

    module procedure unit_circle
        z = x**2 + y**2 - 1
    end procedure unit_circle
end module test_functions
