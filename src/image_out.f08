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

module image_out
        implicit none

        private

        public :: pfm_write
        public :: pfm_header
        public :: pfm_header_compute
        public :: pfm_end
        public :: pfm_open
        public :: pfm_close

        public :: tiff_open
        public :: tiff_header
        public :: tiff_begin
        public :: tiff_write
        public :: tiff_commit
        public :: tiff_end
        public :: tiff_close

        interface pfm_write
                module subroutine pfm_write(unit, r, g, b)
                        integer, intent(in) :: unit
                        real, intent(in) :: r, g, b
                end subroutine pfm_write
        end interface pfm_write

        interface pfm_header
                module subroutine pfm_header(unit, size)
                        integer, intent(in) :: unit
                        integer, intent(in) :: size
                end subroutine pfm_header

                module subroutine pfm_header_amorized(unit, header)
                        integer, intent(in) :: unit
                        character(len=*), intent(in) :: header
                end subroutine pfm_header_amorized
        end interface pfm_header

        interface pfm_header_compute
                module subroutine pfm_header_compute(header, size)
                        character(len=*), intent(out) :: header
                        integer, intent(in) :: size
                end subroutine pfm_header_compute
        end interface pfm_header_compute

        interface pfm_end
                module subroutine pfm_end(unit)
                        integer, intent(in) :: unit
                end subroutine pfm_end
        end interface pfm_end

        interface pfm_open
                module subroutine pfm_open(filename, number, unit)
                        character(len=19), intent(out) :: filename
                        integer, intent(in) :: number
                        integer, intent(out) :: unit
                end subroutine pfm_open
        end interface pfm_open

        interface pfm_close
                module subroutine pfm_close(unit)
                        integer, intent(in) :: unit
                end subroutine pfm_close
        end interface pfm_close

        interface tiff_open
                module subroutine tiff_open(filename, number, unit)
                        character(len=*), intent(out) :: filename
                        integer, intent(in) :: number
                        integer, intent(out) :: unit
                end subroutine tiff_open
        end interface tiff_open

        interface tiff_header
                module subroutine tiff_header(unit, size)
                        integer, intent(in) :: unit
                        integer, intent(in) :: size
                end subroutine tiff_header
        end interface tiff_header

        interface tiff_begin
                module subroutine tiff_begin(unit, size)
                        integer, intent(in) :: unit
                        integer, intent(in) :: size
                end subroutine tiff_begin
        end interface tiff_begin

        interface tiff_write
                module subroutine tiff_write(r, g, b)
                        real, intent(in) :: r, g, b
                end subroutine tiff_write
        end interface tiff_write

        interface tiff_commit
                module subroutine tiff_commit(unit)
                        integer, intent(in) :: unit
                end subroutine tiff_commit
        end interface tiff_commit

        interface tiff_end
                module subroutine tiff_end(unit, size)
                        integer, intent(in) :: unit
                        integer, intent(in) :: size
                end subroutine tiff_end
        end interface tiff_end

        interface tiff_close
                module subroutine tiff_close(unit)
                        integer, intent(in) :: unit
                end subroutine tiff_close
        end interface tiff_close

end module image_out

