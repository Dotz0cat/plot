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

submodule (image_out) pfm
    implicit none

contains
    module procedure pfm_write
        write(unit) r
        write(unit) g
        write(unit) b
    end procedure pfm_write

    module procedure pfm_header
        character(len=10) :: header
        character(len=10) :: aspect
        integer :: header_size, aspect_size
        integer :: header_pos, aspect_pos
        integer :: rgb_start_pos

        header_pos = 3
        header_size = sizeof(header)
        aspect_pos = header_pos + header_size + 1
        aspect_size = sizeof(aspect)
        rgb_start_pos = aspect_pos + aspect_size + 1

        write(header, '(I4.4, A, I4.4)') size, ' ', size
        write(aspect, '(F10.6)') -1.0

        write(unit) 'PF', 10
        write(unit, pos=3) header, 10
        write(unit, pos=header_pos) aspect, 10

        ! seek to first blank
        write(unit, pos=rgb_start_pos)
    end procedure pfm_header

    module procedure pfm_header_amorized
        character(len=10) :: aspect
        integer :: header_size, aspect_size
        integer :: header_pos, aspect_pos
        integer :: rgb_start_pos

        header_pos = 3
        header_size = sizeof(header)
        aspect_pos = header_pos + header_size + 1
        aspect_size = sizeof(aspect)
        rgb_start_pos = aspect_pos + aspect_size + 1

        write(aspect, '(F10.6)') -1.0

        write(unit) 'PF', 10
        write(unit, pos=3) header, 10
        write(unit, pos=header_pos) aspect, 10

        ! seek to first blank
        write(unit, pos=rgb_start_pos)
    end procedure pfm_header_amorized

    module procedure pfm_header_compute
        write(header, '(I4.4, A, I4.4)') size, ' ', size
    end procedure pfm_header_compute

    module procedure pfm_end
        write(unit) 10
    end procedure pfm_end

    module procedure pfm_open
        write(filename, '(A, I0.4, A)') 'output/out', number, '.pfm'

        open(newunit=unit, file=filename, access="stream", form="unformatted")
    end procedure pfm_open

    module procedure pfm_close
            close(unit)
    end procedure pfm_close
end submodule pfm

                
