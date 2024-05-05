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
    use iso_fortran_env, only: int32, real32
    implicit none

    private

    public :: pfm_write
    public :: pfm_header
    public :: pfm_header_compute
    public :: pfm_end
    public :: pfm_open
    public :: pfm_close

    type, abstract, public :: tiff
        private
        character(len=:), public, allocatable :: filename
        integer, public :: unit

        integer, public :: x_size, y_size

        integer, allocatable :: lines(:)
        integer, allocatable :: line_bytes(:)
        integer :: line_counter = 1

        integer(kind=int32) :: strip_loc = 0, xres_loc = 0, yres_loc = 0, &
            & white_loc = 0, count_loc = 0, sample_loc = 0
        integer(kind=int32) :: strip_tag = 0, xres_tag = 0, yres_tag = 0, &
            & white_tag = 0, count_tag = 0, sample_tag = 0

    contains
        procedure, public :: open => tiff_open
        procedure(header), deferred, public :: header
        procedure(begin), deferred, public :: begin
        procedure(write), deferred, public :: write
        procedure(commit), deferred, public :: commit
        procedure(end), deferred, public :: end
        procedure, public :: close => tiff_close
    end type tiff

    abstract interface
        subroutine header(this)
            import :: tiff
            class(tiff), intent(inout) :: this
        end subroutine header

        subroutine begin(this)
            import :: tiff
            class(tiff), intent(inout) :: this
        end subroutine begin
        
        subroutine write(this, a, b, c, location)
            import :: tiff
            class(tiff), intent(inout) :: this
            real, intent(in) :: a, b, c
            integer, intent(in) :: location
        end subroutine write

        subroutine commit(this)
            import :: tiff
            class(tiff), intent(inout) :: this
        end subroutine commit

        subroutine end(this)
            import :: tiff
            class(tiff), intent(inout) :: this
        end subroutine end
    end interface

    type, extends(tiff), public :: tiff_rgb
        private
        real, dimension(:,:), pointer :: scanline => NULL()

        real(kind=real32) :: min_r = 0, min_g = 0, min_b = 0
        real(kind=real32) :: max_r = 0, max_g = 0, max_b = 0

        integer(kind=int32) :: min_loc = 0, max_loc = 0
        integer(kind=int32) :: min_tag = 0, max_tag = 0

    contains
        !procedure, public :: open => tiff_open
        procedure :: header => tiff_header
        procedure :: begin => tiff_begin
        procedure :: write => tiff_write
        procedure :: commit => tiff_commit_rgb
        procedure :: end => tiff_end
        !procedure, public :: close => tiff_close
    end type tiff_rgb

    type, extends(tiff), public :: tiff_logluv
        private
        integer(kind=int32), dimension(:), pointer :: scanline => NULL()
    contains
        !procedure, public :: open => tiff_open
        procedure :: header => tiff_header_logluv
        procedure :: begin => tiff_begin_logluv
        procedure :: write => tiff_write_logluv
        procedure :: commit =>tiff_commit_logluv
        procedure :: end => tiff_end_logluv
        !procedure, public :: close => tiff_close
    end type tiff_logluv

    public :: tiff_open
    public :: tiff_header, tiff_header_logluv
    public :: tiff_begin, tiff_begin_logluv
    public :: tiff_write, tiff_write_logluv
    public :: tiff_commit_rgb, tiff_commit_logluv
    public :: tiff_end, tiff_end_logluv
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
        module subroutine tiff_open(this, number)
            class(tiff), intent(inout) :: this
            integer, intent(in) :: number
        end subroutine tiff_open
    end interface tiff_open

    interface tiff_header
        module subroutine tiff_header(this)
            class(tiff_rgb), intent(inout) :: this
        end subroutine tiff_header
    end interface tiff_header
    
    interface tiff_header_logluv
        module subroutine tiff_header_logluv(this)
            class(tiff_logluv), intent(inout) :: this
        end subroutine tiff_header_logluv
    end interface tiff_header_logluv

    interface tiff_begin
        module subroutine tiff_begin(this)
            class(tiff_rgb), intent(inout) :: this
        end subroutine tiff_begin
    end interface tiff_begin
    
    interface tiff_begin_logluv
        module subroutine tiff_begin_logluv(this)
            class(tiff_logluv), intent(inout) :: this
        end subroutine tiff_begin_logluv
    end interface tiff_begin_logluv

    interface tiff_write
        module subroutine tiff_write(this, a, b, c, location)
            class(tiff_rgb), intent(inout) :: this
            real, intent(in) :: a, b, c
            integer, intent(in) :: location
        end subroutine tiff_write
    end interface tiff_write
    
    interface tiff_write_logluv
        module subroutine tiff_write_logluv(this, a, b, c, location)
            class(tiff_logluv), intent(inout) :: this
            real, intent(in) :: a, b, c
            integer, intent(in) :: location
        end subroutine tiff_write_logluv
    end interface tiff_write_logluv

    interface tiff_commit_rgb
        module subroutine tiff_commit_rgb(this)
            class(tiff_rgb), intent(inout) :: this
        end subroutine tiff_commit_rgb
    end interface tiff_commit_rgb
    
    interface tiff_commit_logluv
        module subroutine tiff_commit_logluv(this)
            class(tiff_logluv), intent(inout) :: this
        end subroutine tiff_commit_logluv
    end interface tiff_commit_logluv
    
    interface tiff_end
        module subroutine tiff_end(this)
            class(tiff_rgb), intent(inout) :: this
        end subroutine tiff_end
    end interface tiff_end
    
    interface tiff_end_logluv
        module subroutine tiff_end_logluv(this)
            class(tiff_logluv), intent(inout) :: this
        end subroutine tiff_end_logluv
    end interface tiff_end_logluv

    interface tiff_close
        module subroutine tiff_close(this)
            class(tiff), intent(inout) :: this
        end subroutine tiff_close
    end interface tiff_close

end module image_out

