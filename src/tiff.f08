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

submodule (image_out) tiff
        use iso_fortran_env, only: int8, int16, int32, real32
        use iso_c_binding, only: c_loc
        use zlib
        implicit none

        type :: tiff_tag
                integer(kind=int16) :: tag_number
                integer(kind=int16) :: data_type
                integer(kind=int32) :: number_of_entries
                integer(kind=int32) :: offset
        end type 

        real, allocatable, target :: scanline(:, :)
        integer, allocatable :: lines(:)
        integer, allocatable :: line_bytes(:)
        integer :: counter, line_counter

        real(kind=real32) :: min_r, min_g, min_b
        real(kind=real32) :: max_r, max_g, max_b

        integer(kind=int32) :: strip_loc, xres_loc, yres_loc, min_loc, max_loc, white_loc, count_loc, sample_loc
        integer(kind=int32) :: strip_tag, xres_tag, yres_tag, min_tag, max_tag, white_tag, count_tag, sample_tag

contains
        module procedure tiff_open
                write(filename, '(A, I0.4, A)') 'output/out', number, '.tiff'

                open(newunit=unit, file=filename, access="stream", form="unformatted", status="replace")
                
                min_r = 0
                min_g = 0
                min_b = 0
                max_r = 0
                max_g = 0
                max_b = 0

        end procedure tiff_open

        module procedure tiff_close
                deallocate(lines)
                close(unit)
        end procedure tiff_close

        module procedure tiff_header
                integer :: i, j
                integer :: current

                ! file header
                write(unit) 'II', int(42, kind=int16), int(8, kind=int32)
                
                ! IFD header
                write(unit) int(17, kind=int16)

                ! Width tag
                write(unit) tiff_tag(256, 3, 1, size)
                
                ! length tag
                write(unit) tiff_tag(257, 3, 1, size)

                ! bits per sample tag
                write(unit) tiff_tag(258, 3, 3, 0)
                inquire(unit, pos=sample_tag)
                sample_tag = sample_tag - 4
                
                ! compression
                ! will be edited later
                write(unit) tiff_tag(259, 3, 1, 8)

                ! photometric interpation tag
                write(unit) tiff_tag(262, 3, 1, 2)
                
                ! strip offsets
                write(unit) tiff_tag(273, 4, size, 0)
                inquire(unit, pos=strip_tag)
                strip_tag = strip_tag - 4

                ! samples per pixel
                write(unit) tiff_tag(277, 3, 1, 3)

                ! rows per strip
                write(unit) tiff_tag(278, 3, 1, 1)

                ! strip byte counts
                write(unit) tiff_tag(279, 4, size, 0)
                inquire(unit, pos=count_tag)
                count_tag = count_tag - 4
                
                ! xres
                write(unit) tiff_tag(282, 5, 1, 0)
                inquire(unit, pos=xres_tag)
                xres_tag = xres_tag - 4

                ! yres
                write(unit) tiff_tag(283, 5, 1, 0)
                inquire(unit, pos=yres_tag)
                yres_tag = yres_tag - 4

                ! planar config
                write(unit) tiff_tag(284, 3, 1, 1)
                
                ! resolution unit
                write(unit) tiff_tag(296, 3, 1, 1)

                ! while point
                ! d65
                write(unit) tiff_tag(318, 5, 2, 0)
                inquire(unit, pos=white_tag)
                white_tag = white_tag - 4

                ! sample format
                write(unit) tiff_tag(339, 3, 1, 3)
                
                ! min sample value
                write(unit) tiff_tag(340, 11, 3, 0)
                inquire(unit, pos=min_tag)
                min_tag = min_tag - 4

                ! max sample value
                write(unit) tiff_tag(341, 11, 3, 0)
                inquire(unit, pos=max_tag)
                max_tag = max_tag - 4

                write(unit) int(0, kind=int32)
                
                inquire(unit, pos=min_loc)
                write(unit) real(0, kind=real32), real(0, kind=real32), real(0, kind=real32)
                
                inquire(unit, pos=max_loc)
                write(unit) real(0, kind=real32), real(0, kind=real32), real(0, kind=real32)
                
                inquire(unit, pos=xres_loc)
                write(unit) int(size, kind=int32), int(1, kind=int32)

                inquire(unit, pos=yres_loc)
                write(unit) int(size, kind=int32), int(1, kind=int32)

                inquire(unit, pos=white_loc)
                write(unit) int(3127, kind=int32), int(10000, kind=int32)
                write(unit) int(3290, kind=int32), int(10000, kind=int32)

                inquire(unit, pos=sample_loc)
                write(unit) int(32, kind=int16), int(32, kind=int16), int(32, kind=int16)

                inquire(unit, pos=count_loc)
                do i=1, size
                        write(unit) int(0, kind=int32)
                end do

                inquire(unit, pos=strip_loc)
                do j=1, size
                        write(unit) int(0, kind=int32)
                end do

                inquire(unit, pos=current)

                write(unit, pos=min_tag) int(min_loc-1, kind=int32)
                write(unit, pos=max_tag) int(max_loc-1, kind=int32)
                write(unit, pos=xres_tag) int(xres_loc-1, kind=int32)
                write(unit, pos=yres_tag) int(yres_loc-1, kind=int32)
                write(unit, pos=white_tag) int(white_loc-1, kind=int32)
                write(unit, pos=sample_tag) int(sample_loc-1, kind=int32)
                write(unit, pos=count_tag) int(count_loc-1, kind=int32)

                write(unit, pos=strip_tag) int(strip_loc-1, kind=int32)
                write(unit, pos=current)

                allocate(lines(size))
                allocate(line_bytes(size))
                line_counter = 1

        end procedure tiff_header

        module procedure tiff_begin
                if (allocated(scanline)) then
                        print *, 'error: calling tiff begin again without calling tiff commit'
                        return
                end if

                allocate(scanline(3, size))
                
                counter = 1;
                ! scanline header


        end procedure tiff_begin

        module procedure tiff_write
                if (.not. allocated(scanline)) then
                        print *, 'error: calling tiff write before tiff begin'
                        return
                end if

                scanline(1, counter) = r
                min_r = min(min_r, r)
                max_r = max(max_r, r)

                scanline(2, counter) = g
                min_g = min(min_g, g)
                max_g = max(max_g, g)

                scanline(3, counter) = b
                min_b = min(min_b, b)
                max_b = max(max_b, b)

                counter = counter + 1
        end procedure tiff_write

        module procedure tiff_commit
                integer :: i
                integer :: current
                integer :: rc, error
                character(len=sizeof(scanline)), target :: compressed
                real, pointer :: scanline_ptr(:,:)
                type(z_stream) :: stream
                
                if (.not. allocated(scanline)) then
                        print *, 'error: calling tiff commit without calling tiff_begin'
                        return
                end if

                scanline_ptr => scanline

                inquire(unit, pos=current)
                lines(line_counter) = current - 1
                
                !line_bytes(line_counter) = sizeof(scanline)

                counter = counter - 1

                ! write stuff
                !do i=1, counter
                !        write(unit) real(scanline(i, 1), kind=real32), real(scanline(i, 2), kind=real32), &
                !        & real(scanline(i, 3), kind=real32)
                !end do

                rc = deflate_init(stream, Z_DEFAULT_COMPRESSION)
                if (rc .ne. Z_OK) return

                stream%total_in = sizeof(scanline_ptr)
                stream%avail_in = sizeof(scanline_ptr)
                stream%next_in = c_loc(scanline_ptr)

                stream%total_out = len(compressed)
                stream%avail_out = len(compressed)
                stream%next_out = c_loc(compressed)

                rc = deflate(stream, Z_FINISH)
                if (rc .ne. Z_STREAM_END) then
                        error = deflate_end(stream)
                        print *, error
                        return
                end if

                line_bytes(line_counter) = len(compressed) - stream%avail_out

                write(unit) compressed(1:line_bytes(line_counter))

                deallocate(scanline)
                counter = 1
                line_counter = line_counter + 1
        end procedure tiff_commit

        module procedure tiff_end
                integer :: current
                integer :: i
                integer :: j
                
                inquire(unit, pos=current)
                write(unit, pos=min_loc) real(min_r, kind=real32), real(min_g, kind=real32), real(min_b, kind=real32)
                write(unit, pos=max_loc) real(max_r, kind=real32), real(max_g, kind=real32), real(max_b, kind=real32)
                
                ! seek to stripsoffset array
                write(unit, pos=strip_loc)

                line_counter = line_counter - 1

                do i=1, line_counter
                        write(unit) int(lines(i), kind=int32)
                end do
                
                write(unit, pos=count_loc)
                do j=1, line_counter
                        write(unit) int(line_bytes(j), kind=int32)
                end do

                write(unit, pos=current)
        end procedure tiff_end
        
end submodule tiff

