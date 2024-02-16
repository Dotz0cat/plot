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

contains
        module procedure tiff_open
                this%filename = 'output/out0000.tiff'
                write(this%filename, '(A, I0.4, A)') 'output/out', number, '.tiff'

                open(newunit=this%unit, file=this%filename, access="stream", form="unformatted", status="replace")
                
        end procedure tiff_open

        module procedure tiff_close
                deallocate(this%lines)
                close(this%unit)
        end procedure tiff_close

        module procedure tiff_header
                integer :: i, j
                integer :: current

                ! file header
                write(this%unit) 'II', int(42, kind=int16), int(8, kind=int32)
                
                ! IFD header
                write(this%unit) int(17, kind=int16)

                ! Width tag
                write(this%unit) tiff_tag(256, 3, 1, this%x_size)
                
                ! length tag
                write(this%unit) tiff_tag(257, 3, 1, this%y_size)

                ! bits per sample tag
                write(this%unit) tiff_tag(258, 3, 3, 0)
                inquire(this%unit, pos=this%sample_tag)
                this%sample_tag = this%sample_tag - 4
                
                ! compression
                ! will be edited later
                write(this%unit) tiff_tag(259, 3, 1, 8)

                ! photometric interpation tag
                write(this%unit) tiff_tag(262, 3, 1, 2)
                
                ! strip offsets
                write(this%unit) tiff_tag(273, 4, this%y_size, 0)
                inquire(this%unit, pos=this%strip_tag)
                this%strip_tag = this%strip_tag - 4

                ! samples per pixel
                write(this%unit) tiff_tag(277, 3, 1, 3)

                ! rows per strip
                write(this%unit) tiff_tag(278, 3, 1, 1)

                ! strip byte counts
                write(this%unit) tiff_tag(279, 4, this%y_size, 0)
                inquire(this%unit, pos=this%count_tag)
                this%count_tag = this%count_tag - 4
                
                ! xres
                write(this%unit) tiff_tag(282, 5, 1, 0)
                inquire(this%unit, pos=this%xres_tag)
                this%xres_tag = this%xres_tag - 4

                ! yres
                write(this%unit) tiff_tag(283, 5, 1, 0)
                inquire(this%unit, pos=this%yres_tag)
                this%yres_tag = this%yres_tag - 4

                ! planar config
                write(this%unit) tiff_tag(284, 3, 1, 1)
                
                ! resolution unit
                write(this%unit) tiff_tag(296, 3, 1, 1)

                ! while point
                ! d65
                write(this%unit) tiff_tag(318, 5, 2, 0)
                inquire(this%unit, pos=this%white_tag)
                this%white_tag = this%white_tag - 4

                ! sample format
                write(this%unit) tiff_tag(339, 3, 1, 3)
                
                ! min sample value
                write(this%unit) tiff_tag(340, 11, 3, 0)
                inquire(this%unit, pos=this%min_tag)
                this%min_tag = this%min_tag - 4

                ! max sample value
                write(this%unit) tiff_tag(341, 11, 3, 0)
                inquire(this%unit, pos=this%max_tag)
                this%max_tag = this%max_tag - 4

                write(this%unit) int(0, kind=int32)
                
                inquire(this%unit, pos=this%min_loc)
                write(this%unit) real(0, kind=real32), real(0, kind=real32), real(0, kind=real32)
                
                inquire(this%unit, pos=this%max_loc)
                write(this%unit) real(0, kind=real32), real(0, kind=real32), real(0, kind=real32)
                
                inquire(this%unit, pos=this%xres_loc)
                write(this%unit) int(this%x_size, kind=int32), int(1, kind=int32)

                inquire(this%unit, pos=this%yres_loc)
                write(this%unit) int(this%y_size, kind=int32), int(1, kind=int32)

                inquire(this%unit, pos=this%white_loc)
                write(this%unit) int(3127, kind=int32), int(10000, kind=int32)
                write(this%unit) int(3290, kind=int32), int(10000, kind=int32)

                inquire(this%unit, pos=this%sample_loc)
                write(this%unit) int(32, kind=int16), int(32, kind=int16), int(32, kind=int16)

                inquire(this%unit, pos=this%count_loc)
                do i=1, this%x_size
                        write(this%unit) int(0, kind=int32)
                end do

                inquire(this%unit, pos=this%strip_loc)
                do j=1, this%y_size
                        write(this%unit) int(0, kind=int32)
                end do

                inquire(this%unit, pos=current)

                write(this%unit, pos=this%min_tag) int(this%min_loc-1, kind=int32)
                write(this%unit, pos=this%max_tag) int(this%max_loc-1, kind=int32)
                write(this%unit, pos=this%xres_tag) int(this%xres_loc-1, kind=int32)
                write(this%unit, pos=this%yres_tag) int(this%yres_loc-1, kind=int32)
                write(this%unit, pos=this%white_tag) int(this%white_loc-1, kind=int32)
                write(this%unit, pos=this%sample_tag) int(this%sample_loc-1, kind=int32)
                write(this%unit, pos=this%count_tag) int(this%count_loc-1, kind=int32)

                write(this%unit, pos=this%strip_tag) int(this%strip_loc-1, kind=int32)
                write(this%unit, pos=current)

                allocate(this%lines(this%y_size))
                allocate(this%line_bytes(this%x_size))
                this%line_counter = 1

        end procedure tiff_header

        module procedure tiff_begin
                if (associated(this%scanline)) then
                        print *, 'error: calling tiff begin again without calling tiff commit'
                        return
                end if

                allocate(this%scanline(3, this%x_size))
                
                this%counter = 1;

        end procedure tiff_begin

        module procedure tiff_write
                if (.not. associated(this%scanline)) then
                        print *, 'error: calling tiff write before tiff begin'
                        return
                end if

                this%scanline(1, this%counter) = r
                this%min_r = min(this%min_r, r)
                this%max_r = max(this%max_r, r)

                this%scanline(2, this%counter) = g
                this%min_g = min(this%min_g, g)
                this%max_g = max(this%max_g, g)

                this%scanline(3, this%counter) = b
                this%min_b = min(this%min_b, b)
                this%max_b = max(this%max_b, b)

                this%counter = this%counter + 1
        end procedure tiff_write

        module procedure tiff_commit
                integer :: i
                integer :: current
                integer :: rc, error
                character(len=sizeof(this%scanline)), target :: compressed
                real, pointer :: scanline_ptr(:,:)
                type(z_stream) :: stream
                
                if (.not. associated(this%scanline)) then
                        print *, 'error: calling tiff commit without calling tiff_begin'
                        return
                end if

                scanline_ptr => this%scanline

                inquire(this%unit, pos=current)
                this%lines(this%line_counter) = current - 1
                
                this%counter = this%counter - 1

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

                this%line_bytes(this%line_counter) = len(compressed) - stream%avail_out

                write(this%unit) compressed(1:this%line_bytes(this%line_counter))

                error = deflate_end(stream)

                deallocate(this%scanline)
                this%counter = 1
                this%line_counter = this%line_counter + 1
        end procedure tiff_commit

        module procedure tiff_end
                integer :: current
                integer :: i
                integer :: j
                
                inquire(this%unit, pos=current)
                write(this%unit, pos=this%min_loc) real(this%min_r, kind=real32), real(this%min_g, kind=real32), &
                        & real(this%min_b, kind=real32)
                write(this%unit, pos=this%max_loc) real(this%max_r, kind=real32), real(this%max_g, kind=real32), &
                        & real(this%max_b, kind=real32)
                
                ! seek to stripsoffset array
                write(this%unit, pos=this%strip_loc)

                this%line_counter = this%line_counter - 1

                do i=1, this%line_counter
                        write(this%unit) int(this%lines(i), kind=int32)
                end do
                
                write(this%unit, pos=this%count_loc)
                do j=1, this%line_counter
                        write(this%unit) int(this%line_bytes(j), kind=int32)
                end do

                write(this%unit, pos=current)
        end procedure tiff_end
        
end submodule tiff

