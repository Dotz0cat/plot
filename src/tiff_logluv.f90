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

submodule (image_out:tiff) tiff_logluv_smod
    use iso_fortran_env, only: real32, int8, int16, int32
    implicit none

contains
    module procedure tiff_header_logluv
        integer :: i, j
        integer :: current

        ! file header
        write(this%unit) 'II', int(42, kind=int16), int(8, kind=int32)
                
        ! IFD header
        write(this%unit) int(15, kind=int16)

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
        write(this%unit) tiff_tag(259, 3, 1, 34676)

        ! photometric interpation tag
        write(this%unit) tiff_tag(262, 3, 1, 32845)
                
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
        write(this%unit) tiff_tag(339, 3, 1, 2)
                
        write(this%unit) int(0, kind=int32)
                
        inquire(this%unit, pos=this%xres_loc)
        write(this%unit) int(this%x_size, kind=int32), int(1, kind=int32)

        inquire(this%unit, pos=this%yres_loc)
        write(this%unit) int(this%y_size, kind=int32), int(1, kind=int32)

        inquire(this%unit, pos=this%white_loc)
        write(this%unit) int(3127, kind=int32), int(10000, kind=int32)
        write(this%unit) int(3290, kind=int32), int(10000, kind=int32)

        inquire(this%unit, pos=this%sample_loc)
        write(this%unit) int(16, kind=int16), int(16, kind=int16), int(16, kind=int16)

        inquire(this%unit, pos=this%count_loc)
        do i=1, this%x_size
            write(this%unit) int(0, kind=int32)
        end do

        inquire(this%unit, pos=this%strip_loc)
        do j=1, this%y_size
            write(this%unit) int(0, kind=int32)
        end do

        inquire(this%unit, pos=current)

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
    end procedure tiff_header_logluv

    module procedure tiff_begin_logluv
        if (associated(this%scanline)) then
            print *, 'error: calling tiff begin again without calling tiff commit'
            return
        end if

        allocate(this%scanline(this%x_size))
    end procedure tiff_begin_logluv
    
    module procedure tiff_write_logluv
        integer(kind=int32) :: logluv

        if (.not. associated(this%scanline)) then
            print *, 'error: calling tiff write before tiff begin'
            return
        end if
        
        logluv = shiftl(LtoLogL16(a), 16)
        logluv = ior(logluv, shiftl(itrunc_uv(b), 8))
        logluv = ior(logluv, itrunc_uv(c))

        this%scanline(location) = logluv
    end procedure tiff_write_logluv
    
    module procedure tiff_commit_logluv
        integer(kind=int8), dimension(:), allocatable :: compressed
        integer(kind=int8), dimension(:), pointer :: pre_compress
        integer(kind=int8), dimension(:, :), pointer :: inter
        integer(kind=int8), dimension(:), pointer :: raw
        integer :: current

        integer :: i, j, run_end, pixel_counter
        integer :: dump_end, dump_count
        integer(kind=int32) :: run_count
        integer(kind=int8) :: symbol
        integer :: pixels
        integer :: compress_loc
        integer, parameter :: minrun = 2
        integer, dimension(4), parameter :: order = [4, 3, 2, 1]
        
        if (.not. associated(this%scanline)) then
            print *, 'error: calling tiff commit without calling tiff_begin'
            return
        end if

        inquire(this%unit, pos=current)
        this%lines(this%line_counter) = current - 1

        compress_loc = 1
        pixels = int(sizeof(this%scanline))
        pixels = pixels / 4

        allocate(raw(sizeof(this%scanline))) 
        allocate(compressed(sizeof(this%scanline)))
        
        raw = transfer(this%scanline, mold=(/0_int8/))

        inter(1:4, 1:pixels) => raw

        do i=1, 4
            !int32 array broke into 2d int8 array

            pixel_counter = 1
            
            pre_compress => inter(order(i), :)

            !loop over each pixel if needed

            do
                ! while (pixel_counter <= pixels
                if (pixel_counter .gt. pixels) then
                    exit
                end if

                symbol = pre_compress(pixel_counter)
                
                run_count = 1
                run_end = pixel_counter + 1

                do
                    ! while (run_end <= pixels && run_count < 129)
                    if (run_end .gt. pixels .or. run_count .ge. 129) then
                        exit
                    end if
                    
                    if (symbol .eq. pre_compress(run_end)) then
                        run_count = run_count + 1
                        run_end = run_end + 1
                    else
                        exit
                    end if
                end do

                if (run_count .ge. minrun) then
                    !write run
                    compressed(compress_loc) = int(-128 - 2 + run_count, kind=int8)
                    compress_loc = compress_loc + 1

                    !symbol
                    compressed(compress_loc) = symbol
                    compress_loc = compress_loc + 1

                    pixel_counter = run_end
                    cycle
                else
                    !look for next run or till 127 bytes consumed
                    !then write till next run

                    run_count = 1;
                    dump_end = pixel_counter + 1
                    dump_count = 1

                    j = pixel_counter + 1

                    do
                        ! while (j <= pixel_counter + 126 && dump_count <= 126)
                        if (j .gt. pixel_counter + 126 .or. dump_count .gt. 126) then 
                            exit
                        end if
                        
                        ! while (j <= pixels && j + 1 <= pixels)
                        if (j .gt. pixels .or. j + 1 .gt. pixels) then
                            dump_end = pixels + 1
                            dump_count = pixels - pixel_counter + 1
                            exit
                        end if

                        symbol = pre_compress(j)
                        
                        
                        !Look ahead one
                        if (symbol .eq. pre_compress(j + 1)) then
                            !possiable run found
                            run_count = run_count + 1
                        else
                            !Not a run
                            run_count = 1
                        end if
                        
                        j = j + 1
                        dump_end = j
                        dump_count = dump_end - pixel_counter

                        if (run_count .ge. minrun) then
                            ! 3d 3c 3c 3d 3c 3c 3d
                            ! 10 11 12 13 14 15 16
                            ! pi en  j 
                            dump_end = dump_end - run_count + 1
                            dump_count = dump_end - pixel_counter
                            exit
                        end if
                    end do

                    !write till next run
                    compressed(compress_loc) = int(dump_count, kind=int8)
                    compress_loc = compress_loc + 1

                    j = pixel_counter

                    
                    compressed(compress_loc) = pre_compress(j)
                    compress_loc = compress_loc + 1
                    pixel_counter = dump_end

                    j = j + 1

                    do
                        ! while (j < dump_end)
                        if (j .ge. dump_end) exit
                        compressed(compress_loc) = pre_compress(j)
                        compress_loc = compress_loc + 1
                        j = j + 1
                    end do
                    
                    pixel_counter = dump_end
                    cycle
                end if
            end do
        end do
        
        this%line_bytes(this%line_counter) = compress_loc - 1
        
        write(this%unit) compressed(1:compress_loc-1)

        deallocate(compressed)
        deallocate(raw)
        deallocate(this%scanline)
        this%line_counter = this%line_counter + 1
    end procedure tiff_commit_logluv
    
    module procedure tiff_end_logluv
        integer :: current
        integer :: i
        integer :: j
                
        inquire(this%unit, pos=current)
                
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
    end procedure tiff_end_logluv
    
    module integer(kind=int32) function LtoLogl16(lum) result (logl)
        real, intent(in) :: lum

        if (lum .ge. 3.060858522597e8) then
            logl = int(z'7fff', kind=int32)
            return
        end if

        if (lum .le. -3.060858522597e8) then
            logl = int(z'ffff', kind=int32)
            return
        end if

        ! lum greater tan kappa * delta then inplace convert to Y
        if (lum .gt. ((24389.0/27.0) * (216.0/24389.0))) then
            logl = itrunc(768.0 * log2((lum + 16.0)/116.0) + 16384.0)
            return
        end if

        if (lum .gt. 4.88987498e-17) then
            logl = itrunc(256.0 * log2(lum * (27.0/24389.0)) + 16384.0)
            return
        end if

        if (lum .lt. -4.88987498e-17) then
            logl = ior(int(z'8000', kind=int32), &
                & itrunc(256.0 * log2(lum * (27.0/24389.0)) + 16384.0))
            return
        end if

        logl = 0
    end function LtoLogl16

    module integer(kind=int32) function itrunc(x) result(y)
        real, intent(in) :: x

        y = iand(floor(x, kind=int32), 65535)
    end function itrunc

    module integer(kind=int32) function itrunc_uv(x) result(y)
        real, intent(in) :: x

        y = iand(floor(410.0 * x, kind=int32), 255)
    end function itrunc_uv

    real function log2(x) result(y)
        real, intent(in) :: x
        real, save :: ln2 = log(2.0)

        y = log(x)/ln2
    end function log2

end submodule tiff_logluv_smod

