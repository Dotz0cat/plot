# Plot

A complex plotting program made in modern fortran

Everything is controlled in main.f08.

## Variables
* `start` - starting point for the plot \(usually negitive\)
* `stop` - stoping point for the plot \(usually positive\)
* `c1, c2, c3` - the 3 color values either r, g, b or L\*, u', v'
* `steps` - number of steps for the plot
currently the image produced is steps by steps
also controls the number of frames to produce if used to do so.

## Options

### Image output types
* pfm
* rgb 32-bit floating point tiff 
* logluv tiff

Out of these logluv is recomended

### Built in functions
* identity - $z = x + iy$, $x, y \in \mathbb{R}$
* unit circle - $z = x^{2} + y^{2}$, $x, y \in \mathbb{C}$
* taubin heart - $z = (x^{2} + y^{2} - 1)^{3} - x^{2}y^{3}$, $x ,y \in \mathbb{C}$
* taubin heart gradient - $z = \partial z/\partial x + i \partial z/\partial y$, $x, y \in \mathbb{C}$, may not be accurate

### Domain coloring
* `domain_color_luv` - produces colors in the L\*u'v' space, outputs L\*, u', v'
* `domain_color_srgb` - linear sRGB space, outputs r, g, b
* `domain_color_gamma_srgb` - gamma corrected sRGB space, outputs r, g, b

## Dependancies

* [fortran-zlib](https://github.com/interkosmos/fortran-zlib)
* blas

There is not a hard dependance on blas

## Build

1. patch makefile to point to where fortran-zlib is installed
2. `$ make`
3. make `output` directory

Patching the makefile is needed since fortran-zlib does not provide pkg-config. It is also needed since module files are not binary compatiable among fortran compilers.

Plot was devloped using gfortran, open an issue if it does not compile under your fortran compiler.

## Licence

This program is licenced under the GPL version 3 or later

## Contributions

Issues and pull requests are welcome

