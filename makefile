#compile with FC
#I have tried and tested it with gfortran
FC = gfortran
#Compiler options
#I normally compile with -O2
#I have had good results with -flto too
#debug builds with -g
CFLAGS := -O2 -Wall -fopenmp
#The only full dependance is on fortran-zlib
#it provides the compression for rgb tiffs
#hardcode the cppflags and ldlibs for fortran-zlib.
#just patch it to your location
CPPFLAGS = $(shell pkg-config --cflags blas)
CPPFLAGS += -I/usr/include/ -I/usr/include/fortran-zlib/GNU-13.2.1/
LDLIBS = $(shell pkg-config --libs blas)
LDLIBS += -L/usr/lib/ -lfortran-zlib -lz

SRCDIR = src
OBJDIR = obj

OBJS = $(OBJDIR)/image_out.o $(OBJDIR)/pfm.o $(OBJDIR)/tiff.o $(OBJDIR)/tiff_logluv.o $(OBJDIR)/domain_color.o $(OBJDIR)/domain_color_luv.o $(OBJDIR)/test_functions.o $(OBJDIR)/main.o

build: $(OBJDIR) $(OBJS)
	$(FC) $(CFLAGS) $(CPPFLAGS) -I$(OBJDIR) $(OBJS) -o plot $(LDLIBS)

$(OBJDIR):
	@mkdir $(OBJDIR)

$(OBJDIR)/main.o: $(SRCDIR)/main.f90
	$(FC) $(CFLAGS) $(CPPFLAGS) -I$(OBJDIR) -c $(SRCDIR)/main.f90 -o $@

$(OBJDIR)/image_out.o: $(SRCDIR)/image_out.f90
	$(FC) $(CFLAGS) $(CPPFLAGS) -c $(SRCDIR)/image_out.f90 -o $@ -J$(OBJDIR)

$(OBJDIR)/pfm.o: $(OBJDIR)/image_out.o $(SRCDIR)/pfm.f90
	$(FC) $(CFLAGS) $(CPPFLAGS) -c $(SRCDIR)/pfm.f90 -o $@ -J$(OBJDIR)

$(OBJDIR)/tiff.o: $(OBJDIR)/image_out.o $(SRCDIR)/tiff.f90
	$(FC) $(CFLAGS) $(CPPFLAGS) -c $(SRCDIR)/tiff.f90 -o $@ -J$(OBJDIR)

$(OBJDIR)/tiff_logluv.o: $(OBJDIR)/image_out.o $(SRCDIR)/tiff_logluv.f90
	$(FC) $(CFLAGS) $(CPPFLAGS) -c $(SRCDIR)/tiff_logluv.f90 -o $@ -J$(OBJDIR)

$(OBJDIR)/domain_color.o: $(SRCDIR)/domain_color.f90
	$(FC) $(CFLAGS) $(CPPFLAGS) -c $(SRCDIR)/domain_color.f90 -o $@ -J$(OBJDIR)

$(OBJDIR)/domain_color_luv.o: $(OBJDIR)/domain_color.o $(SRCDIR)/domain_color_luv.f90
	$(FC) $(CFLAGS) $(CPPFLAGS) -c $(SRCDIR)/domain_color_luv.f90 -o $@ -J$(OBJDIR)

$(OBJDIR)/test_functions.o: $(SRCDIR)/test_functions.f90
	$(FC) $(CFLAGS) $(CPPFLAGS) -c $(SRCDIR)/test_functions.f90 -o $@ -J$(OBJDIR)

PHONEY: clean

clean:
	@-rm -rf $(OBJDIR)
	@-rm plot

