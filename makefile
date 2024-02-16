FC = gfortran
CFLAGS := -O2 -g -Wall -fopenmp
#-fsanitize=address
CPPFLAGS = $(shell pkg-config --cflags blas fortran-zlib)
LDLIBS = $(shell pkg-config --libs blas fortran-zlib)

SRCDIR = src
OBJDIR = obj

build: $(OBJDIR) $(OBJDIR)/image_out.o $(OBJDIR)/pfm.o $(OBJDIR)/tiff.o $(OBJDIR)/main.o
	$(FC) $(CFLAGS) $(CPPFLAGS) -I$(OBJDIR) $(OBJDIR)/image_out.o $(OBJDIR)/pfm.o $(OBJDIR)/tiff.o $(OBJDIR)/main.o -o plot $(LDLIBS)

$(OBJDIR):
	@mkdir $(OBJDIR)

$(OBJDIR)/main.o: $(SRCDIR)/main.f08
	$(FC) $(CFLAGS) $(CPPFLAGS) -I$(OBJDIR) -c $(SRCDIR)/main.f08 -o $(OBJDIR)/main.o

$(OBJDIR)/image_out.o: $(SRCDIR)/image_out.f08
	$(FC) $(CFLAGS) $(CPPFLAGS) -c $(SRCDIR)/image_out.f08 -o $(OBJDIR)/image_out.o -J$(OBJDIR)

$(OBJDIR)/pfm.o: $(OBJDIR)/image_out.o $(SRCDIR)/pfm.f08
	$(FC) $(CFLAGS) $(CPPFLAGS) -c $(SRCDIR)/pfm.f08 -o $(OBJDIR)/pfm.o -J$(OBJDIR)

$(OBJDIR)/tiff.o: $(OBJDIR)/image_out.o $(SRCDIR)/tiff.f08
	$(FC) $(CFLAGS) $(CPPFLAGS) -c $(SRCDIR)/tiff.f08 -o $(OBJDIR)/tiff.o -J$(OBJDIR)

PHONEY: clean

clean:
	@-rm -rf $(OBJDIR)
	@-rm plot

