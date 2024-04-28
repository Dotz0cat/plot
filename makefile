FC = gfortran
CFLAGS := -O2 -g -Wall -fopenmp -flto
#-fsanitize=address
CPPFLAGS = $(shell pkg-config --cflags blas fortran-zlib)
LDLIBS = $(shell pkg-config --libs blas fortran-zlib)

SRCDIR = src
OBJDIR = obj

OBJS = $(OBJDIR)/image_out.o $(OBJDIR)/pfm.o $(OBJDIR)/tiff.o $(OBJDIR)/tiff_logluv.o $(OBJDIR)/domain_color.o $(OBJDIR)/domain_color_luv.o $(OBJDIR)/test_functions.o $(OBJDIR)/main.o

build: $(OBJDIR) $(OBJS)
	$(FC) $(CFLAGS) $(CPPFLAGS) -I$(OBJDIR) $(OBJS) -o plot $(LDLIBS)

$(OBJDIR):
	@mkdir $(OBJDIR)

$(OBJDIR)/main.o: $(SRCDIR)/main.f08
	$(FC) $(CFLAGS) $(CPPFLAGS) -I$(OBJDIR) -c $(SRCDIR)/main.f08 -o $@

$(OBJDIR)/image_out.o: $(SRCDIR)/image_out.f08
	$(FC) $(CFLAGS) $(CPPFLAGS) -c $(SRCDIR)/image_out.f08 -o $@ -J$(OBJDIR)

$(OBJDIR)/pfm.o: $(OBJDIR)/image_out.o $(SRCDIR)/pfm.f08
	$(FC) $(CFLAGS) $(CPPFLAGS) -c $(SRCDIR)/pfm.f08 -o $@ -J$(OBJDIR)

$(OBJDIR)/tiff.o: $(OBJDIR)/image_out.o $(SRCDIR)/tiff.f08
	$(FC) $(CFLAGS) $(CPPFLAGS) -c $(SRCDIR)/tiff.f08 -o $@ -J$(OBJDIR)

$(OBJDIR)/tiff_logluv.o: $(OBJDIR)/image_out.o $(SRCDIR)/tiff_logluv.f08
	$(FC) $(CFLAGS) $(CPPFLAGS) -c $(SRCDIR)/tiff_logluv.f08 -o $@ -J$(OBJDIR)

$(OBJDIR)/domain_color.o: $(SRCDIR)/domain_color.f08
	$(FC) $(CFLAGS) $(CPPFLAGS) -c $(SRCDIR)/domain_color.f08 -o $@ -J$(OBJDIR)

$(OBJDIR)/domain_color_luv.o: $(OBJDIR)/domain_color.o $(SRCDIR)/domain_color_luv.f08
	$(FC) $(CFLAGS) $(CPPFLAGS) -c $(SRCDIR)/domain_color_luv.f08 -o $@ -J$(OBJDIR)

$(OBJDIR)/test_functions.o: $(SRCDIR)/test_functions.f08
	$(FC) $(CFLAGS) $(CPPFLAGS) -c $(SRCDIR)/test_functions.f08 -o $@ -J$(OBJDIR)

PHONEY: clean

clean:
	@-rm -rf $(OBJDIR)
	@-rm plot

