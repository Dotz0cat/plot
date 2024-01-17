FC = gfortran
CFLAGS := -O2 -g -Wall -fopenmp
#CPPFLAGS = $(shell pkg-config --cflags)
#LDLIBS = $(shell pkg-config --libs)

SRCDIR = src
OBJDIR = obj

build: $(OBJDIR) $(OBJDIR)/main.o
	$(FC) $(CFLAGS) $(CPPFLAGS) $(OBJDIR)/main.o -o plot $(LDLIBS)

$(OBJDIR):
	@mkdir $(OBJDIR)

$(OBJDIR)/main.o: $(SRCDIR)/main.f08
	$(FC) $(CFLAGS) $(CPPFLAGS) -c $(SRCDIR)/main.f08 -o $(OBJDIR)/main.o

PHONEY: clean

clean:
	@-rm -rf $(OBJDIR)
	@-rm plot

