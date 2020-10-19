#
NAME=plasmaGRPY
DIR=../../bin/GRPYLinux
PLASMADIR=../../ThirdParty/plasma-installer_2.8.0
CPU=exe
NAMEFUL=$(DIR)/$(NAME).$(CPU)
FC = gfortran
FFLAGS = -c -O2 -ffpe-summary='none'
LD = gfortran
LDFLAGS = -O2 -ffpe-summary='none'
LIBRARIES= -lplasma -lcoreblasqw -lcoreblas -lquark -llapacke -ltmg -llapack -lf77blas -lcblas -latlas -lpthread -lm -lhwloc
LIBS= -L$(PLASMADIR)/install/lib 
INCLUDES= -I$(PLASMADIR)/install/include

.f.o :
	$(FC) $(FFLAGS) $(INCLUDES) $<

OBJECTS = \
$(NAME).o

$(NAMEFUL) : $(OBJECTS)
	$(LD) -o $(NAMEFUL) $(LDFLAGS) $(OBJECTS) $(LIBS) $(LIBRARIES)

all: $(OBJECTS)
	$(LD) -o $(NAMEFUL) $(LDFLAGS) $(OBJECTS) $(LIBS) $(LIBRARIES)

clean:
	rm -f *.o *.mod $(NAMEFUL)
