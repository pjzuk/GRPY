#
NAME=GRPY
DIR=../../bin/GRPYLinux
CPU=exe
NAMEFUL=$(DIR)/$(NAME).$(CPU)
FC = gfortran
FFLAGS = -c -O2 -ffpe-summary='none'
LD = gfortran
LDFLAGS = -O2
LIBRARIES= -llapackGRPY
LIBS= -L../../lib/

.f.o :
	$(FC) $(FFLAGS) $<

OBJECTS = \
$(NAME).o

$(NAMEFUL) : $(OBJECTS)
	$(LD) -o $(NAMEFUL) $(LDFLAGS) $(OBJECTS) $(LIBS) $(LIBRARIES)

all: $(OBJECTS)
	$(LD) -o $(NAMEFUL) $(LDFLAGS) $(OBJECTS) $(LIBS) $(LIBRARIES)

clean:
	rm -f *.o *.mod $(NAMEFUL)
