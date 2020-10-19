#
NAME=lapackGRPY
DIR=../../lib/
FC = gfortran
FFLAGS = -c -O2

.f.o :
	$(FC) $(FFLAGS) $<

OBJECTS = \
lapblas_matInv.o lapack_dgsev.o lapack_dsyev.o

$(NAMEFUL) : $(OBJECTS)
	ar cr $(DIR)lib$(NAME).a $(OBJECTS)

all: $(OBJECTS)
	ar cr $(DIR)lib$(NAME).a $(OBJECTS)

clean:
	rm -f *.o $(DIR)lib$(NAME).a
