CC = gcc
FC = gfortran
FFLAGS = -O2 -Wall -fbounds-check
CFLAGS= -O9 -Wall

# Default rules
%.o: %.c
	${CC} ${CFLAGS} -c $<
%.o: %.f
	${FC} ${FFLAGS} -c $<
%.o: %.F
	${FC} ${FFLAGS} -c $<
%.o: %.f90
	${FC} ${FFLAGS} -c $<
%.o: %.F90
	${FC} ${FFLAGS} -c $<

all:	sfox_tx

OBJS1 = sfox_tx.o foxgen2.o sfox_assemble.o

sfox_tx: $(OBJS1)
	$(FC) -o sfox_tx $(OBJS1) libwsjt_fort.a

.PHONY : clean
clean:
	-rm -f *.o *.exe 
