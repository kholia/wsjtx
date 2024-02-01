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

all:	rstest

OBJS2 = rstest.o init_rs_sf.o encode_rs_sf.o decode_rs_sf.o \
	rs_sf.o ran1.o
rstest: $(OBJS2)
	$(FC) -o rstest $(OBJS2)

init_rs_sf.o: init_rs.c
	gcc -DBIGSYM=1 $(CFLAGS) -c -o $@ $^
encode_rs_sf.o: encode_rs.c
	gcc -DBIGSYM=1 $(CFLAGS) -c -o $@ $^
decode_rs_sf.o: decode_rs.c
	gcc -DBIGSYM=1 $(CFLAGS) -c -o $@ $^

.PHONY : clean
clean:
	-rm -f *.o rstest.exe 
