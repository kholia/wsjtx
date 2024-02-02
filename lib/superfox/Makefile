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

all:	rs_sf.a rstest rs_125_49

OBJS1 = rstest.o ran1.o
rstest: $(OBJS1)
	$(FC) -o rstest $(OBJS1) rs_sf.a

OBJS2 = rs_125_49.o ran1.o get_crc14.o
rs_125_49: $(OBJS2)
	$(FC) -o rs_125_49 $(OBJS2) rs_sf.a

rs_sf.a: init_rs_sf.o encode_rs_sf.o decode_rs_sf.o rs_sf.o
	ar -crs rs_sf.a init_rs_sf.o encode_rs_sf.o decode_rs_sf.o rs_sf.o

init_rs_sf.o: init_rs.c
	gcc -DBIGSYM=1 $(CFLAGS) -c -o $@ $^
encode_rs_sf.o: encode_rs.c
	gcc -DBIGSYM=1 $(CFLAGS) -c -o $@ $^
decode_rs_sf.o: decode_rs.c
	gcc -DBIGSYM=1 $(CFLAGS) -c -o $@ $^

.PHONY : clean
clean:
	-rm -f *.o *.a *.exe 
