program rs_125_49

  include 'sfox_params.f90'
  character arg*8
  integer dgen(256)                          !Generated data, i*4
  integer*1 dgen1(7*KK)                      !Copy of dgen in 1-bit i*1 format
  integer gsym0(NN)                          !Encoded data, Karn
  integer gsym(NN)                           !Encoded data with errors
  integer dat(256)                           !Decoded data, i*4
  integer iera(0:100)                        !Positions of erasures
  character c357*357,c14*14,chkmsg*15

  nargs=iargc()
  if(nargs.ne.2) then
     print*,'Usage:   rs_125_49 nera nerr'
     print*,'Example: rs_125_49   0   38'
     go to 999
  endif
  call getarg(1,arg)
  read(arg,*) nera
  call getarg(2,arg)
  read(arg,*) nerr

! Initialize the Karn codec
  call rs_init_sf(MM,NQ,NN,KK,NFZ)

! Generate a message with sequential values in the range 0 to NQ-1
  dgen=0
  do i=1,KK-2
     dgen(i)=i
  enddo
  write(c357,'(51b7.7)') dgen(1:KK)
  read(c357,'(357i1)') dgen1
  call get_crc14(dgen1,7*KK,ncrc0)
  write(c14,'(b14.14)') ncrc0
  read(c14,'(2b7.7)') dgen(50:51)

  write(*,1000)
1000 format('Generated data symbols, values 0-127:')
  write(*,1002) dgen(1:KK)
1002 format(20i4)

  call rs_encode_sf(dgen,gsym0)              !Encode dgen into gsym0
  write(*,1004)
1004 format(/'Encoded channel symbols')
  write(*,1002) gsym0(1:NN)
  
  gsym0(KK-1:KK)=0                           !Puncture by removing the CRC
  gsym=gsym0
  do i=1,nerr                    !Introduce errors in the first nerr positions
     gsym(i)=mod(gsym(i)+1,NQ)
  enddo
  write(*,1006)
1006 format(/'Recovered channel symbols, punctured and with additional errors:')
  write(*,1002) gsym(1:NN)

  do i=0,nera-1                     !Specify locations of symbols to be erased
     iera(i)=i
  enddo

  call rs_decode_sf(gsym,iera,nera,dat,nfixed)  !Call the decoder
  write(c357,'(51b7.7)') dat(1:KK)
  read(c357,'(357i1)') dgen1
  call get_crc14(dgen1,7*KK,ncrc)

  write(*,1008)
1008 format(/'Decoded result:')
  chkmsg='Decode failed'
  if(nfixed.ge.0 .and. ncrc.eq.0) chkmsg='CRC OK'
  write(*,1002) dat(1:KK)
  maxfix=(nn-kk)/2 + nera/2
  write(*,1100) nerr,nera,nfixed,maxfix,trim(chkmsg)
1100 format(/'punctured: 2','   nerr:',i3,'   nera:',i3,'   nfixed:',i3, &
          '   maxfix:',i3,3x,a)

999 end program rs_125_49
