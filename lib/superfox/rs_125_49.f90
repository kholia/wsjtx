program rs_125_49

  include 'sfox_params.f90'
  character arg*8
!  integer dgen(KK)                           !Generated data, i*4
  integer dgen(256)                          !Generated data, i*4
  integer*1 dgen1(7*KK)                      !Copy of dgen in 1-bit i*1 format
  integer gsym0(NN)                          !Encoded data, Karn
  integer gsym(NN)                           !Encoded data with errors
!  integer dat(KK)                            !Decoded data, i*4
  integer dat(256)                            !Decoded data, i*4
  integer era(NN)
  character c357*357,c14*14

  nargs=iargc()
  if(nargs.ne.1) then
     print*,'Usage:   rs_125_49 nerr'
     print*,'Example: rs_125_49  38'
     go to 999
  endif
  call getarg(1,arg)
  read(arg,*) nerr

! Initialize the Karn codec

  call rs_init_sf(MM,NQ,NN,KK,NFZ)             !Initialize RS(127,51)

! Generate random message with values 0 to NQ-1
  dgen=0
  do i=1,KK-2
     dgen(i)=int(NQ*ran1(idum))
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
  do i=1,nerr                                !Introduce errors
     gsym(i)=mod(gsym(i)+1,NQ)
  enddo
  write(*,1006)
1006 format(/'Recovered channel symbols, punctured and with additional errors:')
  write(*,1002) gsym(1:NN)

!  era(1)=KK-1
!  era(2)=KK
  nera=0

  call rs_decode_sf(gsym,era,nera,dat,nfixed)
  write(*,1008)
1008 format(/'Decoded result:')
  write(*,1002) dat(1:KK)
  if(nfixed.ge.0) write(*,1100) nerr,nfixed
1100 format(/'nerr:',i3,'   nfixed:',i3)
  if(nfixed.lt.0) write(*,1102) nerr,nfixed
1102 format(/'nerr:',i3,'   nfixed:',i3,', decode failed.')

  write(c357,'(51b7.7)') dat(1:KK)
  read(c357,'(357i1)') dgen1
  call get_crc14(dgen1,7*KK,ncrc)
  if(ncrc.ne.0) print*,'CRC check failed'
  if(ncrc.eq.0) print*,'CRC check is OK'

999 end program rs_125_49
