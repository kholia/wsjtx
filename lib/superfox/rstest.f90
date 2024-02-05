program rstest

  character arg*8
  integer dgen(235)                          !Generated data, i*4
  integer gsym0(255)                         !Encoded data, Karn
  integer gsym(255)                          !Encoded data with errors
  integer dat(235)                           !Decoded data, i*4
  integer iera(0:200)                        !Positions of erasures
      
  nargs=iargc()
  if(nargs.ne.5) then
     print*,'Usage:    rstest  M  N   K  nera nerr'
     print*,'Examples: rstest  6  63  12   0   25'
     print*,'          rstest  7 127  51   0   38'
     print*,'          rstest  8 255  51   0  102'
     print*,'          rstest  8 255 223   0   16'
     go to 999
  endif
  nkv=0
  call getarg(1,arg)
  read(arg,*) mm
  call getarg(2,arg)
  read(arg,*) nn
  call getarg(3,arg)
  read(arg,*) kk
  call getarg(4,arg)
  read(arg,*) nera
  call getarg(5,arg)
  read(arg,*) nerr
  
! Initialize the Karn codec
  nq=2**mm
  nfz=3
  call rs_init_sf(mm,nq,nn,kk,nfz)             !Initialize the Karn RS codec

! Generate random message, kk symbols with values 0 to nq-1
  do i=1,kk
     dgen(i)=i
  enddo

  write(*,1000)
1000 format('Generated data symbols, values 0-127:')
  write(*,1002) dgen(1:kk)
1002 format(20i4)

  call rs_encode_sf(dgen,gsym0)                 !Encode dgen into gsym0
  write(*,1004)
1004 format(/'Encoded channel symbols')
  write(*,1002) gsym0(1:nn)
  
  gsym=gsym0
  do i=1,nerr                                !Introduce errors
     gsym(i)=mod(gsym(i)+1,nq)
  enddo
  write(*,1006) nera
1006 format(/'Recovered channel symbols, with',i4,' errors at the start:')
  write(*,1002) gsym(1:nn)

  do i=0,nera-1
     iera(i)=i
  enddo

  call rs_decode_sf(gsym,iera,nera,dat,nfixed)
  ibad=count(dat(1:kk).ne.dgen(1:kk))
  write(*,1008)
1008 format(/'Decoded result:')
  write(*,1002) dat(1:kk)
  maxfix=(nn-kk)/2 + nera/2
  write(*,1100) nerr,nera,nfixed,maxfix
1100 format(/'nerr:',i3,'   nera:',i3,'   nfixed:',i3,'   maxfix:',i3)
  
999 end program rstest
 
