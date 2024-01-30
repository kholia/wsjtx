program rstest

  character arg*8
  integer dgen(235)                          !Generated data, i*4
  integer gsym0(255)                         !Encoded data, Karn
  integer gsym(255)                          !Encoded data with errors
  integer dat(235)                           !Decoded data, i*4
      
  nargs=iargc()
  if(nargs.ne.4) then
     print*,'Usage:   rstest  M  N   K  nerr'
     print*,'Example: rstest  7 127 51   38'
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
  read(arg,*) nerr

! Initialize the Karn codec
  nq=2**mm
  nfz=3
  call rs_init(mm,nq,nn,kk,nfz)             !Initialize the Karn RS codec

! Generate and random symbols wuth values 0 to nq-1
  do i=1,kk
     dgen(i)=(nq-0.0001)*ran1(idum)
  enddo

  write(*,1000)
1000 format('Generated data symbols, values 0-127:')
  write(*,1002) dgen(1:kk)
1002 format(20i4)

  call rs_encode(dgen,gsym0)                 !Encode dgen into gsym0
  write(*,1004)
1004 format(/'Encoded channel symbols')
  write(*,1002) gsym0(1:nn)
  
  gsym=gsym0
  do i=1,nerr                                !Introduce errors
     gsym(i)=mod(gsym(i)+1,nq)
  enddo
  write(*,1006)
1006 format(/'Recovered channel symbols, with errors:')
  write(*,1002) gsym(1:nn)

  call rs_decode(gsym,era,0,dat,nfixed)
  ibad=0
  do i=1,kk
     if(dat(i).ne.dgen(i)) ibad=ibad+1
  enddo
  write(*,1008)
1008 format(/'Decoded result:')
  write(*,1002) dat(1:kk)
  write(*,1100) nerr,nfixed,ibad
1100 format(/'nerr:',i3,'   nfixed:',i3,'   ibad:',i3)
  
999 end program rstest
 
