program rstest
  
  character arg*8
  integer dat0(255)                          !Message symbols
  integer parsym(255)                        !Parity symbols
  integer chansym0(255)                      !Encoded data, Karn
  integer chansym(255)                       !Encoded data with errors
  integer dat(235)                           !Decoded data, i*4
!  integer, target :: parsym(255)
  integer iera(0:200)                        !Positions of erasures
  integer gfpoly
!  type(c_ptr) :: rs

  data gfpoly/z'11d'/
      
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

! Generate kk message symbols.  (Values must be in  range 0 to nq-1.)
  do i=1,kk
     dat0(i)=i
  enddo

  write(*,1000) mm,nn,kk,nera,nerr
1000 format('M:',i2,'   N:',i4,'   K:',i4,'   nera:',i4,'   nerr:',i4/ &
            'Generated data symbols')
  write(*,1002) dat0(1:kk)
1002 format(20i4)

  call rs_encode_sf(dat0,parsym)                 !Compute parity symbols
  chansym0(1:kk)=dat0(1:kk)
  chansym0(kk+1:nn)=parsym(1:nn-kk)

  write(*,1004)
1004 format(/'Encoded channel symbols')
  write(*,1002) chansym0(1:nn)
  
  chansym=chansym0
  do i=1,nerr                                !Introduce errors
     chansym(i)=mod(chansym(i)+1,nq)
  enddo
  write(*,1006) nera
1006 format(/'Recovered channel symbols, with',i4,' errors at the start:')
  write(*,1002) chansym(1:nn)

  do i=0,nera-1
     iera(i)=i
  enddo

  call rs_decode_sf(chansym,iera,nera,nfixed)
  dat(1:kk)=chansym(1:kk)
  ibad=count(dat(1:kk).ne.dat0(1:kk))
  write(*,1008)
1008 format(/'Decoded result:')
  write(*,1002) dat(1:kk)
  maxfix=(nn-kk)/2 + nera/2
  write(*,1100) nerr,nera,nfixed,maxfix
1100 format(/'nerr:',i3,'   nera:',i3,'   nfixed:',i3,'   maxfix:',i3)
  
999 end program rstest
 
