program rst8

  character arg*8
  integer*1 dat0(223)                          !Generated data
  integer*1 parsym(32)                         !Parity symbols
  integer*1 cword0(255)                        !Generated codeword
  integer*1 cword(255)    !Rcvd codeword with errors; will be corrected in place
  integer iera(0:200)                          !Positions of additional erasures
  integer decode_rs_8
      
  nargs=iargc()
  if(nargs.ne.3) then
     print*,'Usage:   rst8  npad nera nerr'
     print*,'Example: rst8   178   0   16'
     go to 999
  endif
  nkv=0
  call getarg(1,arg)
  read(arg,*) npad
  call getarg(2,arg)
  read(arg,*) nera
  call getarg(3,arg)
  read(arg,*) nerr

! The basic code RS(255,223) is punctured with npad leading zeros.
  nn=255-npad
  kk=223-npad
  write(*,1000) nn,kk
1000 format('Basic code is RS(255,223).  npad:',i4,'   N:',i4,'   K:',i4)

! Generate a message, kk symbols with values 1 to kk.
  do i=1,kk
     dat0(i)=i
  enddo

  write(*,1002)
1002 format('Generated message symbols:')
  write(*,1004) dat0(1:kk)
1004 format(20i4)

  call encode_rs_8(dat0,parsym,npad)                 !Get parity symbols
  cword0(1:kk)=dat0(1:kk)                       !Genetated codeword
  cword0(kk+1:nn)=parsym(1:nn-kk)
  write(*,1006)
1006 format(/'Encoded channel symbols')
  write(*,1004) cword0(1:nn)
  
  cword=cword0
  do i=1,nerr                                !Introduce errors
     j=nn+1-i
     cword(j)=mod(cword(j)+1,256)
  enddo
  write(*,1008) nera
1008 format(/'Received channel symbols, with',i4,' errors at the end:')
  write(*,1004) cword(1:nn)

  do i=0,nera-1
     iera(i)=i
  enddo

  nfixed=decode_rs_8(cword,iera,nera,npad)
  ibad=count(cword(1:kk).ne.cword0(1:kk))

  write(*,1010)
1010 format(/'Decoded result:')
  write(*,1004) cword(1:kk)
  maxfix=(nn-kk)/2 + nera/2
  write(*,1100) nerr,nera,nfixed,maxfix
1100 format(/'nerr:',i3,'   nera:',i3,'   nfixed:',i3,'   maxfix:',i3)
  
999 end program rst8
