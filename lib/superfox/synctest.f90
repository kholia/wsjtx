program synctest

! Generate and test sync waveforms for possible use in SuperFox signal.

  use wavhdr
  include "sfox_params.f90"
  type(hdr) h                            !Header for .wav file
  integer*2 iwave(NMAX)                  !Generated i*2 waveform
  real*4 xnoise(NMAX)                    !Random noise
  real*4 dat(NMAX)                       !Generated real data
  complex cdat(NMAX)                     !Generated complex waveform
  complex clo(NMAX)                      !Complex Local Oscillator
  complex cnoise(NMAX)                   !Complex noise
  complex crcvd(NMAX)                    !Signal as received
!  real xdat(ND)                          !Temporary: for generating idat
  integer*1 idat(ND)                     !Encoded data, 7-bit integers
  integer*1 jdat(ND)                     !Recovered hard-decision symbols
  character fname*17,arg*12
  
  nargs=iargc()
  if(nargs.ne.8) then
     print*,'Usage:   synctest   f0    DT fspread delay width nran nfiles snr'
     print*,'Example: synctest 1500.0 2.5    0.0   0.0   100    0    10   -20'
     go to 999
  endif
  call getarg(1,arg)
  read(arg,*) f0
  call getarg(2,arg)
  read(arg,*) xdt
  call getarg(3,arg)
  read(arg,*) fspread
  call getarg(4,arg)
  read(arg,*) delay
  call getarg(5,arg)
  read(arg,*) syncwidth
  call getarg(6,arg)
  read(arg,*) nran
  call getarg(7,arg)
  read(arg,*) nfiles
  call getarg(8,arg)
  read(arg,*) snrdb

  rms=100.
  fsample=12000.0                   !Sample rate (Hz)
  baud=12000.0/nsps                 !Keying rate, 11.719 baud for nsps=1024
  h=default_header(12000,NMAX)
  idummy=0
  bandwidth_ratio=2500.0/6000.0
  sig=sqrt(2*bandwidth_ratio)*10.0**(0.05*snrdb)
  if(snrdb.gt.90.0) sig=1.0
  ngood=0

  do ifile=1,nfiles
     do i=1,ND
        call random_number(r)
        if(nran.eq.1) r=ran1(idummy)
        idat(i)=128*r
     enddo
  
     fname='000000_000001.wav'
     write(fname(8:13),'(i6.6)') ifile
     open(10,file=trim(fname),access='stream',status='unknown')

     xnoise=0.
     cnoise=0.
     if(snrdb.lt.90) then
        do i=1,NMAX                     !Generate Gaussian noise
           x=gran()
           y=gran()
           xnoise(i)=x
           cnoise(i)=cmplx(x,y)
        enddo
     endif

!Generate cdat (SuperFox waveform) and clo (LO needed for sync detection)
     call gen_sfox(idat,f0,fsample,syncwidth,cdat,clo)  

     crcvd=0.
     crcvd(1:NMAX)=cshift(sig*cdat(1:NMAX),-nint(xdt*fsample)) + cnoise

     dat=aimag(sig*cdat(1:NMAX)) + xnoise     !Add generated AWGN noise
     fac=32767.0
     if(snrdb.ge.90.0) iwave(1:NMAX)=nint(fac*dat(1:NMAX))
     if(snrdb.lt.90.0) iwave(1:NMAX)=nint(rms*dat(1:NMAX))
     write(10) h,iwave(1:NMAX)                !Save the .wav file
     close(10)

     if(fspread.ne.0 .or. delay.ne.0) call watterson(crcvd,NMAX,NZ,fsample,  &
          delay,fspread)

! Find signal freq and DT

     call sync_sf(crcvd,clo,f,t)
     ferr=f-f0
     terr=t-xdt
     if(abs(ferr).gt.10.0 .or. abs(terr).gt.0.04) cycle

     call hard_symbols(crcvd,f,t,jdat)
     nharderr=count(jdat.ne.idat)  
  
     write(*,1100) f0,xdt
1100 format(/'f0:',f7.1,'  xdt:',f6.2)
     write(*,1112) f,t
1112 format('f: ',f7.1,'   DT:',f6.2)
     write(*,1110) ferr,terr
1110 format('err:',f6.1,f12.2)
     write(*,1120) nharderr
1120 format('Hard errors:',i4)
     if(nharderr.le.38) ngood=ngood+1
     write(13,1200) ifile,snrdb,ferr,terr,nharderr
1200 format(i5,3f10.3,i5)
  enddo
  write(*,1300) snrdb,nfiles,ngood,float(ngood)/nfiles
1300 format(f7.2,2i5,f7.2)

999 end program synctest
