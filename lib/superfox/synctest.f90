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
  real xdat(ND)                          !Temporary: for generating idat
  integer*1 idat(ND)                     !Encoded data, 7-bit integers
  character fname*17,arg*12
  
  nargs=iargc()
  if(nargs.ne.6) then
     print*,'Usage:   synctest   f0    DT fspread delay width snr'
     print*,'Example: synctest 1500.0 2.5    0.0   0.0   100  -20'
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
  read(arg,*) snrdb

  rms=100.
  fsample=12000.0                   !Sample rate (Hz)
  baud=12000.0/nsps                 !Keying rate, 11.719 baud for nsps=1024

  call random_number(xdat)
  idat=int(127.9999*xdat)
  
  h=default_header(12000,NMAX)
  fname='000000_000001.wav'
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

  bandwidth_ratio=2500.0/6000.0
  sig=sqrt(2*bandwidth_ratio)*10.0**(0.05*snrdb)
  if(snrdb.gt.90.0) sig=1.0

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

  write(*,1100) f0,xdt
1100 format(/'f0:',f7.1,'  xdt:',f6.2)
  write(*,1112) f,t
1112 format('f: ',f7.1,'   DT:',f6.2)
  write(*,1110) f-f0,t-xdt
1110 format('err:',f6.1,f12.2)

999 end program synctest

  include 'gen_sfox.f90'
  include 'sync_sf.f90'

