program synctest

! Generate and test sync waveforms for possible use in SuperFox signal.

  use wavhdr
  include "sfox_params.f90"
  parameter (MMAX=150,JMAX=300)
  type(hdr) h                            !Header for .wav file
  integer*2 iwave(NMAX)                  !Generated i*2 waveform
  real*4 xnoise(NMAX)                    !Random noise
  real*4 dat(NMAX)                       !Generated real data
  real ccf(-MMAX:MMAX,-JMAX:JMAX)        !2D CCF: DT, dFreq offsets
  complex cdat(NMAX)                     !Generated complex waveform
  complex clo(NMAX)                      !Complex Local Oscillator
  complex cnoise(NMAX)                   !Complex noise
  complex crcvd(NMAX)                    !Signal as received
  complex c(0:NFFT-1)
  complex w,wstep,w0,wstep0
  integer ipk(2)
  character fname*17,arg*12
  character*1 line(-30:30),mark(0:5)
  data mark/' ','.','-','+','X','$'/
  
  nargs=iargc()
  if(nargs.ne.4) then
     print*,'Usage:   synctest   f0    DT width snr'
     print*,'Example: synctest 1500.0 2.5  1500 -20'
     go to 999
  endif
  call getarg(1,arg)
  read(arg,*) f0
  call getarg(2,arg)
  read(arg,*) xdt
  call getarg(3,arg)
  read(arg,*) syncwidth
  call getarg(4,arg)
  read(arg,*) snrdb

  rms=100.
  fsample=12000.0                   !Sample rate (Hz)
  twopi=8.0*atan(1.0)
  tsync=NS*nsps/fsample
  baud=12000.0/nsps                 !Keying rate, 11.719 baud for nsps=1024
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
  
  w0=1.0
  w=1.0
  a0=1500.0+ syncwidth/2.0          !Frequency at midpoint of LO waveform
  a1=f0 + syncwidth/2.0             !Frequency at midpoint of sync waveform
  a2=2.0*syncwidth/tsync            !Frequency drift rate

! Generate complex LO and sync signal
  x0=0.5*(nsync+1)
  s=2.0/nsync
  cdat=0.
  i0=nint(60.0*nsps + xdt*12000.0)
  do i=1,nsync
     if(i.eq.nsync/2+1) a2=-a2       !Reverse sign of drift at midpoint
     x=s*(i-x0)
     dphi0=(a0 + x*a2) * (twopi/fsample)
     wstep0=cmplx(cos(dphi0),sin(dphi0))
     w0=w0*wstep0
     dphi=(a1 + x*a2) * (twopi/fsample)
     wstep=cmplx(cos(dphi),sin(dphi))
     w=w*wstep
     clo(i)=conjg(sig*w0)
     cdat(i+i0)=sig*w
  enddo

  dat=aimag(cdat(1:NMAX)) + xnoise                 !Add generated AWGN noise
  fac=32767.0
  if(snrdb.ge.90.0) iwave(1:NMAX)=nint(fac*dat(1:NMAX))
  if(snrdb.lt.90.0) iwave(1:NMAX)=nint(rms*dat(1:NMAX))
  write(10) h,iwave(1:NMAX)                !Save the .wav file
  close(10)

  crcvd=0.
  crcvd(1:NMAX)=cdat(1:NMAX) + cnoise
  ccf=0.
  df=12000.0/NFFT                         !0.366211
  i1=60*nsps
  do m=-MMAX,MMAX
     lag=100*m
     c(0:nsync-1)=crcvd(i1+1+lag:i1+nsync+lag)*clo(1:nsync)
     c(nsync:)=0.
     fac=1.e-3
     c=fac*c
     call four2a(c,NFFT,1,-1,1)
     do j=-JMAX,JMAX
        k=j
        if(k.lt.0) k=k+NFFT
        ccf(m,j)=real(c(k))**2 + aimag(c(k))**2
     enddo
  enddo

  ccf=ccf/maxval(ccf)
  ipk=maxloc(ccf)
  print*,i0,ipk(1)
  ipk(1)=ipk(1)-MMAX-1
  ipk(2)=ipk(2)-JMAX-1
  ma=ipk(1)-10
  mb=ipk(1)+10
  ja=ipk(2)-30
  jb=ipk(2)+30
  do m=ma,mb
     do j=ja,jb
        k=5.999*ccf(m,j)
        line(j-ipk(2))=mark(k)
     enddo
     write(*,1300) m/120.0,line
1300 format(f6.3,2x,61a1)
  enddo
  t=ipk(1)/120.0
  dfreq=ipk(2)*df
  f=1500.0+dfreq
  write(*,1100) f0,xdt
1100 format(/'f0:',f7.1,'  xdt:',f6.2)
  write(*,1112) f,t
1112 format('f: ',f7.1,'   DT:',f6.2)
  write(*,1110) f-f0,t-xdt
1110 format('err:',f6.1,f12.2)

999 end program synctest
