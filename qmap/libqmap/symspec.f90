subroutine symspec(k,ndiskdat,nb,nbslider,nfsample,    &
     pxdb,ssz5a,nkhz,ihsym,nzap,slimit,lstrong)

!  k        pointer to the most recent new data
!  ndiskdat 0/1 to indicate if data from disk
!  nb       0/1 status of noise blanker
!  nfsample sample rate (Hz)
!  pxdb     power in x channel (0-60 dB)
!  ssz5a    polarized spectrum, for waterfall display
!  nkhz     integer kHz portion of center frequency, e.g., 125 for 144.125
!  ihsym    index number of this half-symbol (1-322)
!  nzap     number of samples zero'ed by noise blanker

  include 'njunk.f90'
  parameter (NSMAX=60*96000)          !Total sample intervals per minute
  parameter (NFFT=32768)              !Length of FFTs
  real*8 ts,hsym
  real*8 fcenter
  common/datcom/dd(2,5760000),ss(322,NFFT),savg(NFFT),fcenter,nutc,  &
       junk(NJUNK)
  real*4 ssz5a(NFFT),w(NFFT),w2a(NFFT),w2b(NFFT)
  complex cx(NFFT)
  complex cx00(NFFT)
  complex cx0(0:1023),cx1(0:1023)
  logical*1 lstrong(0:1023)
  data rms/999.0/,k0/99999999/,nadjx/0/,nadjy/0/
  save

  nfast=1
  if(k.gt.5751000) go to 999
  if(k.lt.NFFT) then
     ihsym=0
     go to 999             !Wait for enough samples to start
  endif
  if(k0.eq.99999999) then
     pi=4.0*atan(1.0)
     w2a=0.
     w2b=0.
     do i=1,NFFT
        w(i)=(sin(i*pi/NFFT))**2                          !Window for nfast=1
        if(i.lt.17833) w2a(i)=(sin(i*pi/17832.925))**2    !Window a for nfast=2
        j=i-8916
        if(j.gt.0 .and. j.lt.17833) w2b(i)=(sin(j*pi/17832.925))**2    ! b
     enddo
     w2a=sqrt(2.0)*w2a
     w2b=sqrt(2.0)*w2b
  endif

  hsym=2048.d0*96000.d0/11025.d0      !Samples per JT65 half-symbol
  if(nfsample.eq.95238)   hsym=2048.d0*95238.1d0/11025.d0

  if(k.lt.k0) then
     ts=1.d0 - hsym
     savg=0.
     ihsym=0
     k1=0
     if(ndiskdat.eq.0) dd(1:2,k+1:5760000)=0.  !### Should not be needed ??? ###
  endif
  k0=k

  nzap=0
  sigmas=1.5*(10.0**(0.01*nbslider)) + 0.7
  peaklimit=sigmas*max(10.0,rms)
  faclim=3.0
  px=0.

  nwindow=2
  nfft2=1024
  kstep=nfft2
  if(nwindow.ne.0) kstep=nfft2/2
  nblks=(k-k1)/kstep
  do nblk=1,nblks
     j=k1+1
     do i=0,nfft2-1
        cx0(i)=cmplx(dd(1,j+i),dd(2,j+i))
     enddo
     call timf2(k,nfft2,nwindow,nb,peaklimit,       &
          faclim,cx0,cx1,slimit,lstrong,   &
          px,nzap)

     do i=0,kstep-1
        dd(1,j+i)=real(cx1(i))
        dd(2,j+i)=aimag(cx1(i))
     enddo
     k1=k1+kstep
  enddo

  npts=NFFT                           !Samples used in each half-symbol FFT

  ts=ts+hsym
  ja=ts                               !Index of first sample
  jb=ja+npts-1                        !Last sample

  i=0
  fac=0.0002
  do j=ja,jb                          !Copy data into cx
     x1=dd(1,j)
     x2=dd(2,j)
     i=i+1
     cx(i)=fac*cmplx(x1,x2)
  enddo

  if(nzap/178.lt.50 .and. (ndiskdat.eq.0 .or. ihsym.lt.280)) then
     nsum=nblks*kstep - nzap
     if(nsum.le.0) nsum=1
     rmsx=sqrt(0.5*px/nsum)
     rms=rmsx
  endif
  pxdb=0.
  if(rmsx.gt.1.0) pxdb=20.0*log10(rmsx)
  if(pxdb.gt.60.0) pxdb=60.0

  cx00=cx

  do mm=1,nfast
     ihsym=ihsym+1
     if(nfast.eq.1) then
        cx=w*cx00                           !Apply window for 2nd forward FFT
     else
        if(mm.eq.1) then
           cx=w2a*cx00
        else
           cx=w2b*cx00
        endif
     endif

     call four2a(cx,NFFT,1,1,1)          !Second forward FFT (X)

     n=min(322,ihsym)
     do i=1,NFFT
        sx=real(cx(i))**2 + aimag(cx(i))**2  
        ss(n,i)=sx                    ! Pol = 0
        savg(i)=savg(i) + sx
        ssz5a(i)=sx
     enddo
  enddo

  nkhz=nint(1000.d0*(fcenter-int(fcenter)))
  if(fcenter.eq.0.d0) nkhz=125

999 return
end subroutine symspec
