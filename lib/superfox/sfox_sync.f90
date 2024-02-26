subroutine sfox_sync(iwave,fsample,isync,f,t)

  use sfox_mod
  parameter (NSTEP=8)
  integer*2 iwave(0:NMAX-1)
  integer isync(44)
  integer ipeak(2)
  complex, allocatable :: c(:)             !Work array
  real x(171)
  real, allocatable :: s(:,:)              !Symbol spectra, stepped by NSTEP 
  real, allocatable :: savg(:)             !Average spectrum
  real, allocatable :: ccf(:,:)
!  character*1 line(-15:15),mark(0:6),c1
!  data mark/' ','.','-','+','X','$','#'/

  nfft=nsps
  nh=nfft/2
  istep=NSPS/NSTEP
  jz=(13.5*fsample)/istep
  df=fsample/nfft
  dtstep=istep/fsample
  fsync=1500.0-bw/2
  ftol=20.0
  ia=nint((fsync-ftol)/df)
  ib=nint((fsync+ftol)/df)
  lagmax=1.5/dtstep
  lag1=-lagmax
  lag2=lagmax

  x=0.
  do i=1,NS
     x(isync(i))=1.0
  enddo

  allocate(s(0:nh/2,jz))
  allocate(savg(0:nh/2))
  allocate(c(0:nfft-1))
  allocate(ccf(ia:ib,lag1:lag2))

  s=0.
  savg=0.
  fac=1.0/nfft

! Compute symbol spectra with df=baud/2 and NSTEP steps per symbol.
  do j=1,jz
     i1=(j-1)*istep
     i2=i1+nsps-1
     k=-1
     do i=i1,i2,2          !Load iwave data into complex array c0, for r2c FFT
        xx=iwave(i)
        yy=iwave(i+1)
        k=k+1
        c(k)=fac*cmplx(xx,yy)
     enddo
     c(k+1:)=0.
     call four2a(c,nfft,1,-1,0)              !r2c FFT
     do i=1,nh/2
        s(i,j)=real(c(i))**2 + aimag(c(i))**2
        savg(i)=savg(i) + s(i,j)
     enddo
  enddo
  savg=savg/jz

  ccfbest=0.
  ibest=0
  lagpk=0
  lagbest=0
  j0=0.5/dtstep                        !Nominal start-signal index
  
  do i=ia,ib
     ccfmax=0.
     do lag=lag1,lag2
        ccft=0.
        do kk=1,NS
           k=isync(kk)
           n=NSTEP*(k-1) + 1
           j=n+lag+j0
           if(j.ge.1 .and. j.le.jz) ccft=ccft + s(i,j)
        enddo  ! kk
        ccft=ccft - NS*savg(i)
        ccf(i,lag)=ccft
        if(ccft.gt.ccfmax) then
           ccfmax=ccft
           lagpk=lag
        endif
     enddo  ! lag

     if(ccfmax.gt.ccfbest) then
        ccfbest=ccfmax
        ibest=i
        lagbest=lagpk
     endif
  enddo  ! i

  ipeak=maxloc(ccf)
  ipk=ipeak(1)-1+ia
  jpk=ipeak(2)-1+lag1

  dxi=0.
  dxj=0.
  if(ipk.gt.ia .and. ipk.lt.ib) then
     call peakup(ccf(ipk-1,jpk),ccf(ipk,jpk),ccf(ipk+1,jpk),dxi)
  endif
  if(jpk.gt.lag1 .and. jpk.lt.lag2) then
     call peakup(ccf(ipk,jpk-1),ccf(ipk,jpk),ccf(ipk,jpk+1),dxj)
  endif

  f=ibest*df + bw/2 + dxi*df
  t=lagbest*dtstep + dxj*dtstep
!  write(*,4100) ibest,lagbest,f,dxi*df,t,dxj*dtstep
!4100 format(2i6,2f10.1,2f10.3)

  nsum=0
  sq=0.
  do lag=lag1,lag2
     if(abs(lag-lagbest).gt.3) then
        sq=sq + ccf(ibest,lag)**2
        nsum=nsum+1
     endif
     write(51,3051) lag*dtstep,ccf(ibest,lag)
3051 format(2f12.4)
  enddo

  rms=sqrt(sq/nsum)
  snrsync=ccf(ibest,lagbest)/rms
!  print*,'snr:',snrsync

  return
end subroutine sfox_sync
