subroutine sfox_sync(iwave,fsample,isync,f,t,fwidth)

  use sfox_mod
  parameter (NSTEP=8)
  integer*2 iwave(0:NMAX-1)
  integer isync(44)
  integer ipeak(2)
  integer ipeak2(1)
  complex, allocatable :: c(:)             !Work array
  real, allocatable :: s(:,:)              !Symbol spectra, stepped by NSTEP 
  real, allocatable :: savg(:)             !Average spectrum
  real, allocatable :: ccf(:,:)
  real, allocatable :: s2(:)               !Fine spectrum of sync tone

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
        do m=1,NS
           k=isync(m)
           n=NSTEP*(k-1) + 1
           j=n+lag+j0
           if(j.ge.1 .and. j.le.jz) ccft=ccft + s(i,j)
        enddo  ! m
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

  dxj=0.
  if(jpk.gt.lag1 .and. jpk.lt.lag2) then
     call peakup(ccf(ipk,jpk-1),ccf(ipk,jpk),ccf(ipk,jpk+1),dxj)
  endif

  f=ibest*df + bw/2 + dxi*df
  t=(lagbest+dxj)*dtstep
  t=t-0.01                               !### Why is this needed? ###

  nfft2=4*NSPS
  deallocate(c)
  allocate(c(0:nfft2-1))
  allocate(s2(0:nfft2-1))

  i0=(t+0.5)*fsample
  s2=0.
  df2=fsample/nfft2
  do m=1,NS
     i1=i0+(isync(m)-1)*NSPS
     i2=i1+NSPS-1
     k=-1
     do i=i1,i2,2          !Load iwave data into complex array c0, for r2c FFT
        if(i.gt.0) then
           xx=iwave(i)
           yy=iwave(i+1)
        else
           xx=0.
           yy=0.
        endif
        k=k+1
        c(k)=fac*cmplx(xx,yy)
     enddo
     c(k+1:)=0.
     call four2a(c,nfft2,1,-1,0)              !r2c FFT
     do i=1,nfft2/4
        s2(i)=s2(i) + real(c(i))**2 + aimag(c(i))**2
     enddo
  enddo
  ipeak2=maxloc(s2)
  ipk=ipeak2(1)-1

  dxi=0.
  if(ipk.gt.1 .and. ipk.lt.nfft/4) then
     call peakup(s2(ipk-1),s2(ipk),s2(ipk+1),dxi)
  endif
  f=(ipk+dxi)*df2 + bw/2.0
  fwidth=0.

  if(ipk.gt.100 .and. ipk.lt.nfft2/4-100) then 
     call pctile(s2(ipk-100:ipk+100),201,48,base)
     s2=s2-base
     smax=maxval(s2(ipk-10:ipk+10))
     w=count(s2(ipk-10:ipk+10).gt.0.5*smax)
     if(w.gt.4.0) fwidth=sqrt(w*w - 4*4)*df2
  endif
  
  return
end subroutine sfox_sync
