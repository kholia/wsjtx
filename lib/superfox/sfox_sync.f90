subroutine sfox_sync(iwave,fsample,isync,f,t)

  use sfox_mod
  parameter (NSTEP=8)
  integer*2 iwave(NMAX)
  integer isync(44)
  integer ipeak(1)
  complex, allocatable :: c(:)             !Work array
  real x(171)
  real, allocatable :: s(:,:)              !Symbol spectra, stepped by NSTEP 
  real, allocatable :: savg(:)             !Average spectrum
  real, allocatable :: ccf(:,:)
!  character*1 line(-15:15),mark(0:6),c1
!  data mark/' ','.','-','+','X','$','#'/

  nh=NFFT1/2
  istep=NSPS/NSTEP
  jz=(13.5*fsample)/istep
  df=fsample/NFFT1
  dtstep=istep/fsample
  fsync=1500.0-bw/2
  ftol=20.0
  ia=nint((fsync-ftol)/df)
  ib=nint((fsync+ftol)/df)
  lagmax=1.0/dtstep
  lag1=0
  lag2=lagmax

  x=0.
  do i=1,NS
     x(isync(i))=1.0
  enddo

  allocate(s(0:nh/2,jz))
  allocate(savg(0:nh/2))
  allocate(c(0:NFFT1-1))
  allocate(ccf(ia:ib,lag1:lag2))

  s=0.
  savg=0.
  fac=1.0/NFFT1

! Compute symbol spectra with df=baud/2 and NSTEP steps per symbol.
  do j=1,jz
     k=(j-1)*istep
     do i=0,nh-1
        c(i)=cmplx(fac*iwave(k+2*i+1),fac*iwave(k+2*i+2))
     enddo
     c(nh:)=0.
     call four2a(c,NFFT1,1,-1,0)           !Forward FFT, r2c
     do i=0,nh/2
        p=real(c(i))*real(c(i)) + aimag(c(i))*aimag(c(i))
        s(i,j)=p
        savg(i)=savg(i) + p
     enddo
     ipeak=maxloc(s(ia:ib,j))
!     print*,j,ipeak(1)+ia-1
  enddo
  savg=savg/jz

!###

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
  f=ibest*df + bw/2
  t=lagbest*dtstep
!  write(*,4100) ibest,lagbest,f,t
!4100 format(2i6,f10.1,f10.3)

!  print*,'aaa',ibest,lagbest
  do lag=lag1,lag2
     write(51,3051) lag*dtstep,ccf(ibest,lag)
3051 format(2f12.4)
  enddo

  return
end subroutine sfox_sync
