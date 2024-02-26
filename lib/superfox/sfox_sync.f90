subroutine sfox_sync(iwave,fsample,isync,f,t)

  use sfox_mod
  parameter (NSTEPS=8)
  integer*2 iwave(NMAX)
  integer isync(44)
  integer ipeak(1)
  complex, allocatable :: c(:)             !Work array
  real x(171)
  real, allocatable :: s(:,:)              !Symbol spectra, stepped by NSTEPS 
  real, allocatable :: savg(:)             !Average spectrum
  real, allocatable :: ccf(:,:)            !
  character*1 line(-15:15),mark(0:6),c1
  data mark/' ','.','-','+','X','$','#'/

  nh=NFFT1/2
  istep=NSPS/NSTEPS
  jz=(13.5*fsample)/istep
  df=fsample/NFFT1
  tstep=istep/fsample
  x=0.
  do i=1,NS
     x(isync(i))=1.0
  enddo

  allocate(s(0:nh/2,jz))
  allocate(savg(0:nh/2))
  allocate(c(0:NFFT1-1))

  s=0.
  savg=0.
  fac=1.0/NFFT1
! Compute symbol spectra with df=baud/2 and NSTEPS steps per symbol.
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
  enddo

  pmax=maxval(s(82:112,1:jz))
  s=s/pmax
  do j=jz,1,-1
     do i=-15,15
        k=6.001*s(97+i,j)
        line(i)=mark(k)
     enddo
     c1=' '
     k=j/NSTEPS + 1
     if(k.le.171) then
        if(x(k).ne.0.0) c1='*'
     endif
!     write(*,2001) j,c1,line
!2001 format(i3,2x,a1,' |',31a1,'|')
     xx=0
     if(c1.eq.'*') xx=1
     write(44,3044) j*tstep,xx,3.5*s(96:98,j)
3044 format(f10.4,4f10.4)
  enddo
     
  savg=savg/jz
  ipeak=maxloc(savg(82:112))
  i0=ipeak(1)+81
  dxi=0.
!  if(i0.gt.0 .and. i0.lt.nh/2) then
!     call peakup(savg(i0-1),savg(i0),savg(i0+1),dxi)
!  endif
  f=(i0+dxi)*df + bw/2.0

  do j=1,jz
     k=j/NSTEPS + 1
     xx=0
     if(k.le.171) xx=x(k)
     write(43,3043) j,s(i0,j),xx
3043 format(i5,2f12.3)
  enddo
  lagmax=1.0/tstep + 1
  pmax=0.
  lagpk=-99
!  print*,i0,jz,tstep,lagmax
  do lag=0,lagmax
     p=0.
     do i=1,NS
        k=NSTEPS*(isync(i)-1) + 1 + lag
        p=p + s(i0,k)
     enddo
     p=p/NS
     if(p.gt.pmax) then
        pmax=p
        lagpk=lag
     endif
     write(42,3042) lag,lag*tstep,p
3042 format(i5,2f15.3)
  enddo
  t=lagpk*tstep
!  print*,f,t
  if(NS.ne.-99) return
  
  nsz=(nint(3.0*fsample) + NS*NSPS)/istep


  pmax=0.
  ntol=100
  iz=nint(ntol/df)
  i0=nint(1500.0/df)
  ipk=-999
  jpk=-999
  jz=nsz-NSTEPS*NS
  allocate(ccf(-iz:iz,1:jz))
  ccf=0.
  do j=1,jz
     do i=-iz,iz
        p=0.
        do k=1,NS
           ii=i0+i+(2*(isync(k)-NQ/2))
           jj=j + NSTEPS*(k-1)
           p=p + s(ii,jj)
        enddo
        ccf(i,j)=p
        if(p.gt.pmax) then
           pmax=p
           ipk=i
           jpk=j
        endif
     enddo
  enddo

  dxi=0.
  dxj=0.
  if(jpk.gt.1 .and. jpk.lt.jz .and. abs(ipk).lt.iz) then
     call peakup(ccf(ipk-1,jpk),ccf(ipk,jpk),ccf(ipk+1,jpk),dxi)
     call peakup(ccf(ipk,jpk-1),ccf(ipk,jpk),ccf(ipk,jpk+1),dxj)
  endif

  dfreq=(ipk+dxi)*df
  f=1500.0+dfreq
  t=(jpk+dxj-201.0)*istep/fsample

  return
end subroutine sfox_sync
