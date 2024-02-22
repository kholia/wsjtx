subroutine sfox_sync(crcvd,fsample,f,t)

  use sfox_mod
  complex crcvd(NMAX)                      !Signal as received
  complex, allocatable :: c(:)             !Work array
  real, allocatable :: s(:,:)              !Symbol spectra, 1/8 symbol steps
!  character*1 line(-30:30),mark(0:5)
!  data mark/' ','.','-','+','X','$'/

  nh=NFFT1/2                               !1024
  istep=nh/8                               !128
  nsz=(nint(3.0*fsample) + NS*NSPS)/istep  !473
  df=fsample/NFFT1                         !5.86 Hz
  tstep=istep/fsample                      !0.0107 s

  allocate(c(0:nfft1-1))
  allocate(s(nh/2,nsz))

  ia=1-istep
  fac=1.0/NFFT1
  do j=1,nsz
     ia=ia+istep
     ib=ia+nh-1
     c(0:NSPS-1)=fac*crcvd(ia:ib)
     c(NSPS:)=0.
     call four2a(c,NFFT1,1,-1,1)
     do i=1,nh/2
        s(i,j)=real(c(i))**2 + aimag(c(i))**2
     enddo
  enddo

  pmax=0.
  ntol=100
  iz=nint(ntol/df)
  i0=nint(1500.0/df)
  ipk=-999
  jpk=-999
  do j=1,nsz-8*NS
     do i=-iz,iz
        p=0.
        do k=1,NS
           ii=i0+i+(2*(isync(k)-NQ/2))
           jj=j + 8*(k-1)
           p=p + s(ii,jj)
        enddo
        if(p.gt.pmax) then
           pmax=p
           ipk=i
           jpk=j
        endif
     enddo
  enddo

  dfreq=ipk*df
  f=1500.0+dfreq
  t=(jpk-201)*istep/fsample

  return
end subroutine sfox_sync
