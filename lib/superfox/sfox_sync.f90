subroutine sfox_sync(crcvd,nv,f,t)

  use sfox_mod
  parameter (NFFT2=2048,NH=NFFT2/2)
  parameter (NSZ=562)                    !Number of 1/8-symbol steps
  complex clo(NMAX)                      !Complex Local Oscillator
  complex crcvd(NMAX)                    !Signal as received
  complex c(0:NFFT2-1)                   !Work array
  real s(NH/2,NSZ)
!  character*1 line(-30:30),mark(0:5)
!  data mark/' ','.','-','+','X','$'/

  df=12000.0/NFFT2                       !5.86 Hz
  istep=NH/8
  tstep=istep/12000.0                    !0.0107 s
  ia=1-istep
  fac=1.0/NFFT2
  do j=1,NSZ
     ia=ia+istep
     ib=ia+NH-1
     c(0:NSPS-1)=fac*crcvd(ia:ib)
     c(NSPS:)=0.
     call four2a(c,NFFT2,1,-1,1)
     do i=1,NH/2
        s(i,j)=real(c(i))**2 + aimag(c(i))**2
     enddo
  enddo

  pmax=0.
  ntol=100
  iz=nint(ntol/df)
  i0=nint(1500.0/df)
  ipk=-999
  jpk=-999
  do j=1,NSZ-8*NS
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
  t=(jpk-201)*128.0/12000.0
!  write(*,4001) ipk,jpk,pmax,dfreq,t
!4001 format(2i8,3f10.3)

  return
end subroutine sfox_sync
