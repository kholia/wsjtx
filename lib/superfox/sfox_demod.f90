subroutine sfox_demod(crcvd,f,t,s3,chansym)

  use sfox_mod
  complex crcvd(NMAX)                    !Signal as received
  complex c(0:NSPS-1)                    !Work array, one symbol long
  real s(0:NQ-1)                         !Power spectrum
  real s3(0:NQ-1,0:NN-1)                 !Symbol spectra
  integer chansym(NN)                    !Hard-decision symbol values
  integer ipk(1)

  i0=nint(12000.0*t)
  df=12000.0/NSPS
  j0=nint(f/df)-NQ/2
  do n=1,NN                             !Loop over all symbols
     ib=n*NSPS + i0
     if(n.gt.ND1) ib=(NS+n)*NSPS + i0
     ia=ib-NSPS+1
     chansym(n)=0
     if(ia.lt.1 .or. ib.gt.NMAX) cycle
     c=crcvd(ia:ib)
     call four2a(c,NSPS,1,-1,1)          !Compute symbol spectrum
     do j=0,NQ-1
        s(j)=real(c(j0+j))**2 + aimag(c(j0+j))**2
        s3(j,n-1)=s(j)
     enddo

! Could we measure fspread, perhaps in the sync routine, and use that to
! decide whether to smooth spectra here?
!     call smo121(s,NSPS)                !Helps for LD, HM propagation...
!     call smo121(s,NSPS)

     ipk=maxloc(s(0:NQ-1))
     chansym(n)=ipk(1) - 1
  enddo

  return
end subroutine sfox_demod
