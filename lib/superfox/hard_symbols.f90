subroutine hard_symbols(crcvd,f,t,chansym)

  use sfox_mod
!  include "sfox_params.f90"
  complex crcvd(NMAX)                    !Signal as received
  complex c(0:NSPS-1)                    !Work array, one symbol long
  real s(0:NQ-1)                         !Power spectrum
  integer chansym(NN)                    !Recovered hard-decision symbols
  integer ipk(1)

  i0=nint(12000.0*t)
  df=12000.0/NSPS
  j0=nint(f/df)-NQ/2
  do n=1,ND                             !Loop over all symbols
     ib=n*NSPS + i0
     if(n.gt.ND1) ib=(NS+n)*NSPS + i0
     ia=ib-NSPS+1
     if(ia.lt.1 .or. ib.gt.NMAX) cycle
     c=crcvd(ia:ib)
     call four2a(c,NSPS,1,-1,1)          !Compute symbol spectrum
     do j=0,NQ-1
        s(j)=real(c(j0+j))**2 + aimag(c(j0+j))**2
     enddo

! Could we measure fspread, perhaps in the sync routine, and use that to
! decide whether to smooth spectra here?
!     call smo121(s,NSPS)                !Helps for LD, HM propagation...
!     call smo121(s,NSPS)

     ipk=maxloc(s(0:NQ-1))
     chansym(n)=ipk(1) - 1
  enddo

  return
end subroutine hard_symbols
