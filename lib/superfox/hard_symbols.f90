subroutine hard_symbols(crcvd,f,t,jdat)

  include "sfox_params.f90"
  complex crcvd(NMAX)                    !Signal as received
  complex c(0:NSPS-1)                    !Work array, one symbol long
  real s(0:NSPS-1)                       !Power spectrum
  integer jdat(ND)                       !Recovered hard-decision symbols
  integer ipk(1)

  i0=nint(12000.0*t)
  df=12000.0/NSPS
  j0=nint(f/df)-128
  do n=1,ND                             !Loop over all symbols
     ib=n*NSPS + i0
     if(n.gt.ND1) ib=(NS+n)*NSPS + i0
     ia=ib-NSPS+1
     if(ia.lt.1 .or. ib.gt.NMAX) cycle
     c=crcvd(ia:ib)
     call four2a(c,NSPS,1,-1,1)          !Compute symbol spectrum
     do i=0,NSPS-1
        s(i)=real(c(i))**2 + aimag(c(i))**2
     enddo
     ipk=maxloc(s)
     ipk(1)=ipk(1)-j0
     if(ipk(1).ge.64) then
        jdat(n)=ipk(1)-64
     else
        jdat(n)=ipk(1)+256-64
     endif
  enddo

  return
end subroutine hard_symbols
