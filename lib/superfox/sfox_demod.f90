subroutine sfox_demod(crcvd,f,t,isync,s3)

  use sfox_mod
  complex crcvd(NMAX)                    !Signal as received
  complex c(0:NSPS-1)                    !Work array, one symbol long
  real s3(0:NQ-1,0:NN-1)                 !Synchronized symbol spectra
  integer isync(44)
!  integer ipk(1)

  j0=nint(12000.0*(t+0.5))
  df=12000.0/NSPS
  i0=nint(f/df)-NQ/2
  k=-1
  do n=1,NDS                             !Loop over all symbols
     if(any(isync(1:NS).eq.n)) cycle
     jb=n*NSPS + j0
     ja=jb-NSPS+1
     if(ja.lt.1 .or. jb.gt.NMAX) cycle
     k=k+1
     c=crcvd(ja:jb)
     call four2a(c,NSPS,1,-1,1)          !Compute symbol spectrum
     do i=0,NQ-1
        s3(i,k)=real(c(i0+i))**2 + aimag(c(i0+i))**2
     enddo
!     ipk=maxloc(s3(0:NQ-1,k))
!     if(k.lt.10) print*,'AAA',k,ipk(1)-1
  enddo

  call pctile(s3,NQ*NN,50,base)
  s3=s3/base

  return
end subroutine sfox_demod
