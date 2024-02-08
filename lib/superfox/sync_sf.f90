subroutine sync_sf(crcvd,clo,verbose,f,t)

  use sfox_mod
  parameter (MMAX=150,JMAX=300)
  real s(-MMAX:MMAX,-JMAX:JMAX)          !s(DT,dFreq)
  complex clo(NMAX)                      !Complex Local Oscillator
  complex crcvd(NMAX)                    !Signal as received
  complex c(0:NFFT-1)                    !Work array
  logical verbose
  integer ipk(2)
  character*1 line(-30:30),mark(0:5)
  data mark/' ','.','-','+','X','$'/

  s=0.
  df=12000.0/NFFT                         !0.366211
  i1=ND1*nsps
  do m=-MMAX,MMAX
     lag=100*m
     c(0:nsync-1)=crcvd(i1+1+lag:i1+nsync+lag)*clo(1:nsync)
     c(nsync:)=0.
     call four2a(c,NFFT,1,-1,1)
     do j=-JMAX,JMAX
        k=j
        if(k.lt.0) k=k+NFFT
        s(m,j)=real(c(k))**2 + aimag(c(k))**2
     enddo
  enddo

  s=s/maxval(s)
  ipk=maxloc(s)
  ipk(1)=ipk(1)-MMAX-1
  ipk(2)=ipk(2)-JMAX-1
  if(verbose) then
     ma=max(-MMAX,ipk(1)-10)
     mb=min(MMAX,ipk(1)+10)
     ja=max(-JMAX,ipk(2)-30)
     jb=min(JMAX,ipk(2)+30)
     do m=ma,mb
        do j=ja,jb
           k=5.999*s(m,j)
           line(j-ipk(2))=mark(k)
        enddo
        write(*,1300) m/120.0,line
1300    format(f6.3,2x,61a1)
     enddo
  endif
  t=ipk(1)/120.0
  dfreq=ipk(2)*df
  f=1500.0+dfreq
!  write(*,3001) ipk,t,dfreq,f
!3001 format('B',2i5,f7.3,2f7.1)

  return
end subroutine sync_sf
