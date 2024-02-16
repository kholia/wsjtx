subroutine plotspec(dat)

  use sfox_mod
  real dat(NMAX)
  real s(0:NSPS/2)
  complex c(0:NSPS-1)
  integer ipk(1)

  nblks=NZ/NSPS
  s=0.
  fac=1.0/NSPS
  do j=1,nblks
     ib=j*NSPS
     ia=ib-NSPS+1
     c=fac*dat(ia:ib)
     call four2a(c,NSPS,1,-1,1)
     do i=0,NSPS/2
        s(i)=s(i) + real(c(i))**2 + aimag(c(i))**2
     enddo
  enddo

  df=12000.0/NSPS
  ipk=maxloc(s)
  f0=df*(ipk(1)-1)
  p_sig_plus_noise=maxval(s)
  p_noise=0.
  do i=0,NSPS/2
     f=i*df
     if(f.le.2500+df .and. abs(f-f0).gt.0.5*df) p_noise=p_noise + s(i)
     write(40,1000) f,s(i)
1000 format(2f10.3)
  enddo
  p_sig=p_sig_plus_noise - p_noise*df/2500.0
  snr=p_sig/p_noise
  snrdb=db(snr)
  write(*,1100) snrdb
1100 format('Measured SNR:',f7.2)

end subroutine plotspec
