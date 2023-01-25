subroutine q65_sync(ss,i0,nts_q65,sync_ok,snr,xdt)

! Test for presence of Q65 sync tone

  parameter (NFFT=32768)
  parameter (LAGMAX=33)
  real ss(322,NFFT)                !Symbol spectra
  real ccf(0:LAGMAX)               !The WSJT "blue curve", peak at DT
  logical sync_ok
  logical first
  integer isync(22),ipk(1)

! Q65 sync symbols
  data isync/1,9,12,13,15,22,23,26,27,33,35,38,46,50,55,60,62,66,69,74,76,85/
  data first/.true./
  save first,isync

  tstep=2048.0/11025.0        !0.185760 s: 0.5*tsym_jt65, 0.3096*tsym_q65
  if(first) then
     fac=0.6/tstep            !3.230
     do i=1,22                                !Expand the Q65 sync stride
        isync(i)=nint((isync(i)-1)*fac) + 1
     enddo
     first=.false.
  endif

  m=nts_q65/2
  i1=max(1,i0-m)
  i2=min(NFFT,i0+m)
  ccf=0.
  do lag=0,LAGMAX                     !Search over range of DT
     do j=1,22                        !Test for Q65 sync
        k=isync(j) + lag
        ccf(lag)=ccf(lag) + sum(ss(k,i1:i2)) + sum(ss(k+1,i1:i2)) &
             + sum(ss(k+2,i1:i2))
! Q: Should we use weighted sums, perhaps a Lorentzian peak?
     enddo
  enddo
  ccfmax=maxval(ccf)
  ipk=maxloc(ccf)
  lagbest=ipk(1)-1
  xdt=lagbest*tstep - 1.0

  xsum=0.
  sq=0.
  nsum=0
  do i=0,lagmax                       !Compute ave and rms of "blue curve"
     if(abs(i-lagbest).gt.2) then
        xsum=xsum+ccf(i)
        sq=sq+ccf(i)**2
        nsum=nsum+1
     endif
  enddo
  ave=xsum/nsum
  rms=sqrt(sq/nsum - ave*ave)
  snr=(ccfmax-ave)/rms
  sync_ok=snr.ge.5.0                  !Require snr > 5.0 for sync detection

  return
end subroutine q65_sync
