subroutine q65_sync(ss,i0,nts_q65,sync_ok,snr,xdt)

! Test for presence of Q65 sync tone

  parameter (NFFT=32768)
  parameter (LAGMAX=33)
  real ss(373,NFFT)                !Symbol spectra
  real ccf(0:LAGMAX)               !The WSJT "blue curve", peak at DT
  logical sync_ok
  logical first
  integer isync0(22),isync(22),ipk(1)

! Q65 sync symbols
  data isync0/1,9,12,13,15,22,23,26,27,33,35,38,46,50,55,60,62,66,69,74,76,85/

  tstep=0.15                      !0.5*tsym_Q65-30x, 0.25*tsys_Q65-60x
  do i=1,22                                !Expand sync stride for Q65-60x
     isync(i)=4*(isync0(i)-1) + 1
  enddo

  m=nts_q65/2
  i1=max(1,i0-m)
  i2=min(NFFT,i0+m)
  ccf=0.
  do lag=0,LAGMAX                     !Search over range of DT
     do j=1,22                        !Test for Q65 sync
        k=isync(j) + lag 
        ccf(lag)=ccf(lag) + sum(ss(k,i1:i2)) + sum(ss(k+1,i1:i2)) &
             + sum(ss(k+2,i1:i2)) + sum(ss(k+3,i1:i2))
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
