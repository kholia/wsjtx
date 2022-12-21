subroutine q65wa(dd,ss,savg,newdat,nutc,fcenter,ntol,nfa,nfb,         &
     mousedf,mousefqso,nagain,ndecdone,nfshift,max_drift,             &
     nfcal,mycall,hiscall,hisgrid,nfsample,nmode,ndepth,        &
     datetime,ndop00)

!  Processes timf2 data from Linrad to find and decode JT65 and Q65 signals.

  use wideband_sync
  use timer_module, only: timer

  parameter (MAXMSG=1000)            !Size of decoded message list
  parameter (NSMAX=60*96000)
  complex cx(NSMAX/64)               !Data at 1378.125 samples/s
  real dd(2,NSMAX)
  real*4 ss(322,NFFT),savg(NFFT)
  real*8 fcenter
  character mycall*12,hiscall*12,hisgrid*6
  logical bq65
  logical candec(MAX_CANDIDATES)
  type(candidate) :: cand(MAX_CANDIDATES)
  character*60 result
  character*20 datetime
  common/decodes/ndecodes,ncand,nQDecoderDone,nWDecoderBusy,              &
       nWTransmitting,result(50)
  common/testcom/ifreq
  save

  if(nagain.eq.1) ndepth=3

  nkhz_center=nint(1000.0*(fcenter-int(fcenter)))
  mfa=nfa-nkhz_center+48
  mfb=nfb-nkhz_center+48
  mode_q65=nmode/10
  nts_q65=2**(mode_q65-1)             !Q65 tone separation factor

  call timer('get_cand',0)
!  call get_candidates(ss,savg,nhsym,mfa,mfb,nts_jt65,nts_q65,cand,ncand)
  call getcand2(ss,savg,nts_q65,cand,ncand)
  call timer('get_cand',1)

!  do i=1,ncand
!     write(71,3071) i,cand(i)%f,cand(i)%xdt,cand(i)%snr
!3071 format(i2,3f10.3)
!  enddo

  candec=.false.
  nwrite_q65=0
  bq65=mode_q65.gt.0
  df=96000.0/NFFT                     !df = 96000/NFFT = 2.930 Hz
  if(nfsample.eq.95238) df=95238.1/NFFT
  ftol=0.010                          !Frequency tolerance (kHz)
  foffset=0.001*(1270 + nfcal)              !Offset from sync tone, plus CAL
  fqso=mousefqso + foffset - 0.5*(nfa+nfb) + nfshift !fqso at baseband (khz)
  iloop=0
  nqd=0

  call timer('filbig  ',0)
  call filbig(dd,NSMAX,f0,newdat,nfsample,cx,n5)
  call timer('filbig  ',1)

! Do the wideband Q65 decode
  do icand=1,ncand
     f0=cand(icand)%f
     if(cand(icand)%iflip.ne.0) cycle    !Do only Q65 candidates here
     if(candec(icand)) cycle             !Skip if already decoded
     freq=cand(icand)%f+nkhz_center-48.0-1.27046
     ikhz=nint(freq)
     idec=-1

!     print*,'AAA',icand,nutc,nqd,fcenter,nfcal,nfsample,ikhz,mousedf,ntol, &
!          mycall,hiscall,hisgrid,mode_q65,f0,fqso,newdat,   &
!          nagain,max_drift,ndop00
     call timer('q65b    ',0)
     call q65b(nutc,nqd,fcenter,nfcal,nfsample,ikhz,mousedf,ntol, &
          mycall,hiscall,hisgrid,mode_q65,f0,fqso,newdat,   &
          nagain,max_drift,ndepth,datetime,ndop00,idec)
     call timer('q65b    ',1)
     if(idec.ge.0) candec(icand)=.true.

!     write(71,3071) icand,cand(icand)%f,32.0+cand(icand)%f,   &
!          cand(icand)%xdt,cand(icand)%snr,idec,ndecodes
!3071 format(i2,4f10.3,2i5)

  enddo  ! icand
  ndecdone=2

  return
end subroutine q65wa

subroutine getcand2(ss,savg0,nts_q65,cand,ncand)

  use wideband_sync
!  parameter(NFFT=32768)
  real ss(322,NFFT)
  real savg0(NFFT),savg(NFFT)
  integer ipk1(1)
  logical sync_ok
  type(candidate) :: cand(MAX_CANDIDATES)
  data nseg/16/,npct/40/

  savg=savg0
  nlen=NFFT/nseg
  do iseg=1,nseg
     ja=(iseg-1)*nlen + 1
     jb=ja + nlen - 1
     call pctile(savg(ja),nlen,npct,base)
     savg(ja:jb)=savg(ja:jb)/(1.015*base)
     savg0(ja:jb)=savg0(ja:jb)/(1.015*base)
  enddo

  df=96000.0/NFFT
  bw=65*nts_q65*1.666666667
  nbw=bw/df + 1
  nb0=2*nts_q65
  smin=1.4
  nguard=5

  j=0
  sync(1:NFFT)%ccfmax=0.

  do i=1,NFFT-nbw-nguard
     if(savg(i).lt.smin) cycle
     spk=maxval(savg(i:i+nb0))
     ipk1=maxloc(savg(i:i+nb0))
     i0=ipk1(1) + i - 1
     fpk=0.001*i0*df
! Check to see if sync tone is present.
     call q65_sync(ss,i0,nts_q65,sync_ok,snr_sync,xdt)
     if(.not.sync_ok) cycle
     j=j+1
!     write(73,3073) j,fpk+32.0-2.270,snr_sync,xdt
!3073 format(i3,3f10.3)
     cand(j)%f=fpk
     cand(j)%xdt=xdt
     cand(j)%snr=snr_sync
     cand(j)%iflip=0
     sync(i0)%ccfmax=snr_sync
     ia=min(i,i0-nguard)
     ib=i0+nbw+nguard
     savg(ia:ib)=0.
     if(j.ge.30) exit
  enddo
  ncand=j

!  do i=1,NFFT
!     write(72,3072) i,0.001*i*df+32.0,savg0(i),savg(i),sync(i)%ccfmax
!3072 format(i6,f15.6,3f15.3)
!  enddo

  return
end subroutine getcand2

subroutine q65_sync(ss,i0,nts_q65,sync_ok,snr,xdt)

  parameter (NFFT=32768)
  parameter (LAGMAX=33)
  real ss(322,NFFT)
  real ccf(0:LAGMAX)
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
  ccf=0.
  do lag=0,LAGMAX
     do j=1,22                        !Test for Q65 sync
        k=isync(j) + lag
!        ccf=ccf + ss(k,i0) + ss(k+1,i0) + ss(k+2,i0)
        ccf(lag)=ccf(lag) + sum(ss(k,i0-m:i0+m)) + sum(ss(k+1,i0-m:i0+m)) &
             + sum(ss(k+2,i0-m:i0+m))
     enddo
  enddo
  ccfmax=maxval(ccf)
  ipk=maxloc(ccf)
  lagbest=ipk(1)-1
  xdt=lagbest*tstep - 1.0

  xsum=0.
  sq=0.
  nsum=0
  do i=0,lagmax
     if(abs(i-lagbest).gt.2) then
        xsum=xsum+ccf(i)
        sq=sq+ccf(i)**2
        nsum=nsum+1
     endif
  enddo
  ave=xsum/nsum
  rms=sqrt(sq/nsum - ave*ave)
  snr=(ccfmax-ave)/rms
  sync_ok=snr.ge.5.0

  return
end subroutine q65_sync
