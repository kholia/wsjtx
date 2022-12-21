subroutine q65wa(dd,ss,savg,newdat,nutc,fcenter,ntol,nfa,nfb,         &
     mousedf,mousefqso,nagain,ndecdone,nfshift,max_drift,             &
     nfcal,mycall,hiscall,hisgrid,nhsym,nfsample,nmode,ndepth,        &
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
  call getcand2(savg,nts_q65,cand,ncand)
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

     write(71,3071) icand,cand(icand)%f,32.0+cand(icand)%f,   &
          cand(icand)%xdt,cand(icand)%snr,idec,ndecodes
3071 format(i2,4f10.3,2i5)

  enddo  ! icand
  ndecdone=2

  return
end subroutine q65wa

subroutine getcand2(savg0,nts_q65,cand,ncand)

  use wideband_sync
!  parameter(NFFT=32768)
  real savg0(NFFT),savg(NFFT)
  integer ipk1(1)
  type(candidate) :: cand(MAX_CANDIDATES)

  savg=savg0
  df=96000.0/NFFT
  bw=65*nts_q65*1.666666667
  nbw=bw/df + 1
  smin=70.0
  nguard=5

!  print*,'aaa',nts_q65,bw
  j=0
  sync(1:NFFT)%ccfmax=0.

  do i=1,NFFT-2*nbw
     if(savg(i).lt.smin) cycle
     spk=maxval(savg(i:i+nbw))
     ipk1=maxloc(savg(i:i+nbw))
     i0=ipk1(1) + i - 1
     fpk=0.001*i*df
     j=j+1
!     write(*,3020) j,fpk,spk
!3020 format(i3,f12.6,f8.1)
     cand(j)%f=fpk
     cand(j)%xdt=2.8
     cand(j)%snr=spk
     cand(j)%iflip=0

     sync(i0)%ccfmax=spk

     ia=min(i,i0-nguard)
     ib=i0+nbw+nguard
     savg(ia:ib)=0.
!     sync(ia:ib)%ccfmax=0.
     if(j.ge.30) exit
  enddo
  ncand=j

  do i=1,NFFT
     write(72,3072) i,0.001*i*df+32.0,savg0(i),savg(i),sync(i)%ccfmax
3072 format(i6,f15.6,3f15.3)
  enddo

  return
end subroutine getcand2
