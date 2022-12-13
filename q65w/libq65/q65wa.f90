subroutine q65wa(dd,ss,savg,newdat,nutc,fcenter,ntol,nfa,nfb,         &
     mousedf,mousefqso,nagain,ndecdone,nfshift,max_drift,             &
     nfcal,mycall,hiscall,hisgrid,nhsym,nfsample,nmode,ndop00)

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
  common/decodes/ndecodes,ncand,result(50)
  common/testcom/ifreq
  save

!  write(*,3001) newdat,ntol,nfa,nfb,mousedf,mousefqso,nagain,nfshift,max_drift
!3001 format(9i6)

  nagain=0              !### TEMPORARY ? ###
  rewind 12

  nkhz_center=nint(1000.0*(fcenter-int(fcenter)))
  mfa=nfa-nkhz_center+48
  mfb=nfb-nkhz_center+48
  mode_q65=nmode/10
  nts_q65=2**(mode_q65-1)             !Q65 tone separation factor

  call timer('get_cand',0)
  call get_candidates(ss,savg,nhsym,mfa,mfb,nts_jt65,nts_q65,cand,ncand)
  call timer('get_cand',1)
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

     call timer('q65b    ',0)
     call q65b(nutc,nqd,fcenter,nfcal,nfsample,ikhz,mousedf,ntol, &
          mycall,hiscall,hisgrid,mode_q65,f0,fqso,newdat,   &
          nagain,max_drift,ndop00,idec)
     call timer('q65b    ',1)
     if(idec.ge.0) candec(icand)=.true.
  enddo  ! icand
  call sec0(1,tsec0)
  ndecdone=2
  call flush(12)

  return
end subroutine q65wa
