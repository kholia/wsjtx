subroutine q65wa(dd,ss,savg,newdat,nutc,fcenter,ntol,nfa,nfb,         &
     mousedf,mousefqso,nagain,nfshift,max_drift,nfcal,mycall,         &
     hiscall,hisgrid,nfsample,nmode,ndepth,datetime,ndop00,fselected)

!  Processes timf2 data received from Linrad to find and decode Q65 signals.

  use timer_module, only: timer

  type candidate
     real :: snr          !Relative S/N of sync detection
     real :: f            !Freq of sync tone, 0 to 96000 Hz
     real :: xdt          !DT of matching sync pattern, -1.0 to +4.0 s
  end type candidate

  parameter (NFFT=32768)             !Size of FFTs done in symspec()
  parameter (MAX_CANDIDATES=50)
  parameter (MAXMSG=1000)            !Size of decoded message list
  parameter (NSMAX=60*96000)
  complex cx(NSMAX/64)               !Data at 1378.125 samples/s
  real dd(2,NSMAX)                   !I/Q data from Linrad
  real ss(322,NFFT)                  !Symbol spectra
  real savg(NFFT)                    !Average spectrum
  real*8 fcenter                             !Center RF frequency, MHz
  character mycall*12,hiscall*12,hisgrid*6
  type(candidate) :: cand(MAX_CANDIDATES)
  character*60 result
  character*20 datetime
  common/decodes/ndecodes,ncand,nQDecoderDone,nWDecoderBusy,              &
       nWTransmitting,result(50)
  save

  tsec0=sec_midn()
  if(nagain.eq.1) ndepth=3            !Use full depth for click-to-decode
  nkhz_center=nint(1000.0*(fcenter-int(fcenter)))
  mfa=nfa-nkhz_center+48
  mfb=nfb-nkhz_center+48
  mode_q65=nmode/10
  nts_q65=2**(mode_q65-1)             !Q65 tone separation factor

  call timer('get_cand',0)
  call getcand2(ss,savg,nts_q65,cand,ncand) !Get a list of decoding candidates
  call timer('get_cand',1)

  nwrite_q65=0
  df=96000.0/NFFT                     !df = 96000/NFFT = 2.930 Hz
  if(nfsample.eq.95238) df=95238.1/NFFT
  ftol=0.010                          !Frequency tolerance (kHz)
  foffset=0.001*(1270 + nfcal)        !Offset from sync tone, plus CAL
  fqso=mousefqso + foffset - 0.5*(nfa+nfb) + nfshift !fqso at baseband (khz)
  nqd=0

  call timer('filbig  ',0)
  call filbig(dd,NSMAX,f0,newdat,nfsample,cx,n5) !Do the full-length FFT
  call timer('filbig  ',1)

  do icand=1,ncand                        !Attempt to decode each candidate
     f0=cand(icand)%f
!     if(f0-mfa.lt.0.0 .or. f0-mfb.gt.0.0) cycle
     print*,'cc',nagain,ntol,fselected,f0+nkhz_center-48.0
     if(nagain.eq.1 .and. abs(f0+nkhz_center-48.0 - fselected).gt.0.001*ntol) cycle
     nagain2=0
     print*,'dd',nagain,ntol,fselected,f0+nkhz_center-48.0
     freq=cand(icand)%f+nkhz_center-48.0-1.27046
     ikhz=nint(freq)
     idec=-1
     call timer('q65b    ',0)
     call q65b(nutc,nqd,fcenter,nfcal,nfsample,ikhz,mousedf,ntol,       &
          mycall,hiscall,hisgrid,mode_q65,f0,fqso,nkhz_center,newdat,   &
          nagain2,max_drift,ndepth,datetime,ndop00,idec)
     call timer('q65b    ',1)
     tsec=sec_midn() - tsec0
     if(tsec.gt.30.0) exit    !Don't start another decode attempt after t=30 s.
  enddo  ! icand

  return
end subroutine q65wa
