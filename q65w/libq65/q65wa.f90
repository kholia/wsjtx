subroutine q65wa(dd,ss,savg,newdat,nutc,fcenter,ntol,nfa,nfb,        &
     mousedf,mousefqso,nagain,ndecdone,nfshift,max_drift,             &
     nfcal,nsum,nxant,mycall,mygrid,                    &
     hiscall,hisgrid,nhsym,nfsample,                &
     ndiskdat,nxpol,nmode,ndop00)

!  Processes timf2 data from Linrad to find and decode JT65 and Q65 signals.

  use wideband_sync
  use timer_module, only: timer

  parameter (MAXMSG=1000)            !Size of decoded message list
  parameter (NSMAX=60*96000)
  complex cx(NSMAX/64), cy(NSMAX/64)   !Data at 1378.125 samples/s
  real dd(4,NSMAX)
  real*4 ss(322,NFFT),savg(NFFT)
  real*8 fcenter
  character*3 shmsg0(4)
  character mycall*12,hiscall*12,mygrid*6,hisgrid*6,cm*1
  logical xpol,bq65
  logical candec(MAX_CANDIDATES)
  logical ldecoded
  character blank*22
  real short(3,NFFT)                 !SNR dt ipol for potential shorthands
  type(candidate) :: cand(MAX_CANDIDATES)
  character*60 result
  common/decodes/ndecodes,ncand,result(50)
  common/testcom/ifreq
  common/early/nhsym1,nhsym2,ldecoded(32768)

  data blank/'                      '/,cm/'#'/
  data shmsg0/'ATT','RO ','RRR','73 '/
  data nfile/0/,nutc0/-999/,nid/0/,ip000/1/,ip001/1/,mousefqso0/-999/
  save

  rewind 12

! Clean start for Q65 at early decode
  if(nhsym.eq.nhsym1 .or. nagain.ne.0) ldecoded=.false.
  if(ndiskdat.eq.1) ldecoded=.false.

  nkhz_center=nint(1000.0*(fcenter-int(fcenter)))
  mfa=nfa-nkhz_center+48
  mfb=nfb-nkhz_center+48
  mode65=mod(nmode,10)
  if(mode65.eq.3) mode65=4
  mode_q65=nmode/10
  nts_jt65=mode65                     !JT65 tone separation factor
  nts_q65=2**(mode_q65-1)             !Q65 tone separation factor
  xpol=(nxpol.ne.0)
  
! No second decode for JT65?
  if(nhsym.eq.nhsym2 .and. nagain.eq.0 .and.ndiskdat.eq.0) mode65=0

  if(nagain.eq.0) then
     call timer('get_cand',0)
     call get_candidates(ss,savg,nhsym,mfa,mfb,nts_jt65,nts_q65,cand,ncand)
     call timer('get_cand',1)
     candec=.false.
  endif
!###
!  do k=1,ncand
!     freq=cand(k)%f+nkhz_center-48.0
!     ipk=cand(k)%indx
!     write(71,3071) k,db(cand(k)%snr),freq,cand(k)%xdt,    &
!          cand(k)%ipol,cand(k)%iflip,ipk,ldecoded(ipk)
!3071 format(i3,f8.2,f10.3,f8.2,2i3,i6,L4)
!  enddo
!###

  nwrite_q65=0
  bq65=mode_q65.gt.0

  mousefqso0=mousefqso
  nsum=0

  df=96000.0/NFFT                     !df = 96000/NFFT = 2.930 Hz
  if(nfsample.eq.95238) df=95238.1/NFFT
  ftol=0.010                          !Frequency tolerance (kHz)
  foffset=0.001*(1270 + nfcal)              !Offset from sync tone, plus CAL
  fqso=mousefqso + foffset - 0.5*(nfa+nfb) + nfshift !fqso at baseband (khz)
  iloop=0

  if(nutc.ne.nutc0) nfile=nfile+1
  nutc0=nutc

  nqd=0
  fa=-1000*0.5*(nfb-nfa) + 1000*nfshift
  fb= 1000*0.5*(nfb-nfa) + 1000*nfshift
  ia=nint(fa/df) + 16385
  ib=nint(fb/df) + 16385
  ia=max(51,ia)
  ib=min(32768-51,ib)
  if(ndiskdat.eq.1 .and. mode65.eq.0) ib=ia

  km=0
  nkm=1
  nz=n/8
  freq0=-999.
  sync10=-999.
  fshort0=-999.
  syncshort0=-999.
  ntry=0
  short=0.                                 !Zero the whole short array
  jpz=1

  call timer('filbig  ',0)
  call filbig(dd,NSMAX,f0,newdat,nfsample,xpol,cx,cy,n5)
  call timer('filbig  ',1)

! Do the wideband Q65 decode        
  do icand=1,ncand
     if(cand(icand)%iflip.ne.0) cycle    !Do only Q65 candidates here
     if(candec(icand)) cycle             !Skip if already decoded
     freq=cand(icand)%f+nkhz_center-48.0-1.27046
     ikhz=nint(freq)
     f0=cand(icand)%f
     call timer('q65b    ',0)
     call q65b(nutc,nqd,nxant,fcenter,nfcal,nfsample,ikhz,mousedf,ntol, &
          xpol,mycall,mygrid,hiscall,hisgrid,mode_q65,f0,fqso,newdat,   &
          nagain,max_drift,nhsym,ndop00,idec)
     call timer('q65b    ',1)
     if(idec.ge.0) candec(icand)=.true.
  enddo  ! icand
  call sec0(1,tsec0)
  ndecdone=2
  call flush(12)

  return
end subroutine q65wa
