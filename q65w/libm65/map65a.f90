subroutine map65a(dd,ss,savg,newdat,nutc,fcenter,ntol,idphi,nfa,nfb,        &
     mousedf,mousefqso,nagain,ndecdone,nfshift,ndphi,max_drift,             &
     nfcal,nkeep,mcall3b,nsum,nsave,nxant,mycall,mygrid,                    &
     neme,ndepth,nstandalone,hiscall,hisgrid,nhsym,nfsample,                &
     ndiskdat,nxpol,nmode,ndop00)

!  Processes timf2 data from Linrad to find and decode JT65 and Q65 signals.

  use wideband_sync
  use timer_module, only: timer

  parameter (MAXMSG=1000)            !Size of decoded message list
  parameter (NSMAX=60*96000)
  complex cx(NSMAX/64), cy(NSMAX/64)   !Data at 1378.125 samples/s
  real dd(4,NSMAX)
  real*4 ss(4,322,NFFT),savg(NFFT)
  real tavg(-50:50)                  !Temp for finding local base level
  real base(4)                       !Local basel level at 4 pol'ns
  real sig(MAXMSG,30)                !Parameters of detected signals
  real a(5)
  real*8 fcenter
  character*22 msg(MAXMSG)
  character*3 shmsg0(4)
  character mycall*12,hiscall*12,mygrid*6,hisgrid*6,cp*1,cm*1
  integer indx(MAXMSG),nsiz(MAXMSG)
  logical done(MAXMSG)
  logical xpol,bq65,q65b_called
  logical candec(MAX_CANDIDATES)
  logical ldecoded
  character decoded*22,blank*22,cmode*2
  real short(3,NFFT)                 !SNR dt ipol for potential shorthands
  real qphi(12)
  type(candidate) :: cand(MAX_CANDIDATES)
  character*60 result
  common/decodes/ndecodes,ncand,result(50)
  common/c3com/ mcall3a
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
     call get_candidates(ss,savg,xpol,nhsym,mfa,mfb,nts_jt65,nts_q65,cand,ncand)
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

  mcall3a=mcall3b
  mousefqso0=mousefqso
  if(.not.xpol) ndphi=0
  nsum=0

!### Should use AppDir! ###
!  open(23,file='CALL3.TXT',status='unknown')

  df=96000.0/NFFT                     !df = 96000/NFFT = 2.930 Hz
  if(nfsample.eq.95238) df=95238.1/NFFT
  ftol=0.010                          !Frequency tolerance (kHz)
  dphi=idphi/57.2957795
  foffset=0.001*(1270 + nfcal)              !Offset from sync tone, plus CAL
  fqso=mousefqso + foffset - 0.5*(nfa+nfb) + nfshift !fqso at baseband (khz)
  iloop=0

2  if(ndphi.eq.1) dphi=30*iloop/57.2957795

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

     print*,'AAA',mode65
     call timer('filbig  ',0)
     call filbig(dd,NSMAX,f0,newdat,nfsample,xpol,cx,cy,n5)
     call timer('filbig  ',1)

     if(nqd.eq.0 .and. bq65) then
! Do the wideband Q65 decode        
        do icand=1,ncand
           if(cand(icand)%iflip.ne.0) cycle    !Do only Q65 candidates here
           if(candec(icand)) cycle             !Skip if already decoded
           freq=cand(icand)%f+nkhz_center-48.0-1.27046
!###! If here at nqd=1, do only candidates at mousefqso +/- ntol
!###           if(nqd.eq.1 .and. abs(freq-mousefqso).gt.0.001*ntol) cycle
           ikhz=nint(freq)
           f0=cand(icand)%f
           call timer('q65b    ',0)
           call q65b(nutc,nqd,nxant,fcenter,nfcal,nfsample,ikhz,mousedf,ntol, &
                xpol,mycall,mygrid,hiscall,hisgrid,mode_q65,f0,fqso,newdat,   &
                nagain,max_drift,nhsym,ndop00,idec)
           call timer('q65b    ',1)
           if(idec.ge.0) candec(icand)=.true.
        enddo  ! icand
     endif
     call sec0(1,tsec0)

!  Trim the list and produce a sorted index and sizes of groups.
!  (Should trimlist remove all but best SNR for given UTC and message content?)
700  call trimlist(sig,km,ftol,indx,nsiz,nz)
  done(1:km)=.false.
  j=0
  ilatest=-1
  do n=1,nz
     ifile0=0
     do m=1,nsiz(n)
        i=indx(j+m)
        ifile=sig(i,1)
        if(ifile.gt.ifile0 .and.msg(i).ne.blank) then
           ilatest=i
           ifile0=ifile
        endif
     enddo
     i=ilatest

     if(i.ge.1) then
        if(.not.done(i)) then
           done(i)=.true.
           nutc=sig(i,2)
           freq=sig(i,3)
           sync1=sig(i,4)
           dt=sig(i,5)
           npol=nint(57.2957795*sig(i,6))
           flip=sig(i,7)
           sync2=sig(i,8)
           nkv=sig(i,9)
           nqual=min(sig(i,10),10.0)
!                  rms0=sig(i,11)
           do k=1,5
              a(k)=sig(i,12+k)
           enddo
           nhist=sig(i,18)
           decoded=msg(i)
           
           if(flip.lt.0.0) then
              do i=22,1,-1
                 if(decoded(i:i).ne.' ') go to 10
              enddo
              stop 'Error in message format'
10            if(i.le.18) decoded(i+2:i+4)='OOO'
           endif
           mhz=fcenter                             !... +fadd ???
           nkHz=nint(freq-foffset)-nfshift
           f0=mhz+0.001*nkHz
           ndf=nint(1000.0*(freq-foffset-(nkHz+nfshift)))
           ndf0=nint(a(1))
           ndf1=nint(a(2))
           ndf2=nint(a(3))
           nsync1=sync1

           s2db=10.0*log10(sync2) - 40             !### empirical ###
           nsync2=nint(s2db)
           if(decoded(1:4).eq.'RO  ' .or. decoded(1:4).eq.'RRR  ' .or.  &
                decoded(1:4).eq.'73  ') then
              nsync2=nint(1.33*s2db + 2.0)
           endif

           if(nxant.ne.0) then
              npol=npol-45
              if(npol.lt.0) npol=npol+180
           endif

           cmode='#A'
           if(mode65.eq.2) cmode='#B'
           if(mode65.eq.4) cmode='#C'
!           write(26,1014) f0,ndf,ndf0,ndf1,ndf2,dt,npol,nsync1,       &
!                nsync2,nutc,decoded,cp,cmode
!1014       format(f8.3,i5,3i3,f5.1,i4,i3,i4,i5.4,4x,a22,2x,a1,3x,a2)
           ndecodes=ndecodes+1
!           write(21,1100) f0,ndf,dt,npol,nsync2,nutc,decoded,cp,          &
!                cmode(1:1),cmode(2:2)
!1100       format(f8.3,i5,f5.1,2i4,i5.4,2x,a22,2x,a1,3x,a1,1x,a1)
        endif

     endif
     j=j+nsiz(n)
  enddo  !i=1,km

!  write(26,1015) nutc
!1015 format(37x,i6.4,' ')
!  call flush(21)
!  call flush(26)
!  call display(nkeep,ftol)
  ndecdone=2

900 continue
!  close(23)
  call flush(12)
  ndphi=0
  mcall3b=mcall3a

  return
end subroutine map65a
