subroutine getcand2(ss,savg0,nts_q65,cand,ncand)

!  use wideband_sync

  type candidate
     real :: snr          !Relative S/N of sync detection
     real :: f            !Freq of sync tone, 0 to 96000 Hz
     real :: xdt          !DT of matching sync pattern, -1.0 to +4.0 s
  end type candidate

  parameter (NFFT=32768)
  parameter (MAX_CANDIDATES=50)
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
     cand(j)%f=fpk
     cand(j)%xdt=xdt
     cand(j)%snr=snr_sync
     ia=min(i,i0-nguard)
     ib=i0+nbw+nguard
     savg(ia:ib)=0.
     if(j.ge.30) exit
  enddo
  ncand=j

  return
end subroutine getcand2
