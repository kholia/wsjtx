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
