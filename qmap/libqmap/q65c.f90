subroutine q65c(itimer)

  use timer_module, only: timer
  use timer_impl, only: fini_timer !, limtrace
  use, intrinsic :: iso_c_binding, only: C_NULL_CHAR
  use FFTW3
  use q65
  use q65_decode

  parameter (NFFT=32768)
  include 'njunk.f90'
  real*8 fcenter
  real*4 pdb(4)
  integer nparams0(NJUNK+3),nparams(NJUNK+3)
  logical first
  logical*1 bAlso30
  character*12 mycall,hiscall
  character*6 mygrid,hisgrid
  character*20 datetime

  common/datcom2/dd(2,5760000),ss(400,NFFT),savg(NFFT),nparams0

!### REMEMBER that /npar/ is not updated until nparams=nparams0 is executed. ###
  common/npar/fcenter,nutc,fselected,mousedf,mousefqso,nagain,            &
       ndepth,ndiskdat,ntx60,newdat,nn1,nn2,nfcal,nfshift,                 &
       ntx30a,ntx30b,ntol,n60,nCFOM,nfsample,nxpol,nmode,               &
       ndop00,nsave,nn3,nn4,nhsym,mycall,mygrid,hiscall,hisgrid,      &
       datetime,junk1,junk2,bAlso30
  equivalence (nparams,fcenter)
  data first/.true./
  save first

  nparams=nparams0                     !Copy parameters into common/npar/
  datetime(12:)='00       '

  npatience=1
  newdat=1                          !Always on ??

  if(ndiskdat.eq.1) then
     call chkstat(dd,nhsym,pdb)
     if((abs(pdb(1)-pdb(2)).gt.3.0 .and. pdb(1).gt.1.0) .or.  &
          pdb(1).lt.1.0) ntx30a=20                               !Tx 1st half
     if((abs(pdb(3)-pdb(4)).gt.3.0 .and. pdb(3).gt.1.0) .or.  &
          pdb(3).lt.1.0) ntx30b=20                               !Tx 2nd half
     if(pdb(4).lt.0.04) then
        ntx30a=0  !Older 56s files have no Tx
        ntx30b=0  !Older 56s files have no Tx
     endif

!     write(*,3001) nutc,nhsym,pdb,ntx30a,ntx30b
!3001 format(i4.4,i6,4f7.1,2i6)
  endif

  if(ntx30a.gt.5) then
     dd(1:2,1:30*96000)=0.
     ss(1:200,1:NFFT)=0.
     do i=1,NFFT
        savg(i)=sum(ss(201:400,i))
     enddo
  endif
  if(ntx30b.gt.5) then
     dd(1:2,30*96000+1:60*96000)=0.
     ss(201:400,1:NFFT)=0.
     do i=1,NFFT
        savg(i)=sum(ss(1:200,i))
     enddo
  endif

  call timer('decode0 ',0)
  call decode0(dd,ss,savg)
  call timer('decode0 ',1)

  return
end subroutine q65c

subroutine all_done

  use timer_module, only: timer
  use timer_impl, only: fini_timer

  call timer('decode0 ',101)
  call fini_timer
  print*,'All Done'

  return
end subroutine all_done
