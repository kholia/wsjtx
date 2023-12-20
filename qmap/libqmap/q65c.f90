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
  integer nparams0(NJUNK+3),nparams(NJUNK+3)
  logical first
  logical*1 bAlso30,bSkip
  character*12 mycall,hiscall
  character*6 mygrid,hisgrid
  character*20 datetime

  common/datcom2/dd(2,5760000),ss(400,NFFT),savg(NFFT),nparams0

!### REMEMBER that /npar/ is not updated until nparams=nparams0 is executed. ###
  common/npar/fcenter,nutc,fselected,mousedf,mousefqso,nagain,            &
       ndepth,ndiskdat,neme,newdat,nn1,nn2,nfcal,nfshift,                 &
       mcall3,nkeep,ntol,nxant,nCFOM,nfsample,nxpol,nmode,               &
       ndop00,nsave,nn3,nn4,max_nhsym,mycall,mygrid,hiscall,hisgrid,      &
       datetime,junk1,junk2,bAlso30
  equivalence (nparams,fcenter)
  data first/.true./
  save first

  nparams=nparams0                     !Copy parameters into common/npar/
  datetime(12:)='00       '

  if(itimer.ne.0) then
     call timer('decode0 ',101)
     call fini_timer
     return
  endif

  npatience=1
  newdat=1                          !Always on ??

  call chkstat(dd,max_nhsym,bSkip)
  if(bSkip) return

  call timer('decode0 ',0)
  call decode0(dd,ss,savg)
  call timer('decode0 ',1)

  return
end subroutine q65c
