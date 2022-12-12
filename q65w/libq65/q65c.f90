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
  logical ldecoded,first
  character*12 mycall,hiscall
  character*6 mygrid,hisgrid
  character*20 datetime

  common/datcom2/dd(2,5760000),ss(322,NFFT),savg(NFFT),nparams0

!### REMEMBER that /npar/ is not updated until nparams=nparams0 is executed. ###
  common/npar/fcenter,nutc,idphi,mousedf,mousefqso,nagain,                &
       ndepth,ndiskdat,neme,newdat,nfa,nfb,nfcal,nfshift,                 &
       mcall3,nkeep,ntol,nxant,nrxlog,nfsample,nxpol,nmode,               &
       ndop00,nsave,max_drift,nhsym,mycall,mygrid,hiscall,hisgrid,        &
       datetime,junk1,junk2
!### REMEMBER that /npar/ is not updated until nparams=nparams0 is executed. ###

  common/early/nhsym1,nhsym2,ldecoded(32768)
  equivalence (nparams,fcenter)
  data first/.true./
  save first

  nparams=nparams0                     !Copy parameters into common/npar/
  if(itimer.ne.0) then
     call timer('decode0 ',101)
     call fini_timer
     return
  endif

  datetime(18:20)=':00'

!  if(first) call timer('q65w    ',0)
!  first=.false.

  npatience=1
  call timer('decode0 ',0)
  call decode0(dd,ss,savg)
  call timer('decode0 ',1)

  return
end subroutine q65c
