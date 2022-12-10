subroutine m65c

  use timer_module, only: timer
  use timer_impl, only: init_timer !, limtrace
  use, intrinsic :: iso_c_binding, only: C_NULL_CHAR
  use FFTW3
  use q65
  use q65_decode

  parameter (NFFT=32768)
  include 'njunk.f90'
!  real*4 dd(4,5760000),ss(4,322,32768),savg(4,32768)
  real*8 fcenter
  integer nparams0(NJUNK+3),nparams(NJUNK+3)
  logical ldecoded,first
  character*12 mycall,hiscall
  character*6 mygrid,hisgrid
  character*20 datetime
  character*80 cwd

  common/datcom2/dd(4,5760000),ss(4,322,NFFT),savg(4,NFFT),nparams0
  common/npar/fcenter,nutc,idphi,mousedf,mousefqso,nagain,                &
       ndepth,ndiskdat,neme,newdat,nfa,nfb,nfcal,nfshift,                 &
       mcall3,nkeep,ntol,nxant,nrxlog,nfsample,nxpol,nmode,               &
       ndop00,nsave,max_drift,nhsym,mycall,mygrid,hiscall,hisgrid,        &
       datetime,junk1,junk2
  common/early/nhsym1,nhsym2,ldecoded(32768)
  equivalence (nparams,fcenter)
  data first/.true./
  save first,cwd

  lq65w=.true.
  lq65w2=.true.
  nparams=nparams0                     !Copy parameters into common/npar/
  datetime(18:20)=':00'

!  if(first) then
!     call getcwd(cwd)
!     call ftninit(trim(cwd))
!     call init_timer (trim(cwd)//'/timer.out')
!     first=.false.
!  endif

  npatience=1
  nstandalone=0
  if(sum(nparams).ne.0) call decode0(dd,ss,savg,nstandalone)

  return
end subroutine m65c
