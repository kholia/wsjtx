subroutine ftninit

  use timer_impl, only: init_timer !,fini_timer, limtrace
  use, intrinsic :: iso_c_binding, only: C_NULL_CHAR
  use FFTW3
!  character*(*) appd
  character*1 appd
  character addpfx*8
  character wisfile*256
  common/pfxcom/addpfx

  lu=8
  call init_timer('./timer.out')
  
  appd='.'
  addpfx='    '
  open(12,file=appd//'/all_qmap.txt',status='unknown',position='append')
  open(17,file=appd//'/red.dat',status='unknown')
  open(19,file=appd//'/livecq.txt',status='unknown')
  open(71,file=appd//'/fort.71',status='unknown')
  open(72,file=appd//'/fort.72',status='unknown')
  open(73,file=appd//'/fort.73',status='unknown')

! Import FFTW wisdom, if available:
  iret=fftwf_init_threads()            !Initialize FFTW threading 
! Default to 1 thread, but use nthreads for the big ones
  call fftwf_plan_with_nthreads(1)
! Import FFTW wisdom, if available
  wisfile=trim(appd)//'/m65_wisdom.dat'// C_NULL_CHAR
  iret=fftwf_import_wisdom_from_filename(wisfile)

  return
end subroutine ftninit
