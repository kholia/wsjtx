subroutine sfox_gen(idat,f0,fsample,syncwidth,cdat)

  use sfox_mod
!  include "sfox_params.f90"
  complex cdat(NMAX)                     !Generated complex waveform
  complex ctmp(NSYNC)
  complex w,wstep
  integer idat(NN)

  twopi=8.0*atan(1.0)
  tsync=NS*NSPS/fsample
  df=fsample/NSPS
  w=1.0
  j=0
  k=0
  i0=NQ/2
! First group of data symbols:
  do n=1,ND1
     k=k+1
     dphi=(f0 + (idat(k)-i0)*df)*(twopi/fsample)
     wstep=cmplx(cos(dphi),sin(dphi))
     do i=1,NSPS
        j=j+1
        w=w*wstep
        cdat(j)=w
     enddo
  enddo

! Calculate and insert sync waveform
  call sweep(f0,syncwidth,fsample,w,ctmp,nsync)
  cdat(j:j+nsync-1)=ctmp
  j=j+nsync

! Final group of data symbols:
  do n=1,ND2
     k=k+1
     dphi=(f0 + (idat(k)-i0)*df)*(twopi/fsample)
     wstep=cmplx(cos(dphi),sin(dphi))
     do i=1,NSPS
        j=j+1
        w=w*wstep
        cdat(j)=w
     enddo
  enddo

  return
end subroutine sfox_gen
