subroutine sfox_gen(idat,f0,fsample,syncwidth,cdat)

  use sfox_mod
!  include "sfox_params.f90"
  complex cdat(NMAX)                     !Generated complex waveform
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

! Sync waveform
  a1=f0 + syncwidth/2.0             !Frequency at midpoint of sync waveform
  a2=2.0*syncwidth/tsync            !Frequency drift rate
  x0=0.5*(nsync+1)
  s=2.0/nsync
  do i=1,nsync
     j=j+1
     if(i.eq.nsync/2+1) a2=-a2       !Reverse sign of drift at midpoint
     x=s*(i-x0)
     dphi=(a1 + x*a2) * (twopi/fsample)
     wstep=cmplx(cos(dphi),sin(dphi))
     w=w*wstep
     cdat(j)=w
  enddo

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
