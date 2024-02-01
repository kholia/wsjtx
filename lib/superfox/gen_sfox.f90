subroutine gen_sfox(idat,f0,fsample,syncwidth,cdat,clo)

  include "sfox_params.f90"
  complex cdat(NMAX)                     !Generated complex waveform
  complex clo(NMAX)                      !Complex Local Oscillator
  complex w,wstep
  integer idat(ND)

  twopi=8.0*atan(1.0)
  tsync=NS*NSPS/fsample

! Generate complex LO for SuperFox sync
  w=1.0
  a0=1500.0+ syncwidth/2.0          !Frequency at midpoint of LO waveform
  a2=2.0*syncwidth/tsync            !Frequency drift rate
  x0=0.5*(nsync+1)
  s=2.0/nsync
  do i=1,nsync
     if(i.eq.nsync/2+1) a2=-a2       !Reverse sign of drift at midpoint
     x=s*(i-x0)
     dphi=(a0 + x*a2)*(twopi/fsample)
     wstep=cmplx(cos(dphi),sin(dphi))
     w=w*wstep
     clo(i)=conjg(w)
  enddo

! Generate complex SuperFox waveform

  df=fsample/NSPS
  j=0
  k=0
! First group of data symbols:
  do n=1,ND1
     k=k+1
     dphi=(f0 + (idat(k)-65)*df)*(twopi/fsample)
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
     dphi=(f0 + (idat(k)-65)*df)*(twopi/fsample)
     wstep=cmplx(cos(dphi),sin(dphi))
     do i=1,NSPS
        j=j+1
        w=w*wstep
        cdat(j)=w
     enddo
  enddo

  return
end subroutine gen_sfox
