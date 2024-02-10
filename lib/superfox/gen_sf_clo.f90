subroutine gen_sf_clo(fsample,syncwidth,clo)

! Generate complex LO for the SuperFox sync signal
  
  use sfox_mod
  complex clo(NMAX)                      !Complex Local Oscillator
  complex w,wstep

  twopi=8.0*atan(1.0)
  tsync=NS*NSPS/fsample
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

  return
end subroutine gen_sf_clo
