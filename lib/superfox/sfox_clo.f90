subroutine sfox_clo(fsample,syncwidth,clo)

! Generate complex LO for the SuperFox sync signal
  
  use sfox_mod
  complex clo(NSYNC)                      !Complex Local Oscillator
  complex w

  w=1.0
  call sweep(1500.0,syncwidth,fsample,w,clo,nsync)
  clo=conjg(clo)

  return
end subroutine sfox_clo

subroutine sweep(f0,syncwidth,fsample,w,c,iz)

  complex c(iz)
  complex w,wstep

  twopi=8.0*atan(1.0)
  ttot=iz/fsample
  a0=f0 + syncwidth/2.0             !Frequency at midpoint of LO waveform
  a2=2.0*syncwidth/ttot             !Frequency drift rate
  x0=0.5*(iz+1)
  s=2.0/iz
  do i=1,iz
     if(i.eq.iz/2+1) a2=-a2         !Reverse sign of drift at midpoint
     x=s*(i-x0)
     dphi=(a0 + x*a2)*(twopi/fsample)
     j=(i-1)/(4*1024)
     a0a=a0 + (j-2.5)*200.0
     a2a=a2*(1.0 + (j-2.5)/10.0)
     dphi=(a0a + x*a2a)*(twopi/fsample)
     wstep=cmplx(cos(dphi),sin(dphi))
     w=w*wstep
     c(i)=w
  enddo

  return
end subroutine sweep
