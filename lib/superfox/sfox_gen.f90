subroutine sfox_gen(idat,f0,fsample,isync,cdat)

  use sfox_mod
  complex cdat(NMAX)                     !Generated complex waveform
  complex w,wstep
  integer idat(NN)
  integer isync(50)

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

! Insert sync symbols
  do n=1,NS
     dphi=(f0 + (isync(n)-i0)*df)*(twopi/fsample)
     wstep=cmplx(cos(dphi),sin(dphi))
     do i=1,NSPS
        j=j+1
        w=w*wstep
        cdat(j)=w
     enddo
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
