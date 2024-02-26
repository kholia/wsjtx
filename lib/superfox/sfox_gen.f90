subroutine sfox_gen(idat,f0,fsample,isync,itone,cdat)

  use sfox_mod
  complex cdat(NMAX)                     !Generated complex waveform
  complex w,wstep
  integer idat(NN)
  integer isync(44)
  integer itone(171)

  twopi=8.0*atan(1.0)

! Create the itone sequence: data symbols and interspersed sync symbols
  j=1
  k=0
  do i=1,NDS
     if(j.le.NS .and. i.eq.isync(j)) then
        if(j.lt.NS) j=j+1       !Index for next sync symbol
        itone(i)=0              !Insert sync symbol at tone 0
     else
        k=k+1
        itone(i)=idat(k) + 1    !Symbol value 0 is transmitted at tone 1, etc.
     endif
  enddo
  
  df=fsample/NSPS
  w=1.0
  j=0
  i0=NQ/2
! Generate the waveform
  do k=1,NDS                                       !Loop over all symbols
     dphi=(f0 + (itone(k)-i0)*df)*(twopi/fsample)
     wstep=cmplx(cos(dphi),sin(dphi))
     do i=1,NSPS                                   !NSPS samples per symbol
        j=j+1
        w=w*wstep
        cdat(j)=w
     enddo
  enddo

  return
end subroutine sfox_gen
