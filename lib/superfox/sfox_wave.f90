subroutine sfox_wave(fname)
  
  parameter (NWAVE=(160+2)*134400*4) !Max WSJT-X waveform (FST4-1800 at 48kHz)
  parameter (NN=151,NSPS=1024)
  character*(*) fname
  integer itone(151)
  real*8 dt,twopi,f0,baud,phi,dphi

  common/foxcom/wave(NWAVE)
  
  open(25,file=trim(fname),status='unknown')
  read(25,'(20i4)') itone
  close(25)

! Generate the SuperFox waveform.

  dt=1.d0/48000.d0
  twopi=8.d0*atan(1.d0)
  f0=750.0d0
  phi=0.d0
  baud=12000.d0/NSPS
  k=0
  do j=1,NN
     f=f0 + baud*itone(j)
     dphi=twopi*f*dt
     do ii=1,NSPS
        k=k+1
        phi=phi+dphi
        xphi=phi
        wave(k)=wave(k)+sin(xphi)
     enddo
  enddo

  return  
end subroutine sfox_wave
