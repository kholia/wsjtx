subroutine fftbig(dd,nmax)

! Filter and downsample complex data stored in array dd(2,nmax).  
! Output is downsampled from 96000 Hz to 1375.125 Hz.

  use timer_module, only: timer
  parameter (MAXFFT1=5376000,MAXFFT2=77175)
  real*4  dd(2,nmax)                         !Input data
  complex ca(MAXFFT1)                        !FFT of input
  complex c4a(MAXFFT2)                       !Output data
  real*8 df
  integer*8 plan1
  logical first
  include 'fftw3.f'
  common/cacb/ca
  equivalence (rfilt,cfilt)
  data first/.true./,npatience/1/
  save

  if(nmax.lt.0) go to 900

  nfft1=MAXFFT1
  if(first) then
     nflags=FFTW_ESTIMATE
     if(npatience.eq.1) nflags=FFTW_ESTIMATE_PATIENT
     if(npatience.eq.2) nflags=FFTW_MEASURE
     if(npatience.eq.3) nflags=FFTW_PATIENT
     if(npatience.eq.4) nflags=FFTW_EXHAUSTIVE
     
! Plan the big FFT just once
     call timer('FFTplan ',0)
     call sfftw_plan_dft_1d(plan1,nfft1,ca,ca,FFTW_BACKWARD,nflags)
     call timer('FFTplan ',1)
     df=96000.d0/nfft1
     first=.false.
  endif

  nz=min(nmax,nfft1)
  do i=1,nz
     ca(i)=cmplx(dd(1,i),dd(2,i))
  enddo

  if(nmax.lt.nfft1) then
     do i=nmax+1,nfft1
        ca(i)=0.
     enddo
  endif
  call timer('FFTbig  ',0)
  call sfftw_execute(plan1)
  call timer('FFTbig  ',1)
  go to 999

900 call sfftw_destroy_plan(plan1)

999 return
end subroutine fftbig
