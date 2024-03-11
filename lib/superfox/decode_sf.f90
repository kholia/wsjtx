subroutine decode_sf(iwave)

  use sfox_mod
  integer*2 iwave(NMAX)
  integer msg1(0:47)
  integer, allocatable :: rxdat(:)
  integer, allocatable :: rxprob(:)
  integer, allocatable :: rxdat2(:)
  integer, allocatable :: rxprob2(:)
  integer, allocatable :: correct(:)
  real a(3)
  real, allocatable :: s3(:,:)           !Symbol spectra: will be s3(NQ,NN)
  complex crcvd(NMAX)
  integer isync(24)                      !Symbol numbers for sync tones
  data isync/  1, 2, 4, 7,11,16,22,29,37,39,  & 
              42,43,45,48,52,57,63,70,78,80,  &
              83,84,86,89/

! Temporary, for initial tests:
  data msg1/   5, 126,  55,  29,   5, 127,  86, 117,   6,   0,  &
             118,  77,   6,   2,  22,  37,   6,   3,  53, 125,  &
               1,  27, 124, 110,  54,  12,   9,  43,  43,  64,  &
              96,  94,  85,  92,   6,   7,  21,   5, 104,  48,  &
              67,  37, 110,  67,   4, 106,  26,  64/
  
  mm0=7                             !Symbol size (bits)
  nn0=127                           !Number of information + parity symbols
  kk0=48                            !Number of information symbols
  fspread=0.0
  delay=0.0
  fsample=12000.0                   !Sample rate (Hz)
  ns0=24                            !Number of sync symbols
  call sfox_init(mm0,nn0,kk0,'no',fspread,delay,fsample,ns0)

! Allocate storage for arrays that depend on code parameters.
  allocate(s3(0:NQ-1,0:NN-1))
  allocate(rxdat(0:NN-1))
  allocate(rxprob(0:NN-1))
  allocate(rxdat2(0:NN-1))
  allocate(rxprob2(0:NN-1))
  allocate(correct(0:NN-1))

  call rs_init_sf(MM,NQ,NN,KK,NFZ)          !Initialize the Karn codec

  call sfox_ana(iwave,NMAX,crcvd,NMAX)

  call sfox_sync(iwave,fsample,isync,f,t,fwidth) !Find freq, DT, width

  a=0.
  a(1)=1500.0-f - baud                !Shift frequencies down by one bin
  call twkfreq(crcvd,crcvd,NMAX,fsample,a)
  f=1500.0
  call sfox_demod(crcvd,f,t,isync,s3)            !Get s3(0:NQ-1,0:127)
  call sfox_prob(s3,rxdat,rxprob,rxdat2,rxprob2)

  do i=0,KK-1
     write(60,3060) i,msg1(i),rxdat(i),rxprob(i),rxdat2(i),rxprob2(i)
3060 format(6i8)
  enddo

  ntrials=1000
  call ftrsd3(s3,rxdat,rxprob,rxdat2,rxprob2,ntrials,  &
       correct,param,ntry)
  if(ntry.lt.ntrials) then
     print*,'A',ntry,count(rxdat(0:KK-1).ne.msg1),count(correct(0:KK-1).ne.msg1)
     call sfox_unpack(correct(0:KK-1))
  endif

  return
end subroutine decode_sf
