module sfox_mod
  
  parameter (NMAX=15*12000)       !Samples in iwave (180,000)
  integer MM,NQ,NN,KK,ND1,ND2,NFZ,NSPS,NS,NSYNC,NZ,NFFT,NFFT1
  integer isync(50)
  data isync/ 53, 11, 96, 67,119, 49, 83,  8, 92, 85, &  !Random sync pattern
              49, 80,113, 66, 83, 30, 33, 97, 96,116, &
               9, 34,114, 35, 66, 45, 31, 62,108,106, &
               4,126, 86, 98,  7, 49, 61,121,119,115, &
              40, 89,  0, 46, 34,126, 35, 80, 21, 94/

contains
  subroutine sfox_init(mm0,nn0,kk0,itu,fspread,delay,fsample,ts)

    character*2 itu
    integer isps(54)
    integer iloc(1)
    data isps/ 896, 960, 972, 980,1000,1008,1024,1029,1050,1080,   &
              1120,1125,1134,1152,1176,1200,1215,1225,1250,1260,   &
              1280,1296,1323,1344,1350,1372,1400,1440,1458,1470,   &
              1500,1512,1536,1568,1575,1600,1620,1680,1701,1715,   &
              1728,1750,1764,1792,1800,1875,1890,1920,1944,1960,   &
              2000,2016,2025,2048/

    MM=mm0              !Bits per symbol
    NQ=2**MM            !Q, number of MFSK tones
    NN=nn0              !Number of channel symbols
    KK=kk0              !Information symbols
    ND1=25              !Data symbols before sync 
    ND2=NN-ND1          !Data symbols after sync 
    NFZ=3               !First zero

    jsps=nint((12.8-ts)*fsample/NN)
    iloc=minloc(abs(isps-jsps))
    NSPS=isps(iloc(1))  !Samples per symbol
    NS=nint(ts*fsample/NSPS)
    if(mod(NS,2).eq.1) NS=NS+1
    NSYNC=NS*NSPS       !Samples in sync waveform
    NZ=NSPS*(NN+NS)     !Samples in full Tx waveform
    NFFT=32768          !Length of FFT for sync waveform
    NFFT1=2*NSPS        !Length of FFTs for symbol spectra

    fspread=0.0
    delay=0.0
    if(itu.eq.'LQ') then
       fspread=0.5
       delay=0.5
    else if(itu.eq.'LM') then
       fspread=1.5
       delay=2.0
    else if(itu.eq.'LD') then
       fspread=10.0
       delay=6.0
    else if(itu.eq.'MQ') then
       fspread=0.1
       delay=0.5
    else if(itu.eq.'MM') then
       fspread=0.5
       delay=1.0
    else if(itu.eq.'MD') then
       fspread=1.0
       delay=2.0
    else if(itu.eq.'HQ') then
       fspread=0.5
       delay=1.0
    else if(itu.eq.'HM') then
       fspread=10.0
       delay=3.0
    else if(itu.eq.'HD') then
       fspread=30.0
       delay=7.0
    endif

    return
  end subroutine sfox_init

end module sfox_mod
