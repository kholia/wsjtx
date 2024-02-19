program ft8sim_gfsk

! Generate simulated "type 2" ft8 files
! Output is saved to a *.wav file.

  use wavhdr
  use packjt77
  include 'ft8_params.f90'               !Set various constants
  parameter (NWAVE=NN*NSPS)
  type(hdr) h                            !Header for .wav file
  character arg*12,fname*17
  character msg37*37,msgsent37*37
  character c77*77
  complex c0(0:NMAX-1)
  complex c(0:NMAX-1)
  complex cwave(0:NWAVE-1)
  real wave(NMAX)
  real xjunk(NWAVE)
  integer itone(NN)
  integer*1 msgbits(77)
  integer*2 iwave(NMAX)                  !Generated full-length waveform

! Get command-line argument(s)
  nargs=iargc()
  if(nargs.ne.7) then
     print*,'Usage:    ft8sim "message"                 f0     DT fdop del nfiles snr'
     print*,'Examples: ft8sim "K1ABC W9XYZ EN37"       1500.0 0.0  0.1 1.0   10   -18'
     print*,'          ft8sim "WA9XYZ/R KA1ABC/R FN42" 1500.0 0.0  0.1 1.0   10   -18'
     print*,'          ft8sim "K1ABC RR73; W9XYZ <KH1/KH7Z> -11" 300 0 0 0 25 1 -10'
     print*,'          ft8sim "<G4ABC/P> <PA9XYZ> R 570007 JO22DB" 1500 0 0 0 1 -10'
     go to 999
  endif
  call getarg(1,msg37)                   !Message to be transmitted
  call getarg(2,arg)
  read(arg,*) f0                         !Frequency (only used for single-signal)
  call getarg(3,arg)
  read(arg,*) xdt                        !Time offset from nominal (s)
  call getarg(4,arg)
  read(arg,*) fspread                    !Watterson frequency spread (Hz)
  call getarg(5,arg)
  read(arg,*) delay                      !Watterson delay (ms)
  call getarg(6,arg)
  read(arg,*) nfiles                     !Number of files
  call getarg(7,arg)
  read(arg,*) snrdb                      !SNR_2500

  nsig=1
  if(f0.lt.100.0) then
     nsig=f0
     f0=1500
  endif

  nfiles=abs(nfiles)
  twopi=8.0*atan(1.0)
  fs=12000.0                             !Sample rate (Hz)
  dt=1.0/fs                              !Sample interval (s)
  tt=NSPS*dt                             !Duration of symbols (s)
  baud=1.0/tt                            !Keying rate (baud)
  bw=8*baud                              !Occupied bandwidth (Hz)
  txt=NZ*dt                              !Transmission length (s)
  bt=2.0                         
  bandwidth_ratio=2500.0/(fs/2.0)
  sig=sqrt(2*bandwidth_ratio) * 10.0**(0.05*snrdb)
  if(snrdb.gt.90.0) sig=1.0
  txt=NN*NSPS/12000.0

  ! Source-encode, then get itone()
  i3=-1
  n3=-1
  call pack77(msg37,i3,n3,c77)
  call genft8(msg37,i3,n3,msgsent37,msgbits,itone)
  call gen_ft8wave(itone,NN,NSPS,bt,fs,f0,cwave,xjunk,1,NWAVE)  !Generate complex cwave

  write(*,*)  
  write(*,'(a23,a37,3x,a7,i1,a1,i1)') 'Decoded message: ',msgsent37,'i3.n3: ',i3,'.',n3
  write(*,1000) f0,xdt,txt,snrdb,bw
1000 format('f0:',f9.3,'   DT:',f6.2,'   TxT:',f6.1,'   SNR:',f6.1,    &
       '  BW:',f4.1)
  write(*,*)  
  if(i3.eq.1) then
    write(*,*) '         mycall                         hiscall                    hisgrid'
    write(*,'(28i1,1x,i1,1x,28i1,1x,i1,1x,i1,1x,15i1,1x,3i1)') msgbits(1:77) 
  else
    write(*,'(a14)') 'Message bits: '
    write(*,'(77i1)') msgbits
  endif
  write(*,*) 
  write(*,'(a17)') 'Channel symbols: '
  write(*,'(79i1)') itone
  write(*,*)  

  call sgran()

  do ifile=1,nfiles
     c0=0.
     c0(0:NWAVE-1)=cwave
     c0=cshift(c0,-nint((xdt+0.5)/dt))
     if(fspread.ne.0.0 .or. delay.ne.0.0) call watterson(c0,NMAX,NWAVE,fs,delay,fspread)
     c=sig*c0
  
     wave=imag(c)
     peak=maxval(abs(wave))
     nslots=1

     psig=0.
     pnoise=0.
     if(snrdb.lt.90) then
        do i=1,NMAX                   !Add gaussian noise at specified SNR
           xnoise=gran()
           if(i.ge.6001 .and. i.le.157692) then
              psig=psig + wave(i)*wave(i)     !Signal power
              pnoise=pnoise + xnoise*xnoise   !Noise power in signal interval
           endif
           wave(i)=wave(i) + xnoise
        enddo
     endif

! Noise power in signal interval and 2500 Hz bandwidth:
     pnoise=bandwidth_ratio*pnoise
     snr_2500=db(psig/pnoise)                 !SNR in 2500 Hz bandwidth

     gain=100.0
     if(snrdb.lt.90.0) then
       wave=gain*wave
     else
       datpk=maxval(abs(wave))
       fac=32766.9/datpk
       wave=fac*wave
     endif
     if(any(abs(wave).gt.32767.0)) print*,"Warning - data will be clipped."
     iwave=nint(wave)
     h=default_header(12000,NMAX)
     write(fname,1102) ifile
1102 format('000000_',i6.6,'.wav')
     open(10,file=fname,status='unknown',access='stream')
     write(10) h,iwave                !Save to *.wav file
     close(10)
     write(*,1110) ifile,xdt,f0,snrdb,fname,snr_2500
1110 format(i4,f7.2,f8.2,f7.1,2x,a17,f8.2)
  enddo    
999 end program ft8sim_gfsk
