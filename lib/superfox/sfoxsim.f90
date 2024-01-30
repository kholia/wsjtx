program sfoxsim

! Generate a SuperFox waveform with specified SNR and channel parameters.
! Output is saved to a *.wav file.
! SuperFox uses a (127,51) code with 7-bit symbols, punctured to (125,49).
! The puncured symbols contain a 14-bit CRC.
! First tests use RS(127,51) code and Berlekamp-Massey decoder.

  use wavhdr
!  use packjt77
  parameter (NMAX=15*12000)
  parameter (NSPS=1024,NSYNC=2*12000)
  parameter (NWAVE=125*NSPS+NSYNC)
  type(hdr) h                            !Header for .wav file
  character arg*12,fname*17
!  character msg37*37,msgsent37*37
  complex c0(0:NMAX-1)
  complex c(0:NMAX-1)
  complex cwave(0:NWAVE-1)
  real wave(NMAX)
  real xjunk(NWAVE)
  real xdat(51)
  integer*1 idat(51)
  integer itone(125)
  integer*1 msgbits(77)
  integer*2 iwave(NMAX)                  !Generated full-length waveform

! Get command-line argument(s)
  nargs=iargc()
  if(nargs.ne.6) then
     print*,'Usage:   sfoxsim   f0    DT fSpread del nfiles snr'
     print*,'Example: sfoxsim 1500.0 0.0   0.1   1.0   10   -15'
     go to 999
  endif
  call getarg(1,arg)
  read(arg,*) f0                         !Frequency (only used for single-signal
  call getarg(2,arg)
  read(arg,*) xdt                        !Time offset from nominal (s)
  call getarg(3,arg)
  read(arg,*) fspread                    !Watterson frequency spread (Hz)
  call getarg(4,arg)
  read(arg,*) delay                      !Watterson delay (ms)
  call getarg(5,arg)
  read(arg,*) nfiles                     !Number of files
  call getarg(6,arg)
  read(arg,*) snrdb                      !SNR_2500

  twopi=8.0*atan(1.0)
  fs=12000.0                             !Sample rate (Hz)
  dt=1.0/fs                              !Sample interval (s)
  tt=NSPS*dt                             !Duration of symbols (s)
  baud=1.0/tt                            !Keying rate (baud)
  bw=128*baud                            !Occupied bandwidth (Hz)
  tsync=NSYNC*dt                         !Duration of analog sync function
  txt=tsync + 125*NSPS*dt                !Overall transmission length (s)
  bandwidth_ratio=2500.0/(fs/2.0)
  sig=sqrt(2*bandwidth_ratio) * 10.0**(0.05*snrdb)
  if(snrdb.gt.90.0) sig=1.0

  write(*,1000) f0,xdt,fspread,delay,tsync,txt,bw,snrdb
1000 format('f0:',f7.1,'    DT:',f6.2,'   fSpread:',f5.1,'   delay:',f4.1/  &
          'Tsync:',f4.1,'   TxT:',f6.1,'      BW:',f7.1,'   SNR:',f6.1)
  write(*,*)  
  
! Source-encode, then get itone()

  call random_number(xdat)
  idat=int(128*xdat)
  itone=0
  itone(1:49)=idat(1:49)
  
  write(*,'(20i4)') idat
  write(*,*) 
  write(*,'(a17)') 'Channel symbols: '
  write(*,'(20i4)') itone
  write(*,*)  
  if(nsps.ne.-99) go to 999

  do ifile=1,nfiles
     c0=0.
     c0(0:NWAVE-1)=cwave
     c0=cshift(c0,-nint((xdt+0.5)/dt))
!     if(fspread.ne.0.0 .or. delay.ne.0.0) call watterson(c0,NMAX,NWAVE,fs,delay,fspread)
     c=sig*c0
  
     wave=imag(c)
     peak=maxval(abs(wave))
     nslots=1
   
     if(snrdb.lt.90) then
        do i=1,NMAX                   !Add gaussian noise at specified SNR
           xnoise=gran()
           wave(i)=wave(i) + xnoise
        enddo
     endif

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
     write(*,1110) ifile,xdt,f0,snrdb,fname
1110 format(i4,f7.2,f8.2,f7.1,2x,a17)
  enddo    
999 end program sfoxsim
