program sfoxtest

! Generate and test sync waveforms for possible use in SuperFox signal.

  use wavhdr
  include "sfox_params.f90"
  type(hdr) h                            !Header for .wav file
  integer*2 iwave(NMAX)                  !Generated i*2 waveform
  real*4 xnoise(NMAX)                    !Random noise
  real*4 dat(NMAX)                       !Generated real data
  complex cdat(NMAX)                     !Generated complex waveform
  complex clo(NMAX)                      !Complex Local Oscillator
  complex cnoise(NMAX)                   !Complex noise
  complex crcvd(NMAX)                    !Signal as received
  integer imsg(KK)                       !Information symbols
  integer jmsg(KK)                       !Decoded information 
  integer*1 imsg1(7*KK)                  !Copy of imsg in 1-bit i*1 format
  integer idat(NN)                       !Encoded data, 7-bit integers
  integer jdat(NN)                       !Recovered hard-decision symbols
  character fname*17,arg*12
  character c357*357,c14*14 !,chkmsg*15
  
  nargs=iargc()
  if(nargs.ne.8) then
     print*,'Usage:   sfoxtest   f0    DT fspread delay width nran nfiles snr'
     print*,'Example: sfoxtest 1500.0 0.15   0.5   1.0   100    0    10   -10'
     go to 999
  endif
  call getarg(1,arg)
  read(arg,*) f0
  call getarg(2,arg)
  read(arg,*) xdt
  call getarg(3,arg)
  read(arg,*) fspread
  call getarg(4,arg)
  read(arg,*) delay
  call getarg(5,arg)
  read(arg,*) syncwidth
  call getarg(6,arg)
  read(arg,*) nran
  call getarg(7,arg)
  read(arg,*) nfiles
  call getarg(8,arg)
  read(arg,*) snrdb

  rms=100.
  fsample=12000.0                   !Sample rate (Hz)
  baud=12000.0/nsps                 !Keying rate, 11.719 baud for nsps=1024
  h=default_header(12000,NMAX)
  idummy=0
  bandwidth_ratio=2500.0/6000.0

! Generate a message
  do i=1,KK-2
     imsg(i)=i
  enddo

! Append a 14-bit CRC
  imsg(KK-1:KK)=0
  write(c357,'(51b7.7)') imsg(1:KK)
  read(c357,'(357i1)') imsg1
  call get_crc14(imsg1,7*KK,ncrc0)
  write(c14,'(b14.14)') ncrc0
  read(c14,'(2b7.7)') imsg(KK-1:KK)

  call rs_init_sf(MM,NQ,NN,KK,NFZ)          !Initialize the Karn codec
  call rs_encode_sf(imsg,idat)              !Encode imsg into idat
  
! Generate cdat (SuperFox waveform) and clo (LO for sync detection)
  call gen_sfox(idat,f0,fsample,syncwidth,cdat,clo)

  do isnr=0,-30,-1
     snr=isnr
     if(snrdb.ne.0.0) snr=snrdb
     sig=sqrt(2*bandwidth_ratio)*10.0**(0.05*snr)
     if(snr.gt.90.0) sig=1.0
     ngoodsync=0
     ngood=0

     do ifile=1,nfiles
        xnoise=0.
        cnoise=0.
        if(snr.lt.90) then
           do i=1,NMAX                     !Generate Gaussian noise
              x=gran()
              y=gran()
              xnoise(i)=x
              cnoise(i)=cmplx(x,y)
           enddo
        endif

        f1=f0
        if(f0.eq.0.0) then
           f1=1500.0 + 200.0*(ran1(idummy)-0.5)
           xdt=2.0*(ran1(idummy)-0.5)
           call gen_sfox(idat,f1,fsample,syncwidth,cdat,clo)
        endif
        
        crcvd=0.
        crcvd(1:NMAX)=cshift(sig*cdat(1:NMAX),-nint(xdt*fsample)) + cnoise

        dat=aimag(sig*cdat(1:NMAX)) + xnoise     !Add generated AWGN noise
        fac=32767.0
        if(snr.ge.90.0) iwave(1:NMAX)=nint(fac*dat(1:NMAX))
        if(snr.lt.90.0) iwave(1:NMAX)=nint(rms*dat(1:NMAX))

        if(fspread.ne.0 .or. delay.ne.0) call watterson(crcvd,NMAX,NZ,fsample,&
             delay,fspread)

! Find signal freq and DT
        call sync_sf(crcvd,clo,snrdb,f,t)
        ferr=f-f1
        terr=t-xdt
        if(abs(ferr).lt.5.0 .and. abs(terr).lt.0.01) ngoodsync=ngoodsync+1

        call hard_symbols(crcvd,f,t,jdat)           !Get hard symbol values
        nera=0
        call rs_decode_sf(idat,iera,nera,jmsg,nfixed)  !Call the decoder
        write(c357,'(51b7.7)') jmsg(1:KK)
        read(c357,'(357i1)') imsg11
        call get_crc14(imsg1,7*KK,ncrc)

        nharderr=count(jdat.ne.idat)                !Count hard errors
  
        if(snrdb.ne.0) then
           fname='000000_000001.wav'
           write(fname(8:13),'(i6.6)') ifile
           open(10,file=trim(fname),access='stream',status='unknown')
           write(10) h,iwave(1:NMAX)                !Save the .wav file
           close(10)
           write(*,1100) f1,xdt
1100       format(/'f0:',f7.1,'  xdt:',f6.2)
           write(*,1112) f,t
1112       format('f: ',f7.1,'   DT:',f6.2)
           write(*,1110) ferr,terr
1110       format('err:',f6.1,f12.2)
           write(*,1120) nharderr
1120       format('Hard errors:',i4)
        endif

        if(nharderr.le.38) ngood=ngood+1            !(125-49)/2 = 38
!        write(13,1200) ifile,snr,ferr,terr,nharderr
!1200    format(i5,3f10.3,i5)
     enddo  ! ifile
     fgoodsync=float(ngoodsync)/nfiles
     fgood=float(ngood)/nfiles
     if(isnr.eq.0) write(*,1300)
1300 format('    SNR     N  fsync  fgood'/  &
            '----------------------------')
     write(*,1310) snr,nfiles,fgoodsync,fgood
1310 format(f7.2,i6,2f7.2)
     if(snrdb.ne.0.0) exit
     if(fgoodsync.lt.0.5) exit
  enddo  ! isnr

999 end program sfoxtest
