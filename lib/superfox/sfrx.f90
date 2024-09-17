program sfrx

  use sfox_mod
  use julian
  use popen_module, only: get_command_as_string

  integer*2 iwave(NMAX)
  integer ihdr(11)
  integer*8 secday,ntime8,ntime8_chk
  integer*1 xdec(0:49)
  character*120 fname,cmnd,cresult
  character*13 foxcall,foxcall_chk
  character*256 ppath
  complex c0(NMAX)                    !Complex form of signal as received
  real dd(NMAX)
  logical crc_ok
  logical use_otp
  data secday/86400/
  include 'gtag.f90'

  use_otp = .FALSE.
  fsync=750.0
  ftol=100.0
  narg=iargc()

  if(narg.lt.1) then
!     print*,'Usage:    sfrx [fsync [ftol]] infile [...]'
!     print*,'Examples: sfrx 230305_011230.wav'
!     print*,'          sfrx 775 10 240811_102400.wav'
!     print*,'Reads one or more .wav files and calls SuperFox decoder on each.'
!     print*,'Defaults: fsync=750, ftol=100'
     print '(" Git tag: ",z9)',ntag
     go to 999
  endif

  ifile1=1
  call getarg(1,fname)
  read(fname,*,err=1) fsync
  ifile1=2
  call getarg(2,fname)
  read(fname,*,err=1) ftol
  ifile1=3

1 nf=0
  nd=0
  nv=0

  fsample=12000.0
  call sfox_init(7,127,50,'no',fspread,delay,fsample,24)

  npts=15*12000

  do ifile=ifile1,narg
     call getarg(ifile,fname)
     write(72,*) ifile,narg,fname
          if(fname.eq.'OTP') then
             use_otp = .TRUE.
             cycle
          endif
     open(10,file=trim(fname),status='old',access='stream',err=4)

     go to 5
4    print*,'Cannot open file ',trim(fname)
     go to 999
5    read(10) ihdr,iwave
     close(10)

     nz=len(trim(fname))
     nyymmdd=ihdr(1)
     nutc=ihdr(2)
     if(fname(nz-3:nz).eq.'.wav') then
        read(fname(nz-16:nz-11),*) nyymmdd
        read(fname(nz-9:nz-4),*) nutc
     endif
     if(nyymmdd.eq.-1) then
        ntime8=itime8()/30
        ntime8=30*ntime8
     else
        iyr=2000+nyymmdd/10000
        imo=mod(nyymmdd/100,100)
        iday=mod(nyymmdd,100)
        ih=nutc/10000
        im=mod(nutc/100,100)
        is=mod(nutc,100)
        ntime8=secday*(JD(iyr,imo,iday)-2440588) + 3600*ih + 60*im + is
     endif

     dd=iwave
     call sfox_remove_ft8(dd,npts)

     call sfox_ana(dd,npts,c0,npts)

     ndepth=3
     dth=0.5
     damp=1.0

     call qpc_decode2(c0,fsync,ftol, xdec,ndepth,dth,damp,crc_ok,   &
          snrsync,fbest,tbest,snr)
     if(crc_ok) then
        nsnr=nint(snr)
        if (use_otp) then
           nsignature = 1
        else
           nsignature = 0
        endif
        
        call sfox_unpack(nutc,xdec,nsnr,fbest-750.0,tbest,foxcall,nsignature)
! Execute 'foxchk <foxcall> <ntime8>' to get the correct signature value.
        write(cmnd,1102) trim(foxcall),ntime8
1102    format('foxchk  ',a,i12)
        call getarg(0,ppath)
        lindex=index(ppath,'sfrx')-1
        if (.not.use_otp) then
            cmnd=ppath(1:lindex)//cmnd
            cresult=get_command_as_string(trim(cmnd))
            read(cresult,1104) foxcall_chk,ntime8_chk,nsignature_chk
1104        format(a11,i13,i10.7)
            if(nsignature.eq.nsignature_chk) write(*,1110) trim(foxcall)
1110        format(a,' verified')
            if(nsignature.eq.nsignature_chk) nv=nv+1
        endif
        nd=nd+1
!     else
!        i0=index(fname,'.wav')
!        write(60,3060) trim(fname)
!3060    format('cp ',a,' nodecode')
     endif
     nf=nf+1
  enddo
  ncarg=narg
  if (use_otp) then
     ncarg=ncarg-1
  endif
  
  if(ncarg.gt.ifile1) write(*,1999) nf,nd,nv
1999 format('nfiles:',i5,'   ndecodes:',i5,'   nverified:',i5)

999 end program sfrx
