program sfrx

  use sfox_mod
  use julian
!  use popen_module, only: get_command_as_string

  integer*2 iwave(NMAX)
  integer ihdr(11)
  character*120 fname
  include 'gtag.f90'

  narg=iargc()

  if(narg.lt.1) then
     print*,'Usage:    sfrx fsync ftol infile [...]'
     print*,'          sfrx 775 10 240811_102400.wav'
     print*,'Reads one or more .wav files and calls SuperFox decoder on each.'
     print '(" Git tag: ",z9)',ntag
     go to 999
  endif

  call getarg(1,fname)
  read(fname,*,err=1) fsync
  call getarg(2,fname)
  read(fname,*,err=1) ftol

  nfqso=nint(fsync)
  ntol=nint(ftol)

1 nf=0
  nd=0
  nv=0

  do ifile=3,narg
     call getarg(ifile,fname)
     write(72,*) ifile,narg,fname
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

     call sfrx_sub(nyymmdd,nutc,nfqso,ntol,iwave)

     nf=nf+1
  enddo
   
  if(ncarg.gt.ifile1) write(*,1999) nf,nd,nv
1999 format('nfiles:',i5,'   ndecodes:',i5,'   nverified:',i5)

999 end program sfrx
