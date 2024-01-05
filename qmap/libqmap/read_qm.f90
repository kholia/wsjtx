subroutine read_qm(fname)

  include 'njunk.f90'
  parameter(NMAX=60*96000,NFFT=32768)
  character*(*) fname
  character prog_id*24,mycall*12,mygrid*6
  real*8 fcenter
  integer nxtra(15)                        !For possible future additions
  integer*1 id1(2,NMAX)
  common/datcom/dd(2,5760000),ss(400,NFFT),savg(NFFT),                  &
       fcenter,nutc,fselected,mousedf,mousefqso,nagain,                 &
       ndepth,ndiskdat,ntx60,newdat,nn1,nn2,nfcal,nfshift,              &
       ntx30a,ntx30b   !...

  open(28,file=trim(fname),status='old',access='stream',err=900)
  read(28) prog_id,mycall,mygrid,fcenter,nutc,ntx30a,ntx30b,ndop00,ndop58,  &
       ia,ib,fac0,nxtra
  fac=1.0
  if(fac0.gt.0.0) fac=1.0/fac0
  id1=0
  read(28) id1(1:2,ia:ib)
  dd=0.
  dd(1:2,ia:ib)=fac*id1(1:2,ia:ib)   !Boost back to previous level

!  write(*,3001) prog_id,mycall(1:6),mygrid,fcenter,nutc,ntx30a,ntx30b,  &
!       ndop00,ndop58,ia,ib
!3001 format(a24,2x,a6,2x,a6,f10.3,i6.4,2i5/4i9)
  go to 999

900 print*,'Cannot open ',fname

999 close(28)
  return
end subroutine read_qm