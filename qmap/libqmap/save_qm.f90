subroutine save_qm(fname,prog_id,mycall,mygrid,dd,ntx30a,ntx30b)

  parameter(NMAX=60*96000)
  character*(*) fname,prog_id,mycall,mygrid
  character prog_id_24*24,mycall_12*12,mygrid_6*6
  real*4 dd(2,NMAX)
  integer*1 id1(2,NMAX)

  ia=1
  ib=NMAX
  if(ntx30a.gt.5) ia=NMAX/2+1
  if(ntx30b.gt.5) ib=NMAX/2
  
  sq=0.
  do i=ia,ib
     x=dd(1,i)
     y=dd(2,i)
     sq=sq + x*x + y*y
  enddo
  nsum=2*(ib-ia+1)
  rms=sqrt(sq/nsum)

  nbad=0
  dmax=0.
  fac=10.0/rms
  do i=ia,ib
     x=fac*dd(1,i)
     y=fac*dd(2,i)
     ax=abs(x)
     ay=abs(y)
     dmax=max(dmax,ax,ay)
     if(ax.gt.127.0) then
        x=0.
        nbad=nbad+1
     endif
     if(ay.gt.127.0) then
        y=0.
        nbad=nbad+1
     endif
     id1(1,i)=nint(x)
     id1(2,i)=nint(y)
  enddo
  if(ia.gt.30*96000) id1(1:2,1:ia-1)=0
  if(ib.eq.30*96000) id1(1:2,ib+1:60*96000)=0
  
  jz=len(fname)
  fname(jz-1:jz)="qm"
  write(*,3001) fname(jz-13:jz),rms,db(rms*rms),dmax,nbad,ia,ib,      &
       nsum/(2*96000),ntx30a,ntx30b
3001 format(a14,3f7.1,i8,2i9,3i5)

  open(29,file=trim(fname),status='unknown',access='stream')
  prog_id_24=prog_id//"        "
  mycall_12=mycall
  mygrid_6=mygrid
  write(29) prog_id_24,mycall_12,mygrid_6,ntx30a,ntx30b,ia,ib
  write(29) id1(1:2,ia:ib)

  return
end subroutine save_qm

