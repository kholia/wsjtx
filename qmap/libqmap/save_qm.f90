subroutine save_qm(fname,nutc,dd,ntx30a,ntx30b)

  parameter(NMAX=60*96000)
  character*(*) fname
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

  jz=len(fname)
  fname(jz-1:jz)="qm"
  write(*,3001) fname(jz-13:jz),nutc,rms,db(rms*rms),ia,ib,      &
       nsum/(2*96000),ntx30a,ntx30b
3001 format(a14,2x,i4.4,2f7.1,2i9,3i5)

  return
end subroutine save_qm
