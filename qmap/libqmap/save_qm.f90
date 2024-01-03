subroutine save_qm(nutc,dd,ntx30a,ntx30b)

  parameter(NMAX=60*96000)
  real*4 dd(2,NMAX)
  integer*1 id1(2,NMAX)

  ia=1
  ib=NMAX
  if(ntx30a.gt.5) ia=NMAX/2+1
  if(ntx30b.gt.5) ib=NMAX/2
  
  sq=0.
  do i=ia,ib
     x=dd(1,i)
     y=dd92,i)
     sq=sq + x*x + y*y
  enddo
  nsum=ib-ia+1
  rms=sqrt(sq/nsum)

  write(*,3001) nutc,rms
3001 format(i4.4,f10.2)

  return
end subroutine save_qm
