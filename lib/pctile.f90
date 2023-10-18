subroutine pctile(x,npts,npct,xpct)

  real x(npts)
  real,allocatable :: tmp(:)

  if(npts.lt.0) go to 900
  allocate(tmp(npts))

  tmp=x
  call shell(npts,tmp)
  j=nint(npts*0.01*npct)
  if(j.lt.1) j=1
  if(j.gt.npts) j=npts
  xpct=tmp(j)
  deallocate(tmp)

900 return
end subroutine pctile
