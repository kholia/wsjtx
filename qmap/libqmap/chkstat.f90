subroutine chkstat(dd,ihsym,bSkip)

  real dd(2,5760000)
  real power(60)
  logical*1 bSkip

  k=0
  do i=1,60
     sq=0.
     do j=1,96000
        k=k+1
        sq=sq + dd(1,k)*dd(1,k) + dd(2,k)*dd(2,k)
     enddo
     power(i)=sq/(2.0*96000.0)
  enddo
  bSkip=.false.
  n1=count(power(1:30).lt.15.0)
  n2=count(power(31:60).lt.15.0)
  if(ihsym.le.200 .and. n1.gt.15) bSkip=.true.
  if(ihsym.gt.200 .and. n2.gt.15) bSkip=.true.
!  print*,'A',ihsym,n1,n2,bSkip

  return
end subroutine chkstat
