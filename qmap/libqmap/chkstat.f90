subroutine chkstat(dd,nhsym,dbdiff)

  real dd(2,5760000)

  sq0=0.
  sq1=0.
  k=0
  do i=1,60
     sq=0.
     do j=1,96000
        k=k+1
        sq=sq + dd(1,k)*dd(1,k) + dd(2,k)*dd(2,k)
     enddo 
     if(i.ge.12 .and. i.le.24) sq0=sq0+sq
     if(i.ge.42 .and. i.le.54) sq1=sq1+sq
  enddo
  db0=db(1.0+sq0)
  db1=db(1.0+sq1)
  dbdiff=db0-db1
  
  return
end subroutine chkstat
