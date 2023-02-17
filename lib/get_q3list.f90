subroutine get_q3list(fname,nlist,list)

  type q3list
     character*6 call
     character*4 grid
     integer nsec
     integer nfreq
  end type q3list

  parameter (MAX_CALLERS=40)
  character*(*) fname
  character*36 list(40)
  character*8 grid6
  integer time
  integer nt(8)
  integer indx(MAX_CALLERS)
  type(q3list) callers(MAX_CALLERS)


  nhist2=0
  open(24,file=fname,status='unknown',form='unformatted')
  read(24,end=1) nhist2,callers(1:nhist2)
1  close(24)

  moon_el=0
  now=time()
  call date_and_time(values=nt)
  uth=nt(5) + (nt(6)-nt(4))/60.0 + nt(7)/3600.0
  nlist=nhist2
  call indexx(callers(1:nlist)%nfreq,nlist,indx)
  do i=1,nlist
     age=(now - callers(i)%nsec)/3600.0
     j=indx(i)
     grid6=callers(j)%grid//'mm'
     call grid2deg(grid6,xlon,xlat)
     call sun(nt(1),nt(2),nt(3),uth,-xlon,xlat,RASun,DecSun,xLST,        &
          AzSun,ElSun,mjd,day)
!     call moondopjpl(nt(1),nt(2),nt(3),uth,-xlon,xlat,RAMoon,DecMoon,    &
!          xLST,HA,AzMoon,ElMoon,vr,techo)
     print*,i,grid6,azmoon,elsun,elmoon
     write(list(i),1000) i,callers(j)%nfreq,callers(j)%call,    &
          callers(j)%grid,moon_el,age,char(0)
1000 format(i2,'.',i6,2x,a6,2x,a4,i5,f7.1,a1)
  enddo
  
  return
end subroutine get_q3list
