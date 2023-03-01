subroutine get_q3list(fname,bDiskData,nlist,list)

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
  logical*1 bDiskData
  integer time
  integer nt(8)
  integer indx(MAX_CALLERS)
  type(q3list) callers(MAX_CALLERS)
  character*256 jpleph_file_name
  common/jplcom/jpleph_file_name

  nhist2=0
  open(24,file=fname,status='unknown',form='unformatted')
  read(24,end=1) nhist2
  if(nhist2.ge.1 .and. nhist2.le.40) then
     read(24,end=1) callers(1:nhist2)
  else
     nhist2=0
  endif
1 close(24)

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
     call moondopjpl(nt(1),nt(2),nt(3),uth,-xlon,xlat,RAMoon,DecMoon,    &
          xLST,HA,AzMoon,ElMoon,vr,techo)
     moon_el=nint(ElMoon)
     write(list(i),1000) i,callers(j)%nfreq,callers(j)%call,    &
          callers(j)%grid,moon_el,age,char(0)
1000 format(i2,'.',i6,2x,a6,2x,a4,i5,f7.1,a1)

     h1=mod(now,86400)/3600.0
     h2=mod(callers(i)%nsec,86400)/3600.0
     hd=h1-h2
     if(hd.lt.0.0) hd=hd+24.0
!     write(*,3301) i,callers(i)%call,now,callers(i)%nsec,h1,h2,hd
!3301 format(i3,2x,a6,2i12,3f10.6)

  enddo
  
  return
end subroutine get_q3list

subroutine jpl_setup(fname)
  character*256 fname,jpleph_file_name
  common/jplcom/jpleph_file_name
  jpleph_file_name=fname
  return
end subroutine jpl_setup
