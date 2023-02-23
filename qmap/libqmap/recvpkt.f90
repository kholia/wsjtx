subroutine recvpkt(nsam,nblock2,userx_no,k,buf4,buf8)

! Reformat timf2 data from Linrad and stuff data into r*4 array dd().

  include 'njunk.f90'
  parameter (NSMAX=60*96000)          !Total sample intervals per minute
  parameter (NFFT=32768)
  integer*1 userx_no
  real*4 d4,buf4(*)                   !(348)
  real*8 d8,buf8(*)                   !(174)
  integer*2 jd(4),kd(2),nblock2
  real*4 yd(2)
  real*8 fcenter
  common/datcom/dd(2,5760000),ss(322,NFFT),savg(NFFT),fcenter,nutc,  &
       junk(NJUNK)
  equivalence (kd,d4)
  equivalence (jd,d8,yd)

  if(nblock2.eq.-9999) nblock2=-9998    !Silence a compiler warning
  if(nsam.eq.-1) then
! Move data from the UDP packet buffer into array dd().
     if(userx_no.eq.-1) then
        do i=1,174                    !One RF channel, r*4 data
           k=k+1
           d8=buf8(i)
           dd(1,k)=yd(1)
           dd(2,k)=yd(2)
        enddo
     else if(userx_no.eq.1) then
        do i=1,348                    !One RF channel, i*2 data
           k=k+1
           d4=buf4(i)
           dd(1,k)=kd(1)
           dd(2,k)=kd(2)
        enddo
     endif
  else
     if(userx_no.eq.1) then
        do i=1,nsam                    !One RF channel, r*4 data
           k=k+1
           d4=buf4(i)
           dd(1,k)=kd(1)
           dd(2,k)=kd(2)

           k=k+1
           dd(1,k)=kd(1)
           dd(2,k)=kd(2)
        enddo
     endif
  endif

  return
end subroutine recvpkt
