subroutine foxgen2(nslots,cmsg)

  use packjt77
  character*40 cmsg(5)
  character*37 msg
  character*12 mycall
  character*4 mygrid
  character*6 hiscall_1,hiscall_2
  character*4 rpt_1,rpt_2
  character*13 w(19)
  integer nw(19)
  integer ntype        !Message type: 0 Free Text
                       !              1 CQ MyCall MyGrid
                       !              2 Call_1 Call_2 RR73
                       !              3 Call_1 Call_2 rpt
                       !              4 Call_1 RR73; Call_2 <MyCall> rpt
!  save mycall,mygrid
  
  if(nslots.lt.1 .or. nslots.gt.5) return
  print*,' '
  do i=1,nslots
     hiscall_1=''
     hiscall_2=''
     mycall=''
     mygrid=''
     rpt_1=''
     rpt_2=''
     msg=cmsg(i)(1:37)
     call split77(msg,nwords,nw,w)
     ntype=0
     if(msg(1:3).eq.'CQ ') then
        ntype=1
        mycall=w(2)(1:12)
        mygrid=w(3)(1:4)
     else if(index(msg,';').gt.0) then
        ntype=4
        hiscall_1=w(1)(1:6)
        hiscall_2=w(3)(1:6)
        rpt_1='RR73'
        rpt_2=w(5)(1:4)
        mycall=w(4)(2:nw(4)-1)
     else if(index(msg,' RR73').gt.0) then
        ntype=2
        hiscall_1=w(1)(1:6)
        mycall=w(2)(1:12)
        rpt_1='RR73'
     else if(nwords.eq.3 .and. nw(3).eq.3 .and. &
          (w(3)(1:1).eq.'-' .or. w(3)(1:1).eq.'+')) then
        ntype=3
        hiscall_1=w(1)(1:6)
        mycall=w(2)(1:12)
        rpt_1=w(3)(1:4)
     endif
     write(*,3001) ntype,cmsg(i),hiscall_1,rpt_1,hiscall_2,rpt_2,  &
          mycall(1:6),mygrid
3001 format(i1,2x,a37,1x,a6,1x,a4,1x,a6,1x,a4,1x,a6,1x,a4)

!     if(ntype.eq.0) call
  enddo

  return
end subroutine foxgen2
