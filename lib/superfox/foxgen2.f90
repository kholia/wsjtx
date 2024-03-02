subroutine foxgen2(nslots,cmsg)

! Called from foxgen() when it's time to encode a SuperFox message and
! generate the waveform to be transmitted.  We need to parse the old-style
! Fox messages and extract the necessary pieces.

  use packjt77
  character*40 cmsg(5)                !Old-style Fox messages are here
  character*37 msg
  character*22 sfmsg
  character*12 mycall
  character*4 mygrid
  character*6 hiscall_1,hiscall_2
  character*4 rpt_1,rpt_2
  character*13 w(19)
  integer nw(19)
  integer ntype        !Message type: 0 Free Text
                       !              1 CQ MyCall MyGrid
                       !              2 Call_1 MyCall RR73
                       !              3 Call_1 MyCall rpt_1
                       !              4 Call_1 RR73; Call_2 <MyCall> rpt_2
  
  if(nslots.lt.1 .or. nslots.gt.5) return
  k=0
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
!     write(*,3001) ntype,cmsg(i),hiscall_1,rpt_1,hiscall_2,rpt_2,  &
!          mycall(1:6),mygrid
!3001 format(i1,2x,a37,1x,a6,1x,a4,1x,a6,1x,a4,1x,a6,1x,a4)

     k=k+1
     if(ntype.le.3) call sfox_assemble(ntype,k,msg(1:22),mycall,mygrid)
     if(ntype.eq.4) then
        sfmsg=w(1)(1:nw(1))//' '//mycall(1:len(trim(mycall))+1)//'RR73'
        call sfox_assemble(2,k,sfmsg,mycall,mygrid)
        sfmsg=w(3)(1:nw(3))//' '//mycall(1:len(trim(mycall))+1)//w(5)(1:3)
        k=k+1
        call sfox_assemble(3,k,sfmsg,mycall,mygrid)
     endif

  enddo
  call sfox_assemble(ntype,11,msg(1:22),mycall,mygrid)      !k=11 to finish up

  return
end subroutine foxgen2
