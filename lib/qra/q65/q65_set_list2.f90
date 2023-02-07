subroutine q65_set_list2(mycall,hiscall,hisgrid,callers,nhist2,codewords,ncw)

  use types
  parameter (MAX_NCW=206)
  parameter (MAX_CALLERS=40)  !For multiple q3 decodes in NA VHf Contest mode
  character*12 mycall,hiscall
  character*6 hisgrid
  character*37 msg,msgsent
  logical my_std,his_std
  integer codewords(63,MAX_NCW)
  integer itone(85)
  integer isync(22)
  integer time
  type(q3list) callers(MAX_CALLERS)

  data isync/1,9,12,13,15,22,23,26,27,33,35,38,46,50,55,60,62,66,69,74,76,85/

!  print*,'b nhist2:',nhist2
  if(nhist2.ne.-99) return
  
  ncw=0
  if(hiscall(1:1).eq. ' ') return
  call stdcall(mycall,my_std)
  call stdcall(hiscall,his_std)
  
  ncw=MAX_NCW
  do i=1,ncw
     msg=trim(mycall)//' '//trim(hiscall)
     if(.not.my_std) then
        if(i.eq.1 .or. i.ge.6)  msg='<'//trim(mycall)//'> '//trim(hiscall)
        if(i.ge.2 .and. i.le.4) msg=trim(mycall)//' <'//trim(hiscall)//'>'
     else if(.not.his_std) then
        if(i.le.4 .or. i.eq.6) msg='<'//trim(mycall)//'> '//trim(hiscall)
        if(i.ge.7) msg=trim(mycall)//' <'//trim(hiscall)//'>'
     endif
     j0=len(trim(msg))+2
     if(i.eq.2) msg(j0:j0+2)='RRR'
     if(i.eq.3) msg(j0:j0+3)='RR73'
     if(i.eq.4) msg(j0:j0+1)='73'
     if(i.eq.5) then
        if(his_std) msg='CQ '//trim(hiscall)//' '//hisgrid(1:4)
        if(.not.his_std) msg='CQ '//trim(hiscall)
     endif
     if(i.eq.6 .and. his_std) msg(j0:j0+3)=hisgrid(1:4)
     if(i.ge.7 .and. i.le.206) then
        isnr = -50 + (i-7)/2
        if(iand(i,1).eq.1) then
           write(msg(j0:j0+2),'(i3.2)') isnr
           if(msg(j0:j0).eq.' ') msg(j0:j0)='+'
        else
           write(msg(j0:j0+3),'("R",i3.2)') isnr
           if(msg(j0+1:j0+1).eq.' ') msg(j0+1:j0+1)='+'
        endif
     endif

     call genq65(msg,0,msgsent,itone,i3,n3)
     i0=1
     j=0
     do k=1,85
        if(k.eq.isync(i0)) then
           i0=i0+1
           cycle
        endif
        j=j+1
        codewords(j,i)=itone(k) - 1
     enddo
!     write(71,3001) i,isnr,codewords(1:13,i),trim(msg)
!3001 format(i3,2x,i3.2,2x,13i3,2x,a)
  enddo
!  print*,'aa',ncontest,ncw,1970.0 + time()/(365.25*86400.0)

  return
end subroutine q65_set_list2
