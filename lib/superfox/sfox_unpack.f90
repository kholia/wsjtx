subroutine sfox_unpack(imsg)

  use packjt77
  integer imsg(48)
  character*336 msgbits
  character*22 msg(10)
  character*13 foxcall,c13
  character*4 crpt(5)
  logical success

  write(msgbits,1000) imsg
1000 format(48b7.7)
  read(msgbits(331:336),'(b6)') ntype            !Message type

  if(ntype.eq.1) then                            !Get the Fox callsign
     read(msgbits(271:328),'(b58)') n58          !Compound Fox call
     call unpack28(n58,foxcall,success)
  else
     read(msgbits(303:330),'(b28)') n28          !Standard Fox call
     call unpack28(n28,foxcall,success)
  endif

  j=171
  do i=1,5                                       !Extract the reports
     read(msgbits(j:j+3),'(b4)') n
     if(n.eq.15) then
        crpt(i)='RR73'
     else
        write(crpt(i),1006) 2*n-18
1006    format(i3.2)
        if(crpt(i)(1:1).eq.' ') crpt(i)(1:1)='+'
     endif
     j=j+32
  enddo

! Unpack and format user-level messages:
  do i=1,10
     j=28*i - 27
     if(i.gt.5) j=143 + (i-5)*32
     read(msgbits(j:j+27),'(b28)') n28
     if(n28.eq.0) cycle
     call unpack28(n28,c13,success)
     msg(i)=trim(c13)//' '//trim(foxcall)
     if(i.le.5) msg(i)=trim(msg(i))//' RR73'
     if(i.gt.5) msg(i)=trim(msg(i))//' '//crpt(i-5)
     write(*,3001) i,trim(msg(i))
3001 format(i2,2x,a)
  enddo

  return
end subroutine sfox_unpack
