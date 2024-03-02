subroutine sfox_assemble(ntype,k,msg,mycall0,mygrid0)

! In subsequent calls, assemble all necessary information for a SuperFox
! transmission.

  character*22 msg
  character*22 msg0,msg1,msg2(10),msg3(5)
  character*12 mycall0,mycall
  character*4 mygrid0,mygrid
  integer ntype             !Message type: 0 Free Text
                            !              1 CQ MyCall MyGrid    
                            !              2 Call_1 MyCall RR73
                            !              3 Call_1 MyCall rpt
  integer nmsg(0:3)         !Number of messages of type ntype
  data nmsg/0,0,0,0/,nbits/0/,ntx/0/
  save

  if(mycall0(1:1).ne.' ') mycall=mycall0
  if(mygrid0(1:1).ne.' ') mygrid=mygrid0
  if(k.le.10) then
     if(ntype.eq.0) then
        if(nbits.le.191) then           !Enough room for a free text message?
           nmsg(ntype)=nmsg(ntype)+1
           nbits=nbits+142
           msg0=msg
        endif
     else if(ntype.eq.1) then
        if(nbits.le.290) then
           nmsg(ntype)=nmsg(ntype)+1
           nbits=nbits+43
           msg1=msg
        endif
     else if(ntype.eq.2) then
        if(nbits.le.305) then
           nmsg(ntype)=nmsg(ntype)+1
           nbits=nbits+28
           j=nmsg(ntype)
           msg2(j)=msg
        endif
     else
        if(nbits.le.300) then
           nmsg(ntype)=nmsg(ntype)+1
           nbits=nbits+33
           j=nmsg(ntype)
           msg3(j)=msg
        endif
     endif
     return
  endif

  if(k.ge.11) then
! All necessary pieces are in place. Now encode the SuperFox message and
! generate the waveform to be transmitted.
     ntx=ntx+1                                 !Transmission number
     write(*,3002) ntx,ntype,nmsg(0:3),nbits
3002 format(i3,i5,2x,4i3,i6)
     if(nmsg(0).ge.1) write(*,3010) ntx,msg0
3010 format(i3,2x,a22)
     if(nmsg(1).ge.1) write(*,3010) ntx,msg1
     do i=1,nmsg(2)
        write(*,3010) ntx,msg2(i)
     enddo
     do i=1,nmsg(3)
        write(*,3010) ntx,msg3(i)
     enddo
     nmsg=0
     nbits=0
  endif

  return
end subroutine sfox_assemble
