program ft4code

! Provides examples of message packing, LDPC(174,91) encoding, bit and
! symbol ordering, and other details of the FT8 protocol.

  use packjt77
  include 'ft4_params.f90'               !Set various constants
  include 'ft4_testmsg.f90'
  parameter (NWAVE=NN*NSPS)
 
  character*37 msg,msgsent
  character*77 c77
  character*9 comment
  character bad*1,msgtype*18
  integer itone(NN)
  integer*1 msgbits(77),rvec(77),codeword(174)
  data rvec/0,1,0,0,1,0,1,0,0,1,0,1,1,1,1,0,1,0,0,0,1,0,0,1,1,0,1,1,0, &
            1,0,0,1,0,1,1,0,0,0,0,1,0,0,0,1,0,1,0,0,1,1,1,1,0,0,1,0,1, &
            0,1,0,1,0,1,1,0,1,1,1,1,1,0,0,0,1,0,1/

! Get command-line argument(s)
  nargs=iargc()
  if(nargs.ne.1 .and. nargs.ne.3) then
     print*
     print*,'Program ft4code:  Provides examples of message packing, ',       &
          'LDPC(174,91) encoding,'
     print*,'bit and symbol ordering, and other details of the FT4 protocol.'
     print*
     print*,'Usage: ft4code "message"  # Results for specified message'
     print*,'       ft4code -t         # Examples of all message types'
     go to 999
  endif

  call getarg(1,msg)                    !Message to be transmitted
  if(len(trim(msg)).eq.2 .and. msg(1:2).eq.'-t') then
     nmsg=NTEST
  else
     call fmtmsg(msg,iz)          !To upper case; collapse multiple blanks
     nmsg=1
  endif

  write(*,1010)
1010 format(4x,'Message',31x,'Decoded',29x,'Err i3.n3'/100('-')) 

  do imsg=1,nmsg
     if(nmsg.gt.1) msg=testmsg(imsg)
     
! Generate msgsent, msgbits, and itone
     i3=-1
     n3=-1
     call pack77(msg,i3,n3,c77)
     call genft4(msg,0,msgsent,msgbits,itone)
     call encode174_91(msgbits,codeword)
     msgtype=""
     if(i3.eq.0) then
        if(n3.eq.0) msgtype="Free text"
        if(n3.eq.1) msgtype="DXpedition mode"
        if(n3.eq.2) msgtype="EU VHF Contest"
        if(n3.eq.3) msgtype="ARRL Field Day"
        if(n3.eq.4) msgtype="ARRL Field Day"
        if(n3.eq.5) msgtype="Telemetry"
        if(n3.ge.6) msgtype="Undefined type"
     endif 
     if(i3.eq.1) msgtype="Standard msg"
     if(i3.eq.2) msgtype="EU VHF Contest"
     if(i3.eq.3) msgtype="ARRL RTTY Roundup"
     if(i3.eq.4) msgtype="Nonstandard calls"
     if(i3.eq.5) msgtype="EU VHF Contest"
     if(i3.ge.6) msgtype="Undefined msg type"
     if(i3.ge.1) n3=-1
     bad=" "
     comment='         '
     if(msg.ne.msgsent) bad="*"
     if(n3.ge.0) then
        write(*,1020) imsg,msg,msgsent,bad,i3,n3,msgtype,comment
1020    format(i2,'.',1x,a37,1x,a37,1x,a1,2x,i1,'.',i1,1x,a18,1x,a9)
     else
        write(*,1022) imsg,msg,msgsent,bad,i3,msgtype,comment
1022    format(i2,'.',1x,a37,1x,a37,1x,a1,2x,i1,'.',1x,1x,a18,1x,a9)
     endif
  enddo

  if(nmsg.eq.1) then
     write(*,1030) ieor(msgbits,rvec)
1030 format(/'Source-encoded message before scrambling, 77 bits: ',/77i1)
     write(*,1032) rvec
1032 format(/'Scrambling vector, 77 bits: ',/77i1)
     write(*,1034) msgbits
1034 format(/'Source-encoded message after scrambling, 77 bits:'/77i1)
     write(*,1036) codeword(78:91)
1036 format(/'14-bit CRC: ',/14i1)
     write(*,1038) codeword(92:174)
1038 format(/'83 Parity bits: ',/83i1)     
     write(*,1040) 0,itone,0
1040 format(/'Channel symbols (105 tones):'/                            &
          'R Sync',13x,'Data',13x,                                      &
          ' Sync',13x,'Data',13x,                                       &
          ' Sync',13x,'Data',13x,' Sync R'/                             &
          i1,1x,4i1,1x,29i1,1x,4i1,1x,29i1,1x,4i1,1x,29i1,1x,4i1,i2)
  endif

999 end program ft4code
