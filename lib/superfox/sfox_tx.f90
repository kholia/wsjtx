program sfox_tx

! Functioins of this program are required in order to create a SuperFox
! transmission.

! The present version goes through the following steps:
!   1. Read old-style Fox messages from file 'sfox_1.dat' in the WSJT-X 
!      writable data directory.
!   2. Parse up to NSlots=5 messages to extract MyCall, up to 10 Hound
!      calls, and the report or RR73 to be sent to each Hound.
!   3. Assemble and encode a single SuperFox message to produce itone(1:151),
!      the array of channel symbol values.
!   4. Write the contents of itone to file 'sfox_2.dat'.

  character*120 fname                          !Full path for sfox.dat
  character*120 line                           !List of SuperFox message pieces
  character*40 cmsg(5)                         !Old-style Fox messages
  integer itone(151)                           !SuperFox channel-symbol values

  open(70,file='fort.70',status='unknown',position='append')
  call getarg(1,fname)
  open(25,file=trim(fname),status='unknown')
  do i=1,5
     read(25,1000,end=10) cmsg(i)
1000 format(a40)
     write(70,*) 'AAA',i,cmsg(i)
  enddo
  i=6

10 close(25)
  nslots=i-1
  write(70,*) 'BBB',nslots
  call foxgen2(nslots,cmsg,line)
  write(70,*) 'CCC ',trim(line)
  
  do i=1,151         !Dummy loop to populate itone during tests
     itone(i)=i-1
  enddo

  i1=index(fname,'sfox_1.dat')
  fname(i1:i1+9)='sfox_2.dat'
  open(25,file=trim(fname),status='unknown')
  write(25,1100) itone
1100 format(20i4)
  close(25)

end program sfox_tx
