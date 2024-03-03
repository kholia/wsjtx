program sfox_tx

  character*120 fname
  character*40 cmsg(5)
  integer itone(151)

  call getarg(1,fname)
  open(25,file=trim(fname),status='unknown')
  do i=1,5
     read(25,1000,end=10) cmsg(i)
1000 format(a40)
!     write(*,1000) cmsg(i)
  enddo

10 rewind(25)
  do i=1,151
     itone(i)=i-1
  enddo
  write(25,1100) itone
1100 format(20i4)  
  close(25)

end program sfox_tx
