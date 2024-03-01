subroutine foxgen2(nslots,cmsg)

  character*40 cmsg(5)

  print*,' '
  do i=1,nslots
     print*,i,cmsg(i)
  enddo

  return
end subroutine foxgen2
