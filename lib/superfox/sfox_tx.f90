program sfox_tx

  character*120 line

  call getarg(1,line)
  write(*,1000) trim(line)
1000 format(a)
  
end program sfox_tx
