subroutine getpp3(s3,workdat,p)

  use sfox_mod
  real s3(NQ,NN)
  integer workdat(NN)
  integer a(NN)

!  a(1:NN)=workdat(NN:1:-1)
  a=workdat

  psum=0.
  do j=1,NN
     i=a(j)+1
     x=s3(i,j)
     s3(i,j)=0.
     psum=psum + x
     s3(i,j)=x
  enddo
  p=psum/NN

  return
end subroutine getpp3
