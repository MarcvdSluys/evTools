! getgrid.f, Find the parameters you need to get a grid with n values between x1 and x2

program getgrid      
  implicit none
  double precision :: x1,x2,dlgx
  integer :: n,iargc
  character :: bla*99
  
  if(iargc().ne.3) then
     write(*,'(/,A)')'  This program returns the parameters you need for a certain grid of N models with values between X1 and X2 in the Eggleton code'
     write(6,'(A,/)')'  Syntax:  getgrid <X1> <X2> <N>'
     stop
  end if
  
  call getarg(1,bla)
  read(bla,*)x1
  call getarg(2,bla)
  read(bla,*)x2
  call getarg(3,bla)
  read(bla,*)n
  
  dlgx = dlog10(x2/x1)/real(n-1)
  
  write(6,'(/,2ES11.3,I5,/)')dlog10(x1),dlgx,n
  
  write(*,*)
  call printgrid(dlog10(x1),dlgx,n)
  write(*,*)
  
end program getgrid



!Shared with grid.f
subroutine printgrid(xi,dx,n)
  implicit none
  real*8 :: xi,dx
  integer :: i,n
  
  write(6,'(A5,2A10)')'i','x','log x'
  do i=1,n
     write(6,'(I5,F10.5,ES12.3)')i,10.d0**xi,xi
     xi = xi + dx
  end do
  
end subroutine printgrid
