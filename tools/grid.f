! grid.f, see what a log grid of models in Eggletons code will result in

program grid      
  implicit none
  real*8 :: xi,dx
  integer :: n,iargc
  character :: bla*99
  
  if(iargc().ne.3) then
     write(*,'(/,A)')'  This program shows what values are used in a grid of models for the Eggleton code with specified grid settings'
     write(6,'(A,/)')'  syntax:  grid <Xi, dX, n> '
     stop
     !read*,xi,dx,n
  end if
  
  call getarg(1,bla)
  read(bla,*)xi
  call getarg(2,bla)
  read(bla,*)dx
  call getarg(3,bla)
  read(bla,*)n
  
  call printgrid(xi,dx,n)
  write(*,*)
end program grid


!Shared with getgrid.f
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
