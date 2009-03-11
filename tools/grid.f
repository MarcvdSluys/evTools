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
  end if
  
  call getarg(1,bla)
  read(bla,*)xi
  call getarg(2,bla)
  read(bla,*)dx
  call getarg(3,bla)
  read(bla,*)n
  
  write(6,'(/,A,2ES11.3,I5)')'  Start at first value:  ',xi,dx,n
  write(6,'(A,2ES11.3,I5,/)')'  Start at second value: ',xi+dx,dx,n-1
  
  write(*,*)
  call printgrid(xi,dx,n)
  write(*,*)

  write(6,'(/,A,2ES11.3,I5)')'  Intermediate grid:     ',xi+dx/2.,dx,n-1
  write(*,*)
  call printgrid(xi+dx/2.,dx,n-1)
  write(*,*)

end program grid


!Shared with getgrid.f
subroutine printgrid(xi1,dx,n)
  implicit none
  real*8 :: xi1,xi,dx
  integer :: i,n
  
  xi = xi1
  write(6,'(A5,2A10)')'i','x','log x'
  do i=1,n
     write(6,'(I5,F10.5,ES12.3)')i,10.d0**xi,xi
     xi = xi + dx
  end do
  
end subroutine printgrid
