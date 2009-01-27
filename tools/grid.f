! grid.f, see what a log grid of models in Eggletons code will result in

program grid      
  implicit none
  double precision :: xi,dx,x10
  integer :: i,n
  character :: ans
  
  write(*,'(/,A)')'  This program shows what values are used in a grid of models for Eggletons code.'
  write(6,'(A,$)')'   Xi, dX, n: '      
  read*,xi,dx,n
  
  
  write(6,'(A5,2A10)')'i','x','log x'
  do i=1,n
     write(6,'(I5,F10.5,ES12.3)')i,10.d0**xi,xi
     xi = xi + dx
  end do
  
  write(*,*)
end program grid
