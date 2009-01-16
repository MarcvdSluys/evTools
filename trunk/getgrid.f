! getgrid.f, Find the parameters you need to get a grid with n values between x1 and x2

program getgrid      
  implicit none
  double precision :: x1,x2,dlgx
  integer :: i,n
  
  write(*,'(/,A)')'  This program calculates what parameters you need for a certain grid of N models with values between X1 and X2 in Eggletons code.'
  write(6,'(A,$)')'   X1, X2, n: '
  read*,x1,x2,n
  
  dlgx = dlog10(x2/x1)/real(n-1)
  
  write(6,'(2x,2ES11.3,I5,/)')dlog10(x1),dlgx,n
end program getgrid
