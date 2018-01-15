!> \file getgrid.f90  Find the parameters you need to get a grid with n values between x1 and x2


! Copyright 2002-2018 AstroFloyd - astrofloyd.org
! 
! 
! This file is part of the evTools package.
! 
! This is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by
! the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
! 
! This software is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details.
! 
! You should have received a copy of the GNU General Public License along with this code.
! If not, see <http://www.gnu.org/licenses/>.


!***********************************************************************************************************************************
!> \brief  Find the parameters you need to get a grid with n values between x1 and x2

program getgrid
  use SUFR_kinds, only: double
  
  implicit none
  real(double) :: x1,x2,dlgx
  integer :: n,command_argument_count
  character :: bla*(99)
  
  if(command_argument_count().ne.3) then
     write(*,'(/,A)')'  This program returns the parameters you need for a certain grid of N models with values between'// &
          ' X1 and X2 in ev'
     write(6,'(A,/)')'  Syntax:  getgrid <X1> <X2> <N>'
     stop
  end if
  
  call get_command_argument(1,bla)
  read(bla,*)x1
  call get_command_argument(2,bla)
  read(bla,*)x2
  call get_command_argument(3,bla)
  read(bla,*)n
  
  dlgx = log10(x2/x1)/real(n-1)
  
  write(6,'(/,A,2ES11.3,I5)')'  Start at first value:  ',log10(x1),dlgx,n
  write(6,'(A,2ES11.3,I5,/)')'  Start at second value: ',log10(x1)+dlgx,dlgx,n-1
  
  write(*,*)
  call printgrid(log10(x1),dlgx,n)
  write(*,*)
  
  write(6,'(/,A,2ES11.3,I5)')'  Intermediate grid:     ',log10(x1)+dlgx/2.,dlgx,n-1
  write(*,*)
  call printgrid(log10(x1)+dlgx/2.,dlgx,n-1)
  write(*,*)
  
end program getgrid
!***********************************************************************************************************************************



!***********************************************************************************************************************************
!> \brief  Print the masses, mass ratios or periods in a grid
!!
!! \param xi1  Initial value for log M/q/P
!! \param dx   Increment of M/q/P
!! \param n    Number of steps in grid
!!
!! \note  Shared with grid.f90

subroutine printgrid(xi1,dx,n)
  use SUFR_kinds, only: double
  
  implicit none
  real(double), intent(in) :: xi1,dx
  integer, intent(in) :: n
  real(double) :: xi
  integer :: i
  
  xi = xi1
  write(6,'(A5,2A10)')'i','x','log x'
  do i=1,n
     write(6,'(I5,F10.5,ES12.3)')i,10.d0**xi,xi
     xi = xi + dx
  end do
  
end subroutine printgrid
!***********************************************************************************************************************************
