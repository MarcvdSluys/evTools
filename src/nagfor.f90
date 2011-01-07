!> \file nagfor.f90  Provide some redirection/dummy routines for NAG Fortran


! Copyright 2002-2011 AstroFloyd - astrofloyd.org
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




!> \brief  Provide a dummy system() command to allow compilation with NAG Fortran
function system(str)
   implicit none
   character, intent(in) :: str*(*)
   integer :: system
   character :: dummystr*99
   dummystr = trim(str)
   system = 0
end function system


!> \brief  Provide a dummy sleep() command to allow compilation with NAG Fortran
subroutine sleep(nr)
   implicit none
   integer, intent(in) :: nr
   integer :: dummyint
   dummyint = nr
end subroutine sleep

