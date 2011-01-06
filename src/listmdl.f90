!> \file listmdl.f90  Lists the data contained in *.mdl[12] files


! Copyright 2002-2010 AstroFloyd - astrofloyd.org
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


!> \brief  Lists the data contained in *.mdl[12] files
program listmdl
  use constants
  implicit none
  
  integer :: nblk,blk,ans
  character :: findfile*(99),infile*(99)
  logical :: svblk
  
  call setconstants()
  
  svblk = .false.
  
  if(command_argument_count().eq.1) then
     call get_command_argument(1,infile)
  else
     infile = findfile('*.mdl*') !Search for input file in current dir
     if(len_trim(infile).le.0) call quit_program('No file found in this directory.')
  end if
     
  
  
4 continue
  call list_mdl_models(infile,nblk)
  
  
  
  !************************************************************************      
  !***   CHOOSE STRUCTURE MODEL
  !************************************************************************      
  
20 continue
  if(nblk.eq.1) then
     blk = 1 
  else  
     
     blk = 0
     do while(blk.lt.1.or.blk.gt.nblk)
        write(6,'(A50,I3,A3)', advance='no')' For which model do you want to print details (1-',nblk,'): '
        read*,blk
        if(blk.eq.0) then
           write(6,'(A,/)')'  Program finished'
           stop
        end if
     end do
     
     
  end if
  
  
  
22 continue
  call print_mdl_details(infile,blk,svblk)
  
  
  
  
  !************************************************************************      
  !***   FINISH
  !************************************************************************      
  
  ans = -1
  do while(ans.lt.0.or.ans.gt.3)
     if(nblk.eq.1) then
        write(6,'(A,/)')'  Program finished'
        stop
     end if
     
     write(6,*)''
     write(6,'(A)')' You can:'
     write(6,'(A)')'   0) Quit'
     write(6,'(A)')'   1) See another structure model'
     write(6,'(A)')'   2) List all models again'
     write(6,'(A)')'   3) Save this model'
     write(6,*)''
     write(6,'(A27)', advance='no')' What do you want to do ?  '
     
     read*,ans
  end do
  
  select case(ans)
  case(1)
     goto 20
  case(2)
     goto 4
  case(3)
     svblk = .true.
     goto 22
  end select
  
  
  
  close(10)
  write(6,'(A,/)')'  Program finished'
  
end program listmdl
!***********************************************************************************************************************************



