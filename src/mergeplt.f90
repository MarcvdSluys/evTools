!> \file mergeplt.f90  Merges the data contained in two .plt-files
!!
!! AF 21-01-2004


!  Copyright 2002-2011 AstroFloyd - astrofloyd.org
!  
!  
!  This file is part of the evTools package.
!  
!  This is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by
!  the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
!  
!  This software is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details.
!  
!  You should have received a copy of the GNU General Public License along with this code.  If not, see 
!  <http://www.gnu.org/licenses/>.



!***********************************************************************************************************************************
!> \brief  This program merges data from the plt output file from ev

program mergeplt      
  use kinds
  
  implicit none
  integer, parameter :: nn=30000,nnn=100
  real(double) :: dat1(nnn,nn),dat2(nnn,nn)
  !integer :: model1(nn),model2(nn)
  integer :: narg,command_argument_count
  
  integer :: i,j,n1,n2,ncols,ncols1,ncols2,ncolsmax
  character :: fin1*(99),fin2*(99),fout*(99)
  
  
  ncolsmax = 89  !Program is not designed to handle more columns
  
  
  narg = command_argument_count()
  if(narg.eq.3) then
     call get_command_argument(1,fin1)
     call get_command_argument(2,fin2)
     call get_command_argument(3,fout)
  else if(narg.eq.2) then
     call get_command_argument(1,fin1)
     call get_command_argument(2,fin2)
     fout = 'merged.plt1'
     write(6,'(A)')'  No output file name specified, using merged.plt1'
  else
     write(*,'(A)')'  mergeplt: merges the contents of two plot files to a third file'
     write(*,'(A)')'            syntax:  mergeplt <infile1> <infile2> <outfile>'
     goto 9999
  end if
  
  write(*,*)''
  write(*,'(A)')'  Reading file '//trim(fin1)
  open (unit=10,form='formatted',status='old',file=trim(fin1))
  rewind 10
  read(10,*)ncols1
  if(ncols1.gt.nnn+1) then
     write(*,'(A)')'  The array DAT is too small for the number of columns'
     write(*,'(A)')'  Aborting...'
     goto 9999
  end if
  do j=1,nn
     read(10,*,err=12,end=11) (dat1(i,j),i=1,ncols1)
  end do
  write(*,'(A)')'  End of file reached, arrays too small!'
  close(10)
  goto 15
  
11 write(*,'(A,I6,A,I6,A,I6)')'  End of the file reached,',j-1,' lines read: model',nint(dat1(1,1)),' - ',nint(dat1(1,j-1))
  write(*,'(A8,ES14.7,A3,ES14.7)')'  time: ', dat1(1,1),' - ',dat1(1,j-1)
  close(10)
  goto 15
  
12 write(*,*)'  File unreadable after line ',j-1
  write(*,'(A)')'  Make sure files overlap !!!'
  write(*,*)''
  write(*,'(A,I6,A,I6,A,I6)')'  ',j-1,' lines read: model', nint(dat1(1,1)),' - ',nint(dat1(1,j-1))
  write(*,'(A8,ES14.7,A3,ES14.7)')'  time: ', dat1(1,1),' - ',dat1(1,j-1)
  close(10)
  
  
15 n1=j-1
  
  
  write(*,*)''
  
  
  write(*,'(A)')'  Reading file '//trim(fin2)
  open (unit=20,form='formatted',status='old',file=trim(fin2))
  rewind 20
  read(20,*)ncols2
  ncols = ncols1
  if(ncols2.ne.ncols1) then
     write(*,'(A,I4,A,I4)')'  The two files have different numbers of columns:',ncols1,' vs.',ncols2    
     ncols = min(ncols1,ncols2)
     write(*,'(A,I4)')'  Continuing with the minimum of the two:',ncols  !This can happen because I write 81 cols max.
  end if
  do j=1,nn
     read(20,*,err=22,end=21) (dat2(i,j),i=1,ncols2)
  end do
  write(*,'(A)')'  End of file reached, arrays too small!'
  close(20)
  goto 25
  
21 write(*,'(A,I6,A,I6,A,I6)')'  End of the file reached,',j-1,' lines read: model',nint(dat2(1,1)),' - ',nint(dat2(1,j-1))
  write(*,'(A8,ES14.7,A3,ES14.7)')'  time: ', dat2(1,1),' - ',dat2(1,j-1)
  close(10)
  goto 25
  
22 write(*,*)'  File unreadable after line ',j-1
  write(*,'(A)')'  Continuing process, check the result !!!'
  write(*,*)''
  write(*,'(A,I6,A,I6,A,I6)')'  ',j-1,' lines read: model', nint(dat2(1,1)),' - ',nint(dat2(1,j-1))
  write(*,'(A8,ES14.7,A3,ES14.7)')'  time: ', dat2(1,1),' - ',dat2(1,j-1)
  close(20)
  
25 n2=j-1
  
  write(*,*)''
  
  if(ncols.gt.ncolsmax) then
     write(*,'(A,I4,A,I4,A)')'  The number of columns is larger than', ncolsmax,', I can only save the first', ncolsmax,' !!!'
     ncols = ncolsmax
  end if
  
  open(unit=30,form='formatted',status='new',file=trim(fout),iostat=i)
  if(i.ne.0) then
     write(*,'(A)')'  File already exists: '//trim(fout)
     write(*,'(A)')'  Aborting...'
     goto 9999
  end if
  write(*,'(A)')'  Creating output file: '//trim(fout)
  write(30,'(I4)')ncols
  do j=1,n1
     if(dat1(1,j).ge.dat2(1,1)) goto 28 !Use time rather than model number
     if(ncols.le.81) write(30,2003) nint(dat1(1,j)), (dat1(i,j),i=2,ncols)  !v.2003
     if(ncols.gt.81) write(30,2005) nint(dat1(1,j)), (dat1(i,j),i=2,ncols)  !v.2005
  end do
  
28 do j=1,n2
     if(ncols.le.81) write(30,2003) nint(dat2(1,j)), (dat2(i,j),i=2,ncols)  !v.2003
     if(ncols.gt.81) write(30,2005) nint(dat2(1,j)), (dat2(i,j),i=2,ncols)  !v.2005
  end do
  close(30)
  
2003 format(I6,ES17.9,ES14.6,11F9.5,7ES12.4,3F9.5,16ES12.4,F8.4,21ES13.5,12F9.5,6F9.5,ES14.6) !v.2003, 81 colums
2005 format(I6,ES17.9,ES14.6,12ES13.5,7ES12.4,3ES13.5,16ES12.4, 39ES13.5,ES14.6,ES13.5,F5.1,6ES13.5) !v.2005, 89 colums
  
  
  
  
  
  write(*,'(A)')'  Program finished'
9999 write(*,*)''
  
end program mergeplt
!***********************************************************************************************************************************

