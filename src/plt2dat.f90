!Convert a .plt? file to a .dat file with selected columns
!
!   Copyright 2002-2010 AstroFloyd - astrofloyd.org
!   
!   
!   This file is part of the twin-tools package.
!   
!   This is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by
!   the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
!   
!   This software is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
!   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details.
!   
!   You should have received a copy of the GNU General Public License along with this code.  If not, see <http://www.gnu.org/licenses/>.


program plt2dat
  use constants
  implicit none   
  integer, parameter :: nff=200, n=100000,nc=89
  integer :: i,j,nc1,f,nf,fl,sel(99),nsel,io1,io2
  real*8 :: dat(nc)
  character :: fnames(nff)*99,infile*99,outfile*99
  
  call setconstants()
  
  !Give the columns of the plt file which you want to output:
  !nsel = 7       !t,M,R,L,Teff,M_He,X_c
  !sel(1:nsel) = (/2,4,8,9,10,  5,   56/)
  
  !For Lennart (May 2010):
  !nsel = 15      !t,M,Mhe,R,L,Teff,Po,Mtr, Xc,Xs,Ys,Cs,Ns,Os,Qcnv
  !sel(1:nsel) = (/2,4,  5,8,9,  10,28, 33, 56,42,43,44,45,46,  81/)
  
  !Simple version for Lennart (May 2010):
  nsel = 4      ! i,t,Po,Mtr
  sel(1:nsel) = (/1,2,28, 33/)
  
  
  !Use the first nff files of *.plt1, *.plt or *.plt2:
  call findfiles('*.plt1',nff,1,fnames,nf)
  if(nf.eq.0) call findfiles('*.plt',nff,1,fnames,nf)
  if(nf.eq.0) call findfiles('*.plt2',nff,1,fnames,nf)
  
  
  do f=1,nf
     infile = fnames(f)
     do fl=len_trim(infile),1,-1
        if(infile(fl:fl).eq.'.') exit
     end do
     fl = fl-1
     outfile = infile(1:fl)//'.dat'
     
     
     open(unit=10,form='formatted',status='old',file=trim(infile),iostat=io1)
     if(io1.ne.0) then
        write(0,'(A,/)')'  Error opening '//trim(infile)//', aborting...'
        stop
     end if
     rewind(10)
     read(10,'(I4)')nc1
     if(nc1.ne.nc) write(6,'(A,I3,A,I3,A)')'  Warning: this plt file has',nc1,' columns, the programme is designed for', &
          nc,' columns !'
     
     open(unit=20,form='formatted',status='replace',file=trim(outfile),iostat=io2)
     if(io2.ne.0) then
        write(0,'(A,/)')'  Error opening '//trim(outfile)//', aborting...'
        stop
     end if
     
     do i=1,n
        !Read data:
        !read(10,'(F6.0,E17.9,E14.6,12E13.5,7E12.4,3E13.5,17E12.4,39E13.5,E14.6,E13.5,F2.0,4E13.5)',err=12,end=15) dat
        read(10,*,iostat=io1) dat
        if(io1.lt.0) exit  !EOF
        if(io1.gt.0) then
           write(0,'(A,I4,A,/)')'  Error reading '//trim(infile)//', line',i,' aborting...'
           stop
        end if
        
        
        !Modify variables:
        dat(8:10) = 10.d0**dat(8:10)  !Un-log R,L,Teff
        
        
        !Write selected variables:
        do j=1,nsel
           if(sel(j).eq.1) then
              write(20,'(I6,$)',iostat=io2)nint(dat(1))
           else
              write(20,'(ES17.9,$)',iostat=io2)dat(sel(j))
           end if
           if(io2.gt.0) then
              write(0,'(A,I4,A,/)')'  Error writing to '//trim(outfile)//', line',i,' aborting...'
              stop
           end if
        end do
        write(20,'(A)')''
        
        
     end do! i=1,n
     
     
     if(i.ge.n) write(0,'(A)')'End of file not reached, arrays too small!'
     
     
     close(10)
     close(20)
     
  end do  !f
  
end program plt2dat



