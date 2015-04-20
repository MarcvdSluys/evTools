!> \file plt2obs.f90  Convert code output in plt-file to observables (V, U-B, B-V, V-R, R-I)
!!                    Based on Kurucz' atmosphere models, needs UBVRI.Kur


! Copyright 2002-2015 AstroFloyd - astrofloyd.org
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


program plt2obs
  use SUFR_kinds, only: double
  use SUFR_dummy, only: dumstr9
  use constants, only: libdir
  use ubvdata, only: ubv
  
  implicit none
  integer, parameter :: nn=30000,nc=81,nff=100
  real(double) :: tm,m,mc,z,zmod,mbol,bc,mv,umb,bmv,vmr,rmi
  real(double) :: logt,logl,logr,dat(nc,nn)
  integer :: i,j,n,ncols,fnl,nf,f,io
  character :: fname*(99),fnames(nff)*(99),oname*(55),ans
  
  call setconstants()
  
  ! Read atmosphere-model data:
  open(unit=10, file=trim(libdir)//'/UBVRI.Kur',status='old',action='read',iostat=io)
  if(io.eq.0) then
     read(10,*) dumstr9
     read(10,*) dumstr9
     read(10,*) ubv
     close(10)
  else
     write(6,'(A)')" Warning:  I can't find the file "//trim(libdir)//"/UBVRI.Kur, so I can't calculate colours and magnitudes..."
  end if
  
  write(6,*)''
  
  
  z = 2.d-2
  
  
  
  
  
  !************************************************************************      
  !***   READ THE INPUT FILE
  !************************************************************************      
  
  fname = '                                                                                '
  oname = '                                                       '
  call findfiles('*.plt*',nff,1,fnames,nf)
  if(nf.eq.0) goto 9999
  
  do f=1,nf
     fname = fnames(f)
     if(fname(1:5).eq.'     ') goto 9999
     
     write(6,'(A)')'Reading file '//fname
     
     do i=99,1,-1
        fnl = i-1
        if(fname(i:i).eq.'.') exit
     end do
     oname = fname(1:fnl)//'.obs'
     
     
     dat = 0.d0
     open (unit=10,form='formatted',status='old',file=fname)
     rewind 10
     read(10,*)ncols
     write(6,'(A,I4,A)')'  Reading',ncols,' columns of data'
     if(ncols.ne.nc) write(6,'(A,I4)')'  WARNING: Number of colums in this file does not match that of the program:',nc
     do j=1,nn
        read(10,10,err=12,end=11) (dat(i,j),i=1,ncols)
10      format(F6.0,E17.9,E14.6,11F9.5,7E12.4,3F9.5,16E12.4,F8.4,21E13.5,12F9.5,6F9.5,E14.6,E12.5) !Can read upto 82 columns
     end do
     write(6,'(A)')'  End of file reached, arrays too small!'
     close(10)
     goto 15
     
11   write(6,'(A,I5,A)')'  End of the file reached,',j-1,' lines read.'
     close(10)
     goto 15
     
12   if(j.ge.3) write(6,'(A,I5)')'  Error reading file, line',j
     close(10)
     if(j.lt.3) goto 19
     write(6,'(A)')"  I'll skip the rest of the file and use the first part."
15   continue
     write(6,*)''
     
     n = j-1   !Number of models in the file
     
     
     
     goto 29
19   write(6,'(A)')'  I will try the new output format...'
     dat = 0.d0
     open (unit=20,form='formatted',status='old',file=fname)
     rewind 20
     read(20,*)ncols
     write(6,'(A,I4,A)')'  Reading',ncols,' columns of data'
     if(ncols.ne.nc) write(6,'(A,I4)')'  WARNING: Number of colums in this file does not match that of the program:',nc
     do j=1,nn
        !read(20,20,err=22,end=21) (dat(i,j),i=1,nc)
        read(20,*,err=22,end=21) (dat(i,j),i=1,nc)
!20      format(F6.0,E17.9,E14.6,12E13.5,7E12.4,3E13.5,17E12.4,39E13.5,E14.6)
     end do
     write(6,'(A)')'  End of file reached, arrays too small!'
     close(20)
     goto 25
     
21   write(6,'(A,I5,A)')'  End of the file reached,',j-1,' lines read.'
     close(20)
     goto 25
     
22   write(6,'(A,I5)')'  Error reading file, aborting at line',j
     if(j.lt.3) goto 9999
     write(6,'(A)')"  I'll skip the rest of the file and use the first part."
     close(20)
25   continue
     write(6,*)''
     
     n = j-1   !Number of models in the file
     
29   continue
     !************************************************************************      



     !Is the model-Z 0.02?
     zmod = 1.d0 - dat(42,1) - dat(43,1)
     if(abs(log10(zmod/z)).gt.1.d-2) then
        write(6,'(A)')' There seems to be a difference between the metalicity of the model and the code.'
        write(6,'(A23,F8.5,A3,F8.5,A12)', advance='no')' Should I change Z from',z,'to',zmod,'?  (y/n):  '
        read*,ans
        if(ans.eq.'y'.or.ans.eq.'Y') z = zmod
     end if


     !Calculate data and write to output
     open(30,form='formatted',file=oname,status='replace')
     do i=1,n
        tm   = dat(2,i)
        m    = dat(4,i)
        mc   = dat(5,i) !He core mass, added June 2007
        logr = dat(8,i)
        logl = dat(9,i)
        logt = dat(10,i)
        call lt2ubv(logl,logt,m,log10(z/2.d-2),mbol,bc,mv,umb,bmv,vmr,rmi)
        write(30,52)tm,m,mc,logr,logl,logt,mv,umb,bmv,vmr,rmi
52      format(ES17.9,5F9.5,2x,5F9.5)
     end do
     close(30)

  end do

9999 write(6,*)''
end program plt2obs
!***************************************************************************************************

