!Convert a .plt? file to a .ce file with selected columns for common-envelope calculations

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

program plt2ce
  use kinds
  implicit none   
  
  integer, parameter :: nff=200, n=100000,nc=89
  real(double) :: dat(nc),r,l,t,m0,r0
  integer :: i,nc1,f,nf,fl
  character :: fnames(nff)*(99),fname*(99)
  
  m0 = 1.9891d33
  r0 = 6.9599d10
  
  call findfiles('*.plt1',nff,1,fnames,nf)  !Use the first nff files
  
  do f=1,nf
     fname = fnames(f)
     do fl=len_trim(fname),1,-1
        if(fname(fl:fl).eq.'.') exit
     end do
     fl = fl-1
     
     open(unit=10,form='formatted',status='old',file=fname)
     rewind(10)
     read(10,'(I4)')nc1
     if(nc1.ne.nc) write(6,'(A,I3,A,I3,A)')'Data file has',nc1,' columns, the programme is designed for',nc,' columns !'
     open(unit=20,form='formatted',status='replace',file=fname(1:fl)//'.ce')
     
     do i=1,n
        !read(10,10,err=12,end=15) dat
!10      format(F6.0,E17.9,E14.6,12E13.5,7E12.4,3E13.5,17E12.4,39E13.5,E14.6,E13.5,F2.0,4E13.5)
        read(10,*,err=12,end=15) dat
        
        r = 10.d0**dat(8)
        l = 10.d0**dat(9)
        t = 10.d0**dat(10)
        !write 1:age, 2:M, 3:Mhe, 4:Mco, 5:R, 6:L, 7:Teff, 8:be, 9:be0, 10:be1, 11:be2, 12:be3, 13:I, 14:core He, 15:core C+O, 
        !  16:core entropy, 17:T10^5K entropy
        !BEs: 8:be: Total Binding Energy, 9:be0: Gravitational BE, 10:be1: Internal BE, 11:be2: Recombination BE, 
        !  12:be3: H2 dissociation BE.   13:I: Moment of Inertia
        write(20,20)dat(2),dat(4:6),r,l,t, dat(15)*m0,dat(84:87)*m0, dat(22)*dat(4)*m0*(r*r0)**2, dat(57),dat(58)+dat(60), &
             dat(88),dat(89)
20      format(ES17.9,16ES13.5)
     end do! i=1,n
     write(6,'(A)')'End of file not reached, arrays too small!'
     goto 15
12   write(6,'(A,I6,A)')'Error reading line',i-1,' of '//trim(fname)//', using the first part of the file only'
15   continue
     close(10)
     close(20)
  end do  !f
  
end program



