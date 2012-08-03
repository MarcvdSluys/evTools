!> \file findplt.f90  Reads .plt[12] files and displays properties of the interpolated model 
!! with a certain value for a certain variable

! cloned from findp.f
! Needs the file <libdir>/UBVRI.Kur to calculate magnitudes and colours (see below)


! Copyright 2002-2012 AstroFloyd - astrofloyd.org
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
!> \brief  Find the model with the closest value for a specified variable, interpolate, and return the values of the other variables

program findplt
  use kinds, only: double
  use constants, only: libdir
  use ubvdata, only: ubv
  
  implicit none
  integer, parameter :: nn=30000,nnn=100
  real(double) :: x(nnn),x1(nnn),xi(nnn),xfind,a,b,mbol,bc
  integer :: i,j,ncols,prmdl,succ,narg,command_argument_count,iin,iout,glt,io
  character :: fname*(99),arg*(99),tmpstr*(10)
  
  call setconstants()
  
  
  !Read atmosphere-model data
  open(unit=10, file=trim(libdir)//'/UBVRI.Kur',status='old',action='read',iostat=io)
  if(io.eq.0) then
     read(10,*)tmpstr
     read(10,*)tmpstr
     read(10,*)ubv
     close(10)
  else
     write(6,'(A)')" Warning:  I can't find the file "//trim(libdir)//"/UBVRI.Kur, so I can't calculate colours and magnitudes..."
  end if
  
  !Read the filename from the command line if any, search the current directory otherwise
  narg = command_argument_count()
  iout = 0
  if(narg.eq.3.or.narg.eq.4) then
     call get_command_argument(1,fname)
     call get_command_argument(2,arg)
     read(arg,*) iin
     call get_command_argument(3,arg)
     read(arg,*) xfind
     if(narg.eq.4) then
        call get_command_argument(4,arg)
        read(arg,*) iout
     end if
  else
     write(6,'(A)')'                                                                           ' 
     write(6,'(A)')'  Syntax:  FINDPLT  <file.plt> <variable> <value> [<output>]'
     write(6,'(A)')'           findplt finds every instant where <variable> becomes <value>, using interpolation'
     write(6,'(A)')'           <output>: -1: all variables, 0: (default) selection of variables, >0: variable number <output>'
     write(6,'(A)')'  '
     write(6,'(A)')'  <variable>:                                                              '
     write(6,'(A)')'    1: model        16: Lh           28: Porb        34: Horb              '
     write(6,'(A)')'    2: t            17: Lhe          29: FLR         35: dHorb/dt          '
     write(6,'(A)')'    3: dt           18: Lc           30: F1          36: dHgw/dt           '
     write(6,'(A)')'    4: M            19: Lnu          31: dM          37: dHwml/dt          '
     write(6,'(A)')'    5: Mhe          20: Lth          32: dMwind      38: dHmb/dt           '
     write(6,'(A)')'    6: Mco          21: Prot         33: dMmt        39: dHmtr/dt          '
     write(6,'(A)')'    7: Mone         22: VK2                          40: Mcomp             '
     write(6,'(A)')'    8: log R        23: Rcz                          41: e                 '
     write(6,'(A)')'    9: log L        24: dRcz                                               '
     write(6,'(A)')'   10: log Teff     25: Tet                                                '
     write(6,'(A)')'   11: log Tc       26: Ralv                                               '
     write(6,'(A)')'   12: log Tmax     27: Bp                 H  He   C   N   O  Ne  Mg       '
     write(6,'(A)')'   13: log Rhoc                    Surf:  42  43  44  45  46  47  48       '
     write(6,'(A)')'   14: log RhoTm                   Tmax:  49  50  51  52  53  54  55       '
     write(6,'(A)')'   15: Ub,env                      Core:  56  57  58  59  60  61  62       '
     write(6,'(A)')'                                                                           ' 
     write(6,'(A)')'   91: V   92: U-B   93: B-V   94: V-I   95: I-R   96: U-V   97: V-R       '
     write(6,'(A)')'   99: Zsurf                                                               '
     write(6,'(A)')'                                                                           '
     write(6,'(A)')'  <value>: value to find for <variable>                                    '
     write(6,'(A)')'                                                                           ' 
     
     stop
  end if
  
  if(fname(1:3).eq.'   ') stop
  
  open(unit=10,form='formatted',status='old',file=fname)
  rewind 10
  read(10,*)ncols
  
  !print*,ncols,'columns'
  
  succ = 0
  glt = 1            ! glt determines whether we search the first model where:  x(iin) < xfind (glt=1),   or  x(iin) > xfind (glt=2)
  
  do j=1,nn
     !read(10,10,err=11,end=15) x(1:ncols)
     !10   format(F6.0,E17.9,E14.6,11F9.5,7E12.4,3F9.5,16E12.4,F8.4,21E13.5,12F9.5,6F9.5,E14.6,E12.5) !Can read upto 82 columns
!10   format(F6.0,E17.9,E14.6,11F9.5,7E12.4,3F9.5,16E12.4,F8.4,21E13.5,12F9.5,6F9.5,E14.6)
     read(10,*,err=11,end=15) x(1:ncols)
     
     !Calculate colours/magnitude
     x(99) = 1.d0 - x(42) - x(43)                       !Z_surf = 1 - X - Y
     call lt2ubv(x(9),x(10),x(4),log10(x(99)/2.d-2),mbol,bc,x(91),x(92),x(93),x(94),x(95))
     x(96) = x(92)+x(93)  ! (U-V) = (U-B) + (B-V)
     x(97) = x(94)+x(95)  ! (V-I) = (V-R) + (R-I)
     
     if(j.eq.1.and.x(iin).lt.xfind) glt = 2
     
     if(j.gt.1) then
        if((glt.eq.1.and.x(iin).lt.xfind.and.x1(iin).gt.xfind).or.   &   !this is the first model where x < xfind
             (glt.eq.2.and.x(iin).gt.xfind.and.x1(iin).lt.xfind)) then   !this is the first model where x > xfind
           do i=1,nnn
              a     = (x(i)-x1(i))/(x(iin)-x1(iin))  !Interpolate all variables x(1-81), put them in xi
              b     = x1(i) - a*x1(iin)
              xi(i) = a*xfind + b
           end do
           succ = 1
           glt = 3-glt  !Change 1 <==> 2
           
           !Print (some of) the interpolated values
           call printmodel(nnn,xi,iin,iout)
           
        end if !if((glt.eq.1.and.x(iin).lt.xfind.and.x1(iin).gt.xfind).or.(glt.eq.2.and.x(iin).gt.xfind.and.x1(iin).lt.xfind)) then
     end if !if(j.gt.1) then
     
     prmdl = nint(x(1))
     x1 = x             !For the next loop: the previous value for x
  end do !j=1,nn
  goto 15
  
11 write(6,*)''
  print*,'  Error reading file after line ',j-1,', model ',prmdl,'!'
  write(6,'(A)')'  Only the first part of the file will be processed.'
  write(6,*)''
15 close(10)
  
  if(succ.eq.0) then
     !write(6,'(A)')' Value not found, aborting... '
     !stop
     write(6,*)''
     write(6,'(A)')' *** Value not found, printing last model ***'
     call printmodel(nnn,x,iin,iout)
     stop
  end if
  
  
  if(1.eq.2) write(6,'(ES12.5)')x(2)
  
  
end program findplt
!***********************************************************************************************************************************




!***********************************************************************************************************************************
!> \brief  Prints a selected (interpolated) model
!!
!! \param n     Size of data array
!! \param xx    Data array
!! \param iin   Variable number to print if iout>0
!! \param iout  0: print standard selection, <0: print all variables, >0: print variables iin and iout

subroutine printmodel(n, xx, iin, iout)
  use kinds, only: double
  use constants, only: m0
  
  implicit none
  integer, intent(in) :: n,iin,iout
  real(double), intent(in) :: xx(n)
  real(double) :: x(n)
  integer :: i
  
  x = xx
  
  x(8)  = 10.d0**x(8)   ! logR -> R
  x(9)  = 10.d0**x(9)   ! logL -> L
  x(10) = 10.d0**x(10)  ! logT -> Teff
  
  x(15) = x(15)*m0
  
  if(iout.eq.0) then
     write(6,*)''
     write(6,*)''
     
     write(6,'(A10,5x,A10,A12,3A9,5A11)') 'General:','Mdl','t (Gyr)','M (Mo)','Mhe (Mo)','Mco (Mo)','R','L','Teff','Ubind','dM/dt'
     write(6,'(15x,f10.3,es12.5,3f9.4,5es11.3)') x((/1,2, 4,5,6, 8,9,10, 15,33/))
     
     write(6,'(A10,5x,5A10,4A6)') 'Surface:','H','He','C','N','O','V','B-V','V-I','U-V'
     write(6,'(15x,5ES10.3,4F6.2)') x((/42,43,44,45,46,91,93,94,96/))
     
     write(6,'(A10,5x,5A10,3A8,A10)') 'Core:','H','He','C','N','O','log Tc','log rho','Mhe'
     write(6,'(15x,5es10.3,3f8.4)') x((/56,57,58,59,60,11,13,5/))
     write(6,*)''
  else if(iout.gt.0) then
     write(6,'(70x,1p,2G12.3)') x(iin),x(iout)
  else !iout.lt.0
     do i=1,n
        write(6,'(I6,1p,G12.3)') i,x(i)
     end do
  end if
  
  
  if(1.eq.2) then 
     write(6,*)''
     write(6,'(F8.4,ES12.5)', advance='no') x((/4,2/))
  end if
  
end subroutine printmodel
!***********************************************************************************************************************************


