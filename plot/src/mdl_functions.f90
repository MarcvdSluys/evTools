!> \file mdl_functions.f90  Routines to help plot the data contained in mdl* files

! AF, 21-08-2010

!   Copyright 2002-2010 AstroFloyd - astrofloyd.org
!   
!   
!   This file is part of the eggleton-plot package.
!   
!   This is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published
!   by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
!   
!   This software is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
!   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details.
!   
!   You should have received a copy of the GNU General Public License along with this code.  If not, see 
!   <http://www.gnu.org/licenses/>.





!***********************************************************************************************************************************
!> \brief  Find the model closest to a selected point in a graph
subroutine identify_closest_model(nn,ny,xx,yy,xmin,xmax,ymin,ymax)
  implicit none
  integer, intent(in) :: nn,ny!,nq
  real, intent(in) :: xx(nn),yy(10,nn)  
  !real, intent(in) :: dat(nq,nn)
  
  integer :: iy,iy0,i,i0,nsel,col
  real :: dist,mindist
  real :: xmin,xmax,ymin,ymax,dx,dy,xsel(4),ysel(4)
  !real :: d(nq)
  
  character :: hlbls*5
  
  !Identify closest model
  xsel = 0.
  ysel = 0.
  write(6,'(A)')' Select a point in the graph and press "x" to finish'
  nsel=0
  call pgsci(1)
  call pgolin(1,nsel,xsel,ysel,2)
  
  iy0 = 1
  
  
  dx = abs(xmax-xmin)
  dy = abs(ymax-ymin)
  mindist = huge(mindist)
  do iy=1,ny
     do i=1,nn
        dist = (abs(xsel(1)-xx(i))/dx)**2 + (abs(ysel(1)-yy(iy,i))/dy)**2
        if(dist.lt.mindist) then
           i0 = i
           iy0 = iy
           mindist = dist
        end if
     end do
  end do
  write(6,*)''
  write(6,'(A,ES12.4,A,ES12.4)')          ' Selected point:    x =',xsel(1),',  y =',ysel(1)
  write(6,'(A,ES12.4,A,ES12.4,A,I5)')' Closest model:     x =',xx(i0),',  y =',yy(iy0,i0),  &
       ',  model =',i0
  
  !Copied from plotplt:
  !dx = 0
  !dy = 0
  !if(i0.gt.1.and.i0.lt.nn) then
  !   dx = xx(i0+1)-xx(i0-1)
  !   dy = yy(iy0,i0+1)-yy(iy0,i0-1)
  !else if(i0.gt.1) then
  !   dx = xx(i0)-xx(i0-1)
  !   dy = yy(iy0,i0)-yy(iy0,i0-1)
  !else if(i0.lt.nn) then
  !   dx = xx(i0+1)-xx(i0)
  !   dy = yy(iy0,i0+1)-yy(iy0,i0)
  !end if
  !
  !write(6,'(3(A,ES12.4))')' Derivative:       dx =',dx,', dy =',dy,',  dy/dx =',dy/dx
  !
  !write(6,*)''
  !!From listiyt
  !write(6,'(A)')' Line   Mdl     t (yr)   M(Mo)   Mhe   Mco   Menv    R (Ro)   L (Lo)    Te (K)   Tc (K)'//  &
  !     '       V    B-V     Xc    Yc   Porb(d)     dM/dt  M2/Mo'
  !d = dat(:,i0)
  !write(6,'(I5,I6,ES11.4,F8.3,2F6.3,F7.3,2(1x,2ES9.2),1x,2F7.3,1x,2F6.3,2ES10.2,F7.3)')i0+1,nint(d(1)),d(2),d(4),d(5),d(6),  &
  !     d(63),d(8),d(9),d(10),d(11),d(101),d(103),d(56),d(57),d(28),abs(d(31)),d(40)
  !write(6,*)''
  
  col = 2
  !col = colours(mod(iy0-1,ncolours)+1)  !2,3,...,ncolours,1,2,...
  call pgsci(col)
  
  call pgpoint(1,xx(i0),yy(iy0,i0),2)
  write(hlbls,'(I5)')i0
  call pgptxt(xx(i0),yy(iy0,i0),0.,0.,hlbls)
  
  call pgsci(1)
  
end subroutine identify_closest_model
!***********************************************************************************************************************************
