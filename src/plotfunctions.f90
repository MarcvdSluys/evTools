!>  \file plotfunctions.f90  Functions and subroutines for the evTools package that need pgplot

! Copyright 2002-2010 AstroFloyd - astrofloyd.org
! 
! 
! This file is part of the evTools package.
! 
! This is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published
! by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
! 
! This software is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details.
! 
! You should have received a copy of the GNU General Public License along with this code.  If not, see 
! <http://www.gnu.org/licenses/>.



!***********************************************************************
!> \brief Plot lines of constant R on HRD (plotplt*)
subroutine plotlinesofconstantradius(xmin,xmax,ymin,ymax)
  use constants
  implicit none
  integer :: logri
  real :: x,xmin,xmax,y,ymin,ymax,logr,dlogr,cst
  real :: r1,r2,dr,x2(2),y2(2)
  character :: str*99
  
  call pgsls(4)
  cst = log10(real(4*pi*sigma * r0**2/l0))
  dlogr = 1.
  
  r1 = (ymin - cst - 4*xmin)/2.  !logR in lower-left  corner
  r2 = (ymax - cst - 4*xmax)/2.  !logR in upper-right corner
  dr = r2-r1
  if(floor(dr).lt.4) dlogr = dlogr/2.
  
  do logri=nint(-10./dlogr),nint(10./dlogr)  !0=1Ro, 1=10Ro, etc
     logr = real(logri)*dlogr
     if(logr.lt.r1.or.logr.gt.r2) cycle
     if(logr.lt.0.0001) then
        if(logr.lt.-2.0) then
           write(str,'(F6.3,A)')10**logr,'R\d\(2281)\u'
        else
           write(str,'(F5.2,A)')10**logr,'R\d\(2281)\u'
        end if
     else
        write(str,'(I5,A)')nint(10**logr),'R\d\(2281)\u'
     end if
     x2 = (/2.,6./)
     y2 = cst+2*logr+4*(/2.,6./)
     call pgline(2,x2,y2)
     x = xmin - (xmax-xmin)*0.015
     y = cst+2*logr+4*xmin
     if(y.gt.ymax) then
        x = (ymax - (cst+2*logr))*0.25
        y = ymax + (ymax-ymin)*0.01
     end if
     if(logr.ge.-3.and.logr.lt.4. .and. logr.gt.r1+0.5*dlogr.and.logr.lt.r2-0.5*dlogr) call pgptxt(x,y,0.,0.,trim(str))
  end do
  call pgsls(1)
end subroutine plotlinesofconstantradius
!***********************************************************************



!***********************************************************************
!> \brief Plot the zams; use <libdir>/zams_z0_02.plt
!! It would be nice to not only plot a HRD, but any parameter
!! \todo Write a routine to read a plt file first, then use it here
subroutine plotzams()
  implicit none
  
end subroutine plotzams
!***********************************************************************











!***********************************************************************
!> \brief Make a convection plot from the data in a *.plt? file
subroutine pltconvection(nmax,nvar,n, dat,vx,ymin,ymax, nhp,hp,hlp,hlbl)
  use kinds
  implicit none
  integer :: nmax,nvar,n, nhp,hp(1000),vx
  integer :: i,j,ci0,lw0
  integer :: plconv,plsmcv,plnuc,plcb,ib,ibold,nz,dib,ch
  real(double) :: dat(nvar,nmax)
  real :: xx(nmax),xx2(2),y(nmax),yy2(2),zonex(4),zoney(3,4),zoney1(4),zoney2(2),dat1(nmax)
  real :: ymin,ymax,ch0
  character :: hlp,hlbl
  character :: hlbls*5
  
  
  plconv = 1  !Plot convection
  plsmcv = 1  !Plot semiconvection
  plnuc  = 1  !Plot nuclear burning regions
  plcb   = 1  !Plot core boundaries
  
  !Get original styles and colours
  call pgqci(ci0)
  call pgqlw(lw0)
  call pgqch(ch0)
  
  
  call pgslw(3)
  xx(1:n) = dat(vx,1:n)
  y(1:n) = dat(4,1:n)
  call pgline(n,xx(1:n),y(1:n))
  
  ch = 1 !Plot hatches
  if(ch.eq.1) then
     call pgsls(4)
     call pgslw(1)
     call pgsfs(1)
     
     
     !*** Convection ***
     if(plconv.eq.1) then
        call pgslw(3)
        call pgsci(14)
        ibold = 0
        do i=2,n
           ib = 68
           do j=68,62,-1
              ib = j
              if(dat(j,i).lt.0.d0) exit
           end do
           dib = ib-ibold
           
           zonex = 0.
           zoney = 0.
           nz = 1
           zonex = real((/xx(i-1),xx(i-1),xx(i),xx(i)/))
           if(ib.eq.63.or.ib.eq.65.or.ib.eq.67) then
              zoney(1,:)                          = abs(real((/0.d0,dat(63,i-1),dat(63,i),0.d0/)))
              if(ib.eq.65.or.ib.eq.67) zoney(2,:) = abs(real((/dat(64,i-1),dat(65,i-1),dat(65,i),dat(64,i)/)))
              if(ib.eq.67) zoney(3,:)             = abs(real((/dat(66,i-1),dat(67,i-1),dat(67,i),dat(66,i)/)))
              nz = (ib-61)/2
           end if
           if(ib.eq.62.or.ib.eq.64.or.ib.eq.66.or.ib.eq.68) then
              zoney(1,:) = abs(real((/dat(63,i-1),dat(64,i-1),dat(64,i),dat(63,i)/)))
              zoney(2,:) = abs(real((/dat(65,i-1),dat(66,i-1),dat(66,i),dat(65,i)/)))
              zoney(3,:) = abs(real((/dat(67,i-1),dat(68,i-1),dat(68,i),dat(67,i)/)))
              nz = (ib-62)/2
              if(nz.eq.0.and.dib.ne.0) nz = 1
           end if
           
           if((dib.eq.1.or.dib.eq.3).and.i.gt.2) then  !'Glue' the jumps
              do j=1,abs(dib)
                 zoney(3,1:2) = (/zoney(2,2),zoney(3,1)/)
                 zoney(2,1:2) = (/zoney(1,2),zoney(2,1)/)
                 zoney(1,1:2) = (/0.,zoney(1,1)/)
              end do
           end if
           
           if((dib.eq.-1.or.dib.eq.-3).and.i.gt.2) then  !'Glue' the jumps
              do j=1,abs(dib)
                 zoney(1,1:2) = (/zoney(1,2),zoney(2,1)/)
                 zoney(2,1:2) = (/zoney(2,2),zoney(3,1)/)
                 zoney(3,1:2) = (/zoney(3,2),zoney(3,2)/)
              end do
           end if
           
           do j=1,nz
              !Zeroes or change in the number of zones can mean trouble:
              if(zoney(j,1)*zoney(j,2)*zoney(j,3)*zoney(j,4).lt.1.e-8.or.dib.ne.0) then
                 if((zoney(j,1)+zoney(j,2))/dat(4,i).lt.1.e-5.and.zoney(j,3)*zoney(j,4).gt.1.e-8) then
                    zoney(j,1) = (zoney(j,3)+zoney(j,4))/2.  !Zone begins, make it end nicely in a point at the left
                    zoney(j,2) = (zoney(j,3)+zoney(j,4))/2.
                 end if
                 if((zoney(j,3)+zoney(j,4))/dat(4,i).lt.1.e-5.and.zoney(j,1)*zoney(j,2).gt.1.e-8) then
                    zoney(j,3) = (zoney(j,1)+zoney(j,2))/2.  !Zone ends, make it end nicely in a point at the right
                    zoney(j,4) = (zoney(j,1)+zoney(j,2))/2.
                 end if
                 if(zoney(j,2).lt.1.e-10.and.zoney(j,1)*zoney(j,3)*zoney(j,4).gt.1.e-10) zoney(j,2) = zoney(j,3)
                 if(zoney(j,3).lt.1.e-10.and.zoney(j,1)*zoney(j,2)*zoney(j,4).gt.1.e-10) zoney(j,3) = zoney(j,2)
              end if
           end do
           
           !Keep this for testing
           !if(xx(i).gt.1245.and.xx(i).lt.1265) then
           !write(6,*)''
           !!if(dib.ne.0) then
           !write(6,'(4I5,6F8.4)')i-1,nint(xx(i-1)),ibold,dib,real(dat(63:68,i-1))
           !write(6,'(4I5,6F8.4)')i,nint(xx(i)),ib,dib,real(dat(63:68,i))
           !write(6,'(5I10)')nint(zonex),nz
           !do j=1,nz
           !write(6,'(4F10.6)')zoney(j,:)
           !end do
           !end if
           
           
           do j=1,nz
              zoney1(1:4) = zoney(j,1:4)
              if(zoney(j,1)+zoney(j,2)+zoney(j,3)+zoney(j,4).gt.1.e-8) call pgpoly(4,zonex,zoney1(1:4))
           end do
           ibold = ib
        end do !do i=2,n
     end if !If plconv.eq.1
     
     
     
     
     !*** Semiconvection ***  NOTICE that dat(69:74) was 'cleaned' after reading the file
     if(plsmcv.eq.1) then
        call pgsfs(1)
        call pgsls(1)
        call pgslw(13)
        call pgsci(15)
        ibold = 0
        do i=2,n
           ib = 74
           do j=74,68,-1
              ib = j
              if(dat(j,i).lt.0.d0) exit
           end do
           dib = ib-ibold
           
           zonex = 0.
           zoney = 0.
           nz = 1
           zonex = real((/xx(i-1),xx(i-1),xx(i),xx(i)/))
           if(ib.eq.69.or.ib.eq.71.or.ib.eq.73) then
              zoney(1,:)                          = abs(real((/0.d0,dat(69,i-1),dat(69,i),0.d0/)))
              if(ib.eq.71.or.ib.eq.73) zoney(2,:) = abs(real((/dat(70,i-1),dat(71,i-1),dat(71,i),dat(70,i)/)))
              if(ib.eq.73) zoney(3,:)             = abs(real((/dat(72,i-1),dat(73,i-1),dat(73,i),dat(72,i)/)))
              nz = (ib-67)/2
           end if
           if(ib.eq.68.or.ib.eq.70.or.ib.eq.72.or.ib.eq.74) then
              zoney(1,:) = abs(real((/dat(69,i-1),dat(70,i-1),dat(70,i),dat(69,i)/)))
              zoney(2,:) = abs(real((/dat(71,i-1),dat(72,i-1),dat(72,i),dat(71,i)/)))
              zoney(3,:) = abs(real((/dat(73,i-1),dat(74,i-1),dat(74,i),dat(73,i)/)))
              nz = (ib-68)/2
              if(nz.eq.0.and.dib.ne.0) nz = 1
           end if
           
           if((dib.eq.1.or.dib.eq.3).and.i.gt.2) then  !'Glue' the jumps
              do j=1,abs(dib)
                 zoney(3,1:2) = (/zoney(2,2),zoney(3,1)/)
                 zoney(2,1:2) = (/zoney(1,2),zoney(2,1)/)
                 zoney(1,1:2) = (/0.,zoney(1,1)/)
              end do
           end if
           
           if((dib.eq.-1.or.dib.eq.-3).and.i.gt.2) then  !'Glue' the jumps
              do j=1,abs(dib)
                 zoney(1,1:2) = (/zoney(1,2),zoney(2,1)/)
                 zoney(2,1:2) = (/zoney(2,2),zoney(3,1)/)
                 zoney(3,1:2) = (/zoney(3,2),zoney(3,2)/)
              end do
           end if
           
           
           do j=1,nz
              !Zeroes or change in the number of zones can mean trouble:
              if(zoney(j,1)*zoney(j,2)*zoney(j,3)*zoney(j,4).lt.1.e-8.or.dib.ne.0) then
                 if((zoney(j,1)+zoney(j,2))/dat(4,i).lt.1.e-5.and.zoney(j,3)*zoney(j,4).gt.1.e-8) then
                    zoney(j,1) = (zoney(j,3)+zoney(j,4))/2.  !Zone begins, make it end nicely in a point at the left
                    zoney(j,2) = (zoney(j,3)+zoney(j,4))/2.
                 end if
                 if((zoney(j,3)+zoney(j,4))/dat(4,i).lt.1.e-5.and.zoney(j,1)*zoney(j,2).gt.1.e-8) then
                    zoney(j,3) = (zoney(j,1)+zoney(j,2))/2.  !Zone ends, make it end nicely in a point at the right
                    zoney(j,4) = (zoney(j,1)+zoney(j,2))/2.
                 end if
                 if(zoney(j,2).lt.1.e-10.and.zoney(j,1)*zoney(j,3)*zoney(j,4).gt.1.e-10) zoney(j,2) = zoney(j,3)
                 if(zoney(j,3).lt.1.e-10.and.zoney(j,1)*zoney(j,2)*zoney(j,4).gt.1.e-10) zoney(j,3) = zoney(j,2)
              end if
           end do
           
           !Keep this for testing
           !if(xx(i).gt.1280.and.xx(i).lt.1330) then
           !!if(dib.ne.0) then
           !write(6,*)''
           !write(6,'(4I5,6F8.4)')i-1,nint(xx(i-1)),ibold,dib,real(dat(69:74,i-1))
           !write(6,'(4I5,6F8.4)')i,nint(xx(i)),ib,dib,real(dat(69:74,i))
           !write(6,'(5I10)')nint(zonex),nz
           !do j=1,nz
           !write(6,'(4F10.6)')zoney(j,:)
           !end do
           !end if
           
           
           do j=1,nz
              zoney1(1:4) = zoney(j,1:4)
              if(zoney(j,1)+zoney(j,2)+zoney(j,3)+zoney(j,4).gt.1.e-8) call pgpoly(4,zonex,zoney1(1:4))
           end do
           ibold = ib
        end do   !do i=2,n
     end if !If plsmcnv.eq.1
     
     
     !*** Nuclear burning ***
     if(plnuc.eq.1) then
        call pgsfs(4)
        call pgslw(3)
        call pgsls(1)
        call pgsci(2)
        ibold = 0
        do i=2,n
           do j=80,76,-1
              !If upper and lower boundary are equal, remove them:
              if(dabs(dat(j-1,i)-dat(j,i)).lt.1.d-10) dat(j-1:j,i) = (/0.d0,0.d0/)
           end do
           ib = 80
           do j=80,75,-1
              if(dat(j,i).eq.0.d0) ib = j-1
           end do
           dib = ib-ibold
           
           zonex = 0.
           zoney = 0.
           nz = 1
           zonex = real((/xx(i-1),xx(i-1),xx(i),xx(i)/))
           if(ib.eq.75.or.ib.eq.77.or.ib.eq.79) then
              zoney(1,:)                          = abs(real((/0.d0,dat(75,i-1),dat(75,i),0.d0/)))
              if(ib.eq.77.or.ib.eq.79) zoney(2,:) = abs(real((/dat(76,i-1),dat(77,i-1),dat(77,i),dat(76,i)/)))
              if(ib.eq.79) zoney(3,:)             = abs(real((/dat(78,i-1),dat(79,i-1),dat(79,i),dat(78,i)/)))
              nz = (ib-73)/2
           end if
           if(ib.eq.76.or.ib.eq.78.or.ib.eq.80) then
              zoney(1,:) = abs(real((/dat(75,i-1),dat(76,i-1),dat(76,i),dat(75,i)/)))
              zoney(2,:) = abs(real((/dat(77,i-1),dat(78,i-1),dat(78,i),dat(77,i)/)))
              zoney(3,:) = abs(real((/dat(79,i-1),dat(80,i-1),dat(80,i),dat(79,i)/)))
              nz = (ib-74)/2
           end if
           
           if((dib.eq.1.or.dib.eq.3).and.i.gt.2) then  !'Glue' the jumps
              do j=1,abs(dib)
                 zoney(3,1:2) = (/zoney(2,2),zoney(3,1)/)
                 zoney(2,1:2) = (/zoney(1,2),zoney(2,1)/)
                 zoney(1,1:2) = (/0.,zoney(1,1)/)
              end do
           end if
           
           if((dib.eq.-1.or.dib.eq.-3).and.i.gt.2) then  !'Glue' the jumps
              do j=1,abs(dib)
                 zoney(1,1:2) = (/zoney(1,2),zoney(2,1)/)
                 zoney(2,1:2) = (/zoney(2,2),zoney(3,1)/)
                 zoney(3,1:2) = (/zoney(3,2),zoney(3,2)/)
              end do
           end if
           
           
           do j=1,nz
              !Zeroes or change in the number of zones can mean trouble:
              !if(zoney(j,1)*zoney(j,2)*zoney(j,3)*zoney(j,4).lt.1.e-8.or.dib.ne.0) then
              if(dib.ne.0) then
                 if((zoney(j,1)+zoney(j,2))/dat(4,i).lt.1.e-5.and.zoney(j,3)*zoney(j,4).gt.1.e-8) then
                    zoney(j,1) = (zoney(j,3)+zoney(j,4))/2.  !Zone begins, make it end nicely in a point at the left
                    zoney(j,2) = (zoney(j,3)+zoney(j,4))/2.
                 end if
                 if((zoney(j,3)+zoney(j,4))/dat(4,i).lt.1.e-5.and.zoney(j,1)*zoney(j,2).gt.1.e-8) then
                    zoney(j,3) = (zoney(j,1)+zoney(j,2))/2.  !Zone ends, make it end nicely in a point at the right
                    zoney(j,4) = (zoney(j,1)+zoney(j,2))/2.
                 end if
                 !Doesn't seem necessary:
                 !if(zoney(j,2).lt.1.e-10.and.zoney(j,1)*zoney(j,3)*zoney(j,4).gt.1.e-10) zoney(j,2) = zoney(j,3)
                 !if(zoney(j,3).lt.1.e-10.and.zoney(j,1)*zoney(j,2)*zoney(j,4).gt.1.e-10) zoney(j,3) = zoney(j,2)
              end if
           end do
           
           do j=1,nz
              zoney1(1:4) = zoney(j,1:4)
              call pgpoly(4,zonex,zoney1(1:4))
              !if(mod(dib,2).eq.0.or.i.eq.2) then !Even dib
              !call pgline(2,zonex(2:3),zoney(j,(/1,4/))) !Outline the region.  Dangerous?
              !call pgline(2,zonex(2:3),zoney(j,2:3))
              !call pgline(2,zonex(2:3),zoney1((/1,4/))) !Outline the region.  Dangerous?
              !call pgline(2,zonex(2:3),(/zoney1(1),zoney1(4)/)) !Outline the region.  Dangerous?
              zoney2 = zoney1((/1,4/))
              call pgline(2,zonex(2:3),zoney2(1:2)) !Outline the region.  Dangerous?
              call pgline(2,zonex(2:3),zoney1(2:3))
              !end if
           end do
           ibold = ib
        end do   !do i=2,n
        
        
        if(hlp.eq.'y') then
           call pgsch(0.7)
           do i=1,nhp
              xx2 = (/xx(hp(i)),xx(hp(i))/)
              yy2 = (/ymin,ymax/)
              call pgline(2,xx2,yy2)
              write(hlbls,'(I5)')nint(dat(1,hp(i)))
              if(hlbl.eq.'y') call pgtext(xx(hp(i)),y(hp(i)),hlbls)
           end do
        end if
     end if !If plnuc.eq.1
     
  end if !if(ch.eq.1)
  
  
  !Plot outlines
  !call pgsci(14)
  !do j=63,68
  !   do i=1,n
  !      if(dat(j,i).ne.0.d0) call pgpoint(1,xx(i),abs(real(dat(j,i))),1) !semiconvection bounds
  !   end do
  !end do !j
  !call pgsci(15)
  !do j=69,74
  !   do i=1,n
  !      if(dat(j,i).ne.0.d0) call pgpoint(1,xx(i),abs(real(dat(j,i))),1) !convection bounds
  !   end do
  !end do !j
  
  call pgsci(2)
  if(plnuc.eq.1) then
     do j=75,80
        do i=1,n
           if(dat(j,i).ne.0.d0) call pgpoint(1,xx(i),abs(real(dat(j,i))),1) !nuclear burning bounds
        end do
     end do !j
  end if
  
  !Plot core boundaries:
  if(plcb.eq.1) then
     call pgslw(3)
     call pgsls(1)
     do j=5,7 !core masses
        call pgsci(j-1)
        do i=1,n-1
           dat1(1:2) = real(dat(j,i:i+1))
           if(maxval(dat1(1:2)).gt.1.e-10) call pgline(2,xx(i:i+1),dat1(1:2))  !Only plot when one of them != 0
        end do
     end do !j
  end if
  
  
  !Set original styles and colours:
  call pgsci(ci0)
  call pgslw(lw0)
  call pgsch(ch0)
  
  !Replot axes:
  call pgbox('BCTS',0.0,0,'BCTS',0.0,0)
  
end subroutine pltconvection
!***********************************************************************************************************************************


  
  


!***********************************************************************************************************************************
!> \brief  Find the model in a .mdl[12] file closest to a selected point in a graph
subroutine identify_closest_mdl_model(nn,nx,ny,xx,yy,xmin,xmax,ymin,ymax)
  implicit none
  integer, intent(in) :: nn,nx,ny
  real, intent(in) :: xx(10,nn),yy(10,nn)  
  
  integer :: ix,iy,ix0,iy0,i,i0,nsel,col
  real :: dist,mindist
  real :: xmin,xmax,ymin,ymax,dx,dy,xsel(4),ysel(4)
  
  character :: hlbls*5
  
  !Identify closest model
  xsel = 0.
  ysel = 0.
  write(6,'(A)')' Select a point in the graph and press "x" to finish'
  nsel=0
  call pgsci(1)
  call pgolin(1,nsel,xsel,ysel,2)
  
  ix0 = 1
  iy0 = 1
  
  
  dx = abs(xmax-xmin)
  dy = abs(ymax-ymin)
  mindist = huge(mindist)
  do ix=1,nx
     do iy=1,ny
        do i=1,nn
           dist = (abs(xsel(1)-xx(ix,i))/dx)**2 + (abs(ysel(1)-yy(iy,i))/dy)**2
           if(dist.lt.mindist) then
              i0 = i
              ix0 = ix
              iy0 = iy
              mindist = dist
           end if
        end do
     end do
  end do
  write(6,*)''
  write(6,'(A,ES12.4,A,ES12.4)')          ' Selected point:    x =',xsel(1),',  y =',ysel(1)
  write(6,'(A,ES12.4,A,ES12.4,A,I5)')' Closest model:     x =',xx(ix0,i0),',  y =',yy(iy0,i0),  &
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
  
  call pgpoint(1,xx(ix0,i0),yy(iy0,i0),2)
  write(hlbls,'(I5)')i0
  call pgptxt(xx(ix0,i0),yy(iy0,i0),0.,0.,hlbls)
  
  call pgsci(1)
  
end subroutine identify_closest_mdl_model
!***********************************************************************************************************************************
