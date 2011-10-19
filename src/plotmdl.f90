!> \file plotmdl.f90  Plots the data contained in mdl[12] files

! AF, 19-05-2005

! Copyright 2002-2011 AstroFloyd - astrofloyd.org
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


!***********************************************************************************************************************************
!> \brief Plot the data in the *.mdl[12] output files of ev

program plotmdl  
  use kinds
  use constants
  use mdl_data
  
  implicit none
  integer :: nr,mdl,nx,ny,nsel,io,xwini,pgopen
  !real(double) :: dat1(nq)
  
  real(double) :: dat(nq,nn),age
  real :: xmin,xmax,ymin,ymax,xmin0,xmax0,ymin0,ymax0,x
  real :: xx(10,nn),yy(10,nn),xx1(nn),yy1(nn),xsel(4),ysel(4),x2(2),y2(2)
  
  integer i,ii,j,blk,nblk,vx,vy,hmp,plot,plotstyle,ansi,col
  character findfile*(99),fname*(99),rng,log,xwin*(19)
  character :: lx*(99),ly*(99),fx*(99),fy*(99),title*(99),psname*(99)
  logical :: ex, ab,nab,PCEy,ECEx,JCEx,ECEy,JCEy
  
  
  ! Set constants:
  call setconstants()
  write(6,*)
  call print_code_version(6)  !To screen
  
  call evTools_settings()
  
  plot = 0
  plotstyle = 1
  xwini = 1  !Number of X window to try first
  log = 'n'
  pxnr = 0
  pxin = 0
  do i=200,nq
     pxin(i) = i
  end do
  
  
  ! Define variable labels:
  call set_mdl_labels()
  
  
3 continue
  
  ! Read current path and use it as plot title:
  write(title,'(A)')trim(workdir)
  
  ! Get filename:
  if(command_argument_count().eq.1) then
     call get_command_argument(1,fname)
  else
     fname = findfile('*.mdl*')  ! Search for input file in current dir
  end if
  
  
  
  !***   READ ALL STRUCTURE MODELS IN THE FILE AND DISPLAY MAIN PROPERTIES
  
4 continue
  call list_mdl_models(trim(fname),nblk)
  
  if(nblk.eq.1) then
     blk = 1 
     goto 25
  end if
  
  
  
  
  !***   CHOOSE STRUCTURE MODEL
20 continue
  write(6,'(A47,I3,A3)',advance='no')' Which structure model do you want to plot (1-',nblk,'): '
  read*,blk
  if(blk.eq.0) goto 9999
  if(blk.lt.1.or.blk.gt.nblk) goto 20 
  
  
  
  ! Read file, upto chosen model (blk-1):
25 continue
  
  
  ! Open the input file and read the first blk-1 models:
  call read_first_mdls(fname,blk-1)
  nm = nmsh
  
  
  !***   READ CHOSEN STRUCTURE MODEL
  call read_chosen_mdl(blk, mdl,age,dat)
  close(10)
  
  ! Add file name and model number to plot title
  write(title,'(A,I6)')trim(title)//'/'//trim(fname),mdl
  
  
  
  
  !***   COMPUTE ADDITIONAL PLOT VARIABLES
  if(plot.eq.0) call compute_mdl_variables(dat)
  
  
  
  
  !***   CHOOSE PLOT VARIABLES
32 continue   
  write(6,*)''
  
  
  nr = 4 ! Number of variable columns
  ii = ceiling(real(nc)/real(nr)) !Number of rows
  write(6,'(A)')' Variables:                         0: Quit                   ' 
  do i=1,ii
     do j=0,nr-1
        if(pxnr(i+j*ii).eq.0) then
           write(6,'(A19)',advance='no')''
        else
           write(6,'(I9,A10,5x)',advance='no')i+j*ii,': '//pxns(pxnr(i+j*ii))
        end if
     end do
     write(6,*)
  end do
  
  
  ! Print derived variables, from number 201 on:
  write(6,'(A)')'                                                              '
  write(6,'(A)')'  Derived variables:                                          '
  
  nr = 4 !Number of variable columns
  ii = ceiling(real(nv_der)/real(nr)) !Number of rows
  do i=1,ii
     do j=0,nr-1
        if(pxnr(200+i+j*ii).eq.0) then
           write(6,'(A19)',advance='no')''
        else
           write(6,'(I9,A10,5x)',advance='no')200+i+j*ii,': '//pxns(pxnr(200+i+j*ii))
        end if
     end do
     write(6,*)
  end do
  
  
  ! Print special variables, from number 301 on:
  write(6,'(A)')'                                                              '
  write(6,'(A)')'  Special plots:                                          '
  
  nr = 2 !Number of variable columns
  ii = ceiling(real(nv_sp)/real(nr)) !Number of rows
  do i=1,ii
     do j=0,nr-1
        if(pxnr(300+i+j*ii).eq.0) then
           write(6,'(A39)',advance='no')''
        else
           write(6,'(I9,A30,5x)',advance='no')300+i+j*ii,': '//pxns(pxnr(300+i+j*ii))
        end if
     end do
     write(6,*)
  end do
  
  write(6,*)''
  write(6,*)''
  
  
  
  
  ab = .false.
  nab = .false.
  PCEy = .false.
  ECEx = .false.
  JCEx = .false.
  ECEy = .false.
  JCEy = .false.
  
  
  vx = nq
  do while (pxnr(vx).le.0)
     write(6,'(A)',advance='no')' Choose the X-axis variable: '
     read*,vx
     vx = min(max(vx,0),nq)
     if(vx.eq.0) then
        write(6,'(A,/)')' Program finished'
        stop
     end if
  end do
  
  vy = nq
  do while (pxnr(vy).le.0)
     write(6,'(A)',advance='no')' Choose the Y-axis variable: '
     read*,vy
     vy = min(max(vy,0),nq)
     if(vy.eq.0) then
        write(6,'(A,/)')' Program finished'
        stop
     end if
  end do
  
  
37 continue 
  lx = labels(pxnr(vx))
  ly = labels(pxnr(vy))
  fx = pxfns(pxnr(vx))
  fy = pxfns(pxnr(vy))
  
  nx = 1
  ny = 1
  
  
  do i=1,10
     xx(i,1:nm) = real(dat(vx,1:nm))
     yy(i,1:nm) = real(dat(vy,1:nm))
  end do
  
  
  ! *** SPECIAL PLOTS:
  
  ! Abundances plot:
  if(vy.eq.301.or.ab) then
     ab = .true.
     vy = pxin(10)
     yy(1:7,1:nm) = real(dat(pxin(10):pxin(16),1:nm))
     ny = 7
  end if
  
  ! Nablas plot:
  if(vy.eq.302.or.nab) then
     nab = .true.
     vy = pxin(6)
     yy(1,1:nm) = real(dat(pxin(6),1:nm))    ! Nabla_ad
     yy(2,1:nm) = real(dat(pxin(202),1:nm))  ! Nabla_rad
     yy(3,1:nm) = real(dat(pxin(7),1:nm))    ! True Nabla
     print*,pxin(6),pxin(7),pxin(202)
     ny = 3
  end if
  
  ! CE Porb plot:
  if(vy.eq.303.or.PCEy) then
     PCEy = .true.
     vy = 221
     yy(1,1:nm) = real(dat(221,1:nm))        ! P(r(m)=Rrl)
     yy(2,1:nm) = real(dat(225,1:nm))        ! P(post-alpha-CE)
     yy(3,1:nm) = real(dat(229,1:nm))        ! P(post-gamma-CE)
     ny = 3
  end if
  
  
  ! CE Eorb plot:
  if(vx.eq.304.or.ECEx) then
     ECEx = .true.
     vx = 222
     xx(1,1:nm) = real(dat(222,1:nm))        ! E(r(m)=Rrl)
     xx(2,1:nm) = real(dat(226,1:nm))        ! E(post-alpha-CE)
     xx(3,1:nm) = real(dat(230,1:nm))        ! E(post-gamma-CE)
     nx = 3
  end if
  if(vy.eq.304.or.ECEy) then
     ECEy = .true.
     vy = 222
     yy(1,1:nm) = real(dat(222,1:nm))        ! E(r(m)=Rrl)
     yy(2,1:nm) = real(dat(226,1:nm))        ! E(post-alpha-CE)
     yy(3,1:nm) = real(dat(230,1:nm))        ! E(post-gamma-CE)
     ny = 3
  end if
  
  ! CE Jorb plot:
  if(vx.eq.305.or.JCEx) then
     JCEx = .true.
     vx = 223
     xx(1,1:nm) = real(dat(223,1:nm))        ! J(r(m)=Rrl)
     xx(2,1:nm) = real(dat(227,1:nm))        ! J(post-alpha-CE)
     xx(3,1:nm) = real(dat(231,1:nm))        ! J(post-gamma-CE)
     nx = 3
  end if
  if(vy.eq.305.or.JCEy) then
     JCEy = .true.
     vy = 223
     yy(1,1:nm) = real(dat(223,1:nm))        ! J(r(m)=Rrl)
     yy(2,1:nm) = real(dat(227,1:nm))        ! J(post-alpha-CE)
     yy(3,1:nm) = real(dat(231,1:nm))        ! J(post-gamma-CE)
     ny = 3
  end if
  
  
  if(nx.ne.1.and.nx.ne.ny) write(0,'(/,A,/)')' The number of X variables is different from the number of Y variables (nx!=ny).'// &
       "  I'll proceed, but the results may be inconsistent."
  
  
  
  
  
  
  !***   LIN/LOG AXES
  
  write(6,'(A)',advance='no')' Do you want a logarithmic scale: (N)o, (X)-axis, (Y)-axis, (B)oth: '
  read*,log
  if(log.eq.'X') log='x'
  if(log.eq.'Y') log='y'
  if(log.eq.'B') log='b'
  if(log.eq.'N') log='n'
  
  if(log.eq.'x'.or.log.eq.'b') then
     do i=1,nx
        if(xx(i,1).eq.0.d0) xx(i,1) = xx(i,2)
     end do
     xx(1:nx,1:nm) = log10(abs(xx(1:nx,1:nm))+1.e-20)
  end if
  if(log.eq.'y'.or.log.eq.'b') then
     do i=1,ny
        if(yy(i,1).eq.0.d0) yy(i,1) = yy(i,2)
     end do
     yy(1:ny,1:nm) = log10(abs(yy(1:ny,1:nm))+1.e-20)
  end if
  
  xmin = minval(xx(1:nx,1:nm))
  xmax = maxval(xx(1:nx,1:nm))
  ymin = minval(yy(1:ny,1:nm))
  ymax = maxval(yy(1:ny,1:nm))
  
  if(ab.and.(log.eq.'y'.or.log.eq.'b').and.ymin.lt.-6.) ymin = -6.
  
  
  
  
  
  
  
  
  
  !***   PLOT RANGE
  
  xmin0 = xmin
  xmax0 = xmax
  ymin0 = ymin
  ymax0 = ymax
  
70 write(6,*)''
  write(6,*)' X-range:',xmin,'-',xmax
  write(6,*)' Y-range:',ymin,'-',ymax
  write(6,'(A)',advance='no')' Do you want to change a plot range ?  (N)o, (X)-axis, (Y)-axis, (B)oth:  '
  read*,rng
  if(rng.eq.'N') rng='n'
  if(rng.eq.'X') rng='x'
  if(rng.eq.'Y') rng='y'
  if(rng.eq.'B') rng='b'
  
  if(rng.eq.'n'.or.rng.eq.' ') goto 100
  
  
  if(rng.eq.'x'.or.rng.eq.'b') then
     write(6,'(A)')' Give the new range for the X-axis (Xmin, Xmax):'
     read*,xmin,xmax
     if(xmin.gt.xmax) then
        x = xmin
        xmin = xmax
        xmax = x
        write(6,'(A)')'  Swapped Xmin and Xmax'
     end if !if(xmin.gt.xmax)
     if(xmin.lt.xmin0) xmin = xmin0
     if(xmax.gt.xmax0) xmax = xmax0
  end if
  
  if(rng.eq.'y'.or.rng.eq.'b') then
     write(6,'(A)')' Give the new range for the Y-axis (Ymin, Ymax):'
     read*,ymin,ymax
     if(ymin.gt.ymax) then
        x = ymin
        ymin = ymax
        ymax = x
        write(6,'(A)')'  Swapped Ymin and Ymax'
     end if !if(ymin.gt.ymax)
     if(ymin.lt.ymin0) ymin = ymin0
     if(ymax.gt.ymax0) ymax = ymax0
  end if
  
  write(6,*)''
  print*,'X-range:',xmin,'-',xmax
  print*,'Y-range:',ymin,'-',ymax
  
  
100 continue
  x = 0.02*abs(xmax-xmin)
  if(x.eq.0.) x = 0.05*xmax
  xmin = xmin - x
  xmax = xmax + x
  x = 0.02*abs(ymax-ymin)
  if(x.eq.0.) x = 0.05*ymax
  ymin = ymin - x
  ymax = ymax + x
  
  
  
  hmp = 0
  if(vx.eq.201) then
111  write(6,'(A28,I4,A3)',advance='no')' Highlight a mesh point (1 -',nm,'): '
     read*,hmp
     if(hmp.gt.nm) goto 111
     if(hmp.lt.1) hmp=0
  end if
  
  
  
  
  
  
  
  
  
  !***   PLOT TO SCREEN OR FILE
  
501 continue
  if(plot.eq.8) then !PS file
     ex = .true.
     i = 1
     do while(ex)
        write(psname,'(A,I3.3,A4)')'plot_mdl_'//trim(fx)//'-'//trim(fy)//'_',i,'.eps'
        inquire(file=trim(psname), exist=ex)  ! Check whether the file already exists; ex is True or False
        i = i+1
     end do
     call pgbegin(1,trim(psname)//'/cps',1,1)
     call pgpap(10.5,0.68)                    ! Make it fit on letter paper
     !call pgpap(10.5,0.25)                    ! Tailored ratio
     call pgslw(2)
  else ! Screen
     io = 0
     do while(io.le.0)
        write(xwin,'(I3.3,A7)')xwini,'/xserve'
        io = pgopen(trim(xwin))
        if(io.le.0) then
           write(6,'(A,I3,A,I3)')' X window',xwini," is unavailable, I'll try",xwini+1
           xwini = xwini + 1
        end if
     end do
     
     call pgpap(scrsz,scrrat)
     call pgscf(1)
     if(white_bg) then                         ! Create a white background; swap black (ci=0) and white (ci=1)
        call pgscr(0,1.,1.,1.)                 ! For some reason, this needs to be repeated for AquaTerm
        call pgscr(1,0.,0.,0.)
        call pgsci(0)
        call pgsvp(0.,1.,0.,1.)
        call pgswin(-1.,1.,-1.,1.)
        call pgrect(-2.,2.,-2.,2.)
        call pgsci(1)
     end if
  end if
  
  if(nx*ny.eq.1) then
     call pgsvp(0.06,0.96,0.07,0.96)
  else
     call pgsvp(0.06,0.92,0.07,0.96)          ! Multiple lines; need room for legend on right-hand side
  end if
  
  call pgswin(xmin,xmax,ymin,ymax)
  if(log.eq.'n') call pgbox('BCNTS',0.0,0,'BCNTS',0.0,0)  !Use logarithmic axes rather than logarithmic variables
  if(log.eq.'x') call pgbox('BCLNTS',0.0,0,'BCNTS',0.0,0)
  if(log.eq.'y') call pgbox('BCNTS',0.0,0,'BCLNTS',0.0,0)
  if(log.eq.'b') call pgbox('BCLNTS',0.0,0,'BCLNTS',0.0,0)
  call pgmtxt('T',0.7,0.5,0.5,'~'//trim(title(12:)))  !13 to remove /home/user/
  call pgmtxt('B',2.4,0.5,0.5,lx)
  call pgmtxt('L',2.4,0.5,0.5,ly)
  
  do i=1,ny
     col = colours(mod(i-1,ncolours)+1)
     call pgsci(col)
     xx1(1:nm) = xx(i,1:nm)  !CHECK: only for nx=1 or nx=ny
     yy1(1:nm) = yy(i,1:nm)
     
     select case(plotstyle)
     case(1)
        call pgline(nm,xx1(1:nm),yy1(1:nm))
     case(2)
        call pgpoint(nm,xx1(1:nm),yy1(1:nm),1)
     case(3)
        call pgline(nm,xx1(1:nm),yy1(1:nm))
        call pgsci(1)
        call pgpoint(nm,xx1(1:nm),yy1(1:nm),20)
        call pgsci(col)
     end select
     
     if(ab)  call pgmtext('RV',0.5,real(ny+1-i)/20.,0.,trim(abds(i)))
     if(nab) call pgmtext('RV',0.5,real(ny+1-i)/20.,0.,trim(nabs(i)))
     if(PCEy.or.ECEx.or.JCEx.or.ECEy.or.JCEy) call pgmtext('RV',0.5,real(ny+1-i)/20.,0.,trim(CEs(i)))
  end do
  
  
  call pgsch(1.5)
  call pgsci(8)
  if(hmp.ne.0) then
     do i=1,ny
        call pgpoint(1,xx(1,hmp),yy(i,hmp),2)
     end do
  end if
  call pgsci(1)
  call pgsch(1.)
  call pgsls(2)
  if(vy.eq.21) then
     x2 = (/xmin,xmax/)
     y2 = (/0.,0./)
     call pgline(2,x2,y2)
  end if
  
  if(plot.eq.8) then
     call pgend
     write(6,'(A)')' Plot saved to '//trim(psname)
  end if
  
  
  
  
  
  
  
  
  
  
  
  
  
  !***   FINISH
  
900 if(plot.ne.8) then
     write(6,*)''
     write(6,'(A)')' You can:'
     write(6,'(A)')'  0) quit'
     write(6,'(A)')'  1) change variables'
     write(6,'(A)')'  2) change lin/log axes'
     write(6,'(A)')'  3) change axis ranges'
     write(6,'(A)')'  4) select zoom region'
     write(6,'(A)')'  5) zoom out'
     write(6,'(A)')'  6) change structure model'
     write(6,'(A)')'  7) change input file'
     write(6,'(A)')'  8) save plot as postscript'
     write(6,'(A)')'  '
     write(6,'(A)')' 10) identify a point in the graph'
     write(6,'(A)')' 11) toggle drawing lines/points'
  end if !if(plot.ne.9) then
  write(6,*)''
  write(6,'(A27)',advance='no')' What do you want to do ?  '
  read*,plot
  if(plot.lt.0 .or. plot.eq.9 .or. plot.gt.11) goto 900
  
  if(plot.ne.4.and.plot.ne.10) call pgend
  if(plot.eq.1) goto 32
  if(plot.eq.2) goto 37
  if(plot.eq.3) goto 70
  if(plot.eq.6) goto 4
  if(plot.eq.7) goto 3
  if(plot.eq.8) goto 501
  
  if(plot.eq.4) then  !Select region
941  continue
     call pgsci(1)
     xsel = 0.
     ysel = 0.
     write(6,'(A)')' Select 2-4 corner points with your left mouse button and press "x" to finish'
     nsel=0
     call pgolin(4,nsel,xsel,ysel,2)
     if(nsel.lt.2) then
        write(6,'(A)')' I need at least 2 corner points...'
        goto 941
     end if
     xmin = minval(xsel(1:nsel))  !The new window is drawn for the extreme values of these points
     xmax = maxval(xsel(1:nsel))
     ymin = minval(ysel(1:nsel))
     ymax = maxval(ysel(1:nsel))
     write(6,*)''
     write(6,*)' X-range:',xmin,'-',xmax
     write(6,*)' Y-range:',ymin,'-',ymax
     write(6,*)''
     call pgend
     goto 501
  end if
  
  if(plot.eq.5) then  !Zoom out
     xmin = (xmin+xmax)/2. - 2*abs((xmin+xmax)/2.-xmin) !Central value - 2x the 'radius', 'radius' = central value - minimum
     xmax = (xmin+xmax)/2. + 2*abs((xmin+xmax)/2.-xmin)
     ymin = (ymin+ymax)/2. - 2*abs((ymin+ymax)/2.-ymin)
     ymax = (ymin+ymax)/2. + 2*abs((ymin+ymax)/2.-ymin)
     write(6,*)''
     write(6,*)' X-range:',xmin,'-',xmax
     write(6,*)' Y-range:',ymin,'-',ymax
     write(6,*)''
     goto 501
  end if
  
  
  if(plot.eq.10) then
     call identify_closest_mdl_model(nn,nx,ny,xx,yy, xmin,xmax,ymin,ymax)
     goto 900
  end if
  
  if(plot.eq.11) then !Toggle between drawing points, lines or both
     ansi=-1
     do while(ansi.lt.0.or.ansi.gt.3)
        write(6,'(A)')'  You can plot:'
        write(6,'(A)')'  0: keep the current choice'
        write(6,'(A)')'  1: lines'
        write(6,'(A)')'  2: dots'
        write(6,'(A)')'  3: both'
        write(6,'(A)', advance='no')'  What would you like to plot?  '
        read*,ansi
     end do
     if(ansi.gt.0) plotstyle = ansi !1-3
     goto 501
  end if
  
  
9999 continue
  write(6,'(A,/)')' Program finished'
end program plotmdl
!***********************************************************************************************************************************

