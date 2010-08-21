!> \file plotmdl.f90  Plots the data contained in a mdl* file

! AF, 19-05-2005

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


!> \brief Plot the data in the *.mdl? output file of ev
program plotmdl  
  use kinds
  use constants
  use mdl_data
  
  implicit none
  integer :: nm,nc,nr,mdl,ny,nsel,pxnr(nq),pxin(nq),io,xwini,pgopen
  real(double) :: dat1(nq),rl2p,rl2a
  real(double) :: dE,Eorb,Eorbi,a_orb,a_orbi,Porb,alphaCE
  
  real :: dat(nq,nn),age
  real :: ver,x
  real :: xmin,xmax,ymin,ymax,xmin0,xmax0,ymin0,ymax0
  real :: xx(nn),yy(10,nn),yy1(nn),xsel(4),ysel(4),x2(2),y2(2)
  
  real(double) :: mm,rr,pp,rrh,tt,kk,nnad,nnrad,hh,hhe,ccc,nnn,oo,nne,mmg
  real(double) :: ll,eeth,eenc,eenu,ss,uuint
  real(double) :: m1,m2,r1,l1,ts,tc,mhe,mco,rhoc
  real(double) :: hc,hec,cc,oc,zc,hs,hes,cs,os,zs
  
  integer i,ii,j,blk,nblk,vx,vy,hmp,plot
  character findfile*99,fname*99,rng,log,bla*3,xwin*19
  character :: lx*99,ly*99,fx*99,fy*99,title*99,psname*99
  logical :: ex, ab,nab,CE
  
  !Set constants:
  call setconstants()
  write(6,*)
  call print_code_version(6)  !To screen
  
  call eggletonplot_settings()
  
  plot = 0
  xwini = 1  !Number of X window to try first
  log = 'n'
  pxnr = 0
  pxin = 0
  do i=200,nq
     pxin(i) = i
  end do
  
  
  ab = .false.
  nab = .false.
  CE = .false.
  
  ! Define variable labels:
  call set_mdl_labels()
  
  
  !Read current path and use it as plot title
3 continue
  write(title,'(A)')trim(workdir)
  
  if(command_argument_count().eq.1) then
     call get_command_argument(1,fname)
  else
     fname = findfile('*.mdl*')  ! Search for input file in current dir
  end if
  
  
  
  !***   READ ALL STRUCTURE MODELS IN THE FILE AND DISPLAY MAIN PROPERTIES
  
  write(6,*)''
4 write(6,'(A)')' Reading file '//trim(fname)
  open (unit=10,form='formatted',status='old',action='read',file=fname,iostat=io)
  if(io.ne.0) then
     write(6,'(A,/)')'  Error opening file '//trim(fname)//', aborting...'
     close(10)
     stop
  end if
  rewind 10
  
  io = 0
  read(10,5,iostat=io) nm,nc,ver !Ver used to be overshoot parameter, now file version number (if>1)
5 format (2x,I4,4x,I2,F7.3)
  if(io.ne.0) then
     write(6,'(A,/)')'  Error reading first line of file, aborting...'
     close(10)
     stop
  end if
  
  if(ver.gt.1.) then
     read(10,*)bla 
  else
     nc = 21
     pxnr(1:nc)=(/9,17,2,3,4,5,6,8,10,11,12,13,14,15,16,18,19,20,21,28,27/)!,50,51,52,53,54,55,31,7,24,25,26,60
  end if
  
  write(6,'(A,I4,A,I3,A)')' Reading',nm,' meshpoints,',nc,' columns of data.'
  
  write(6,*)''
  write(6,'(A)')'  Nr  Model Nmsh          Age        M1   Mhe   Mco     Menv         R        L     Teff       Tc     Rhoc'//  &
       '      Xc     Yc     Cc     Oc     Xs    Ys    Zs'
  do ii=1,999
     if(mod(ii,25).eq.0) then
        write(6,*)''
        write(6,'(A)')'  Nr  Model Nmsh          Age        M1   Mhe   Mco     Menv         R        L     Teff       Tc'//  &
             '     Rhoc      Xc     Yc     Cc     Oc     Xs    Ys    Zs'
     end if
     read(10,6,iostat=io) mdl,age
     if(io.lt.0) exit
     if(io.gt.0) then
        write(6,'(A,/)')'  Error reading first line of block, aborting...'
        close(10)
        stop
     end if
6    format (I6,1x,E16.9)
     mhe = 0.
     mco = 0.
     do j=1,nm
        read(10,7,err=13,end=15)mm,rr,pp,rrh,tt,kk,nnad,nnrad,hh,hhe,ccc,nnn,oo,nne,mmg,ll,eeth,eenc,eenu,ss,uuint
        if(j.eq.1) then
           tc  = tt
           hc  = hh
           hec = hhe
           cc = ccc
           oc = oo
           zc  = 1. - hh - hhe
           rhoc = rrh
        end if
        if(j.eq.nm) then
           m1  = mm
           r1  = rr
           l1  = ll
           ts  = tt
           hs  = hh
           hes = hhe
           cs = ccc
           os = oo
           zs  = 1. - hh - hhe
        end if
        if(mhe.eq.0.0.and.hh.gt.0.1) mhe = mm
        if(mco.eq.0.0.and.hhe.gt.0.1) mco = mm
        
     end do !do j=1,nm
     
     write(6,9)ii,mdl,nm,age,m1,mhe,mco,m1-mhe,r1,l1,ts,tc,rhoc,hc,hec,cc,oc,hs,hes,zs!,bms,p,p1
     
7    format (1P,E13.6,4E11.4,16E11.3)
     
  end do !ii
  goto 15
  
  
9 format (I4,I7,I5,ES13.5,f10.4,2f6.3,ES9.2,1x,4ES9.2,ES9.2,1x,4f7.4,1x,3f6.3)
  
13 print*,'  Error reading block',i-1,'line',j-1,', aborting...'
  close(10)
  goto 9999
15 close(10)
  
  nblk = ii-1
  write(6,*)''
  print*,' EOF reached,',nblk,' blocks read.'
  write(6,*)''
  
  if(nblk.eq.0) goto 9999
  if(nblk.eq.1) then
     blk = 1 
     goto 25
  end if
  
  
  
  
  !***   CHOOSE STRUCTURE MODEL
20 write(6,'(A47,I3,A3)',advance='no')' Which structure model do you want to plot (1-',nblk,'): '
  read*,blk
  if(blk.eq.0) goto 9999
  if(blk.lt.1.or.blk.gt.nblk) goto 20 
  
  !Read file, upto chosen model (blk-1)
25 open (unit=10,form='formatted',status='old',file=fname)
  rewind 10
  read(10,5,iostat=io) nm,nc,ver !Ver used to be overshoot parameter, now file version number (if>1)
  if(io.ne.0) then
     write(6,'(A,/)')'  Error reading first line of file, aborting...'
     close(10)
     stop
  end if
  if(ver.gt.1.) then
     read(10,'(60I4)')pxnr(1:nc)
  else
     nc = 21
  end if
  do i=1,blk-1
     read(10,6,iostat=io) mdl,age
     if(io.lt.0) exit
     if(io.gt.0) then
        write(6,'(A,/)')'  Error reading first line of block, aborting...'
        close(10)
        stop
     end if
     do j=1,nm
        read(10,*,iostat=io) bla
        if(io.ne.0) then
           print*,'  Error reading block',i-1,'line',j-1,' while reading the blocks before the selected one, aborting...'
           close(10)
           stop
        end if
     end do !j
  end do !i
  
  
  !***   READ CHOSEN STRUCTURE MODEL
  read(10,6,iostat=io) mdl,age
  if(io.ne.0) then
     write(6,'(A,/)')'  Error reading first line of block, aborting...'
     close(10)
     stop
  end if
  do j=1,nm
     read(10,*,iostat=io) (dat1(i),i=1,nc)  !Gfortran reports a read error when the number is smaller or larger than the accuracy
     dat(1:nc,j) = real(dat1(1:nc))
     if(io.ne.0) then
        print*,'  Error reading line',j-1,' of the selected block, aborting...'
        close(10)
        print*,dat(1:nc,j)
        stop
     end if
  end do
  close(10)
  
  !Add file name and model number to plot title
  write(title,'(A,I6)')trim(title)//'/'//trim(fname),mdl
  
  
  
  
  !***   COMPUTE ADDITIONAL PLOT VARIABLES
  if(plot.eq.0) then
     !Create inverse pxnr index, pxin:  if pxin(i) = 0, then the variable px(i) is not in the file
     do i=1,nc
        if(pxnr(i).gt.0) pxin(pxnr(i)) = i
     end do
     
     do i=1,nm
        dat(201,i) = real(i)
     end do
     dat(202,1:nm) = dat(pxin(6),1:nm) + dat(pxin(8),1:nm)                                 !Nabla_rad
     !Difference between Nabla_rad and Nabla_ad, +1 or -1, +1: convection, calculate after Nabla_rad:
     dat(8,1:nm)   = dat(pxin(8),1:nm)/abs(dat(pxin(8),1:nm))
     dat(203,1:nm) = dat(pxin(9),1:nm)/dat(pxin(9),nm)                                     !M/M*
     dat(204,1:nm) = dat(pxin(17),1:nm)/dat(pxin(17),nm)                                   !R/R*
     dat(205,1:nm) = dat(pxin(12),1:nm)/dat(pxin(14),1:nm)                                 !C/O
     dat(206,1:nm) = dat(pxin(13),1:nm)/dat(pxin(14),1:nm)                                 !Ne/O
     dat(207,1:nm) = -g*dat(pxin(9),1:nm)*m0/(dat(pxin(17),1:nm)*r0) + dat(pxin(27),1:nm)  !Ugr + Uint  
     dat(208,1:nm) = 1.0/(dat(pxin(3),1:nm)*dat(pxin(5),1:nm))                             !Mean free path = 1/(rho * kappa)
     if(pxin(31).ne.0) then
        dat(209,1:nm) = dat(pxin(2),1:nm)/(dat(pxin(31),1:nm)*amu)                !n = rho / (mu * amu)
        pxnr(209) = 209
     end if
     dat(210,1:nm) = real(g*dble(dat(pxin(9),1:nm))*m0/(dble(dat(pxin(17),1:nm))**2*r0**2))                !n = rho / (mu * amu)
     pxnr(201:210) = (/201,202,203,204,205,206,207,208,209,210/)
     
     !Mean molecular weight mu = 2/(1 + 3X + 0.5Y), Astrophysical Formulae I, p.214, below Eq. 3.62):
     dat(211,1:nm) = 2. / (1 + 3*dat(9,1:nm) + 0.5*dat(10,1:nm))
     dat(212,1:nm) = dat(4,1:nm)/(dat(211,1:nm)*m_h)                                       !Particle density       n = rho/(mu*m_H)
     dat(213,1:nm) = a_rad*dat(5,1:nm)**4*c3rd                                             !                   P_rad = aT^4/3
     dat(214,1:nm) = dat(212,1:nm)*k_b*dat(5,1:nm)                                         !                   P_gas = nkT
     dat(215,1:nm) = dat(213,1:nm)/(dat(214,1:nm)+1.e-30)                                  !                    beta = Prad/Pgas
     
     !216-217: binding energy:
     dat(216,1:nm) = 0.0
     dat(217,1:nm) = 0.0
     dat(218,1:nm) = 0.0
     do i=2,nm
        dat(216,i) = dat(pxin(9),i) - dat(pxin(9),i-1)                                     ! Mass of the current shell (Mo)
        !dE = dat(207,i)*dat(216,i) * m0*1.d-40                                            ! BE of the shell (10^40 erg)
        dE = dat(207,i)*dat(216,i) * m0 / (G*M0**2/R0)                                     ! BE of the shell       (G Mo^2 / Ro)
        dat(217,i) = dat(217,i-1) + dE                                                     ! BE of the whole star  (same units)
        if(dat(pxin(10),i).gt.0.1) dat(218,i) = dat(218,i-1) + dE                          ! BE of envelope        (same units)
     end do
     do i=nm-1,1,-1
        if(abs(dat(218,i)).lt.1.d-19) dat(218,i) = dat(218,i+1)                            ! Set the core BE to first non-zero BE
     end do
     
     dat(219,1:nm) = dat(3,1:nm)/dat(4,1:nm)                                               ! P/rho
     
     m1 = dble(dat(pxin(9),nm))                                                            ! Total mass
     m2 = m1                                                                               ! Total mass
     r1 = dble(dat(pxin(17),nm))                                                           ! Surface radius
     dat(220,1) = 0.0
     do i=2,nm
        ! Porb (day) if M1=m, M2=M1, r(m)=Rrl:
        dat(220,i) = real(rl2p(dble(dat(pxin(9),i))*M0, m2*M0, dble(dat(pxin(17),i))*R0)/day)                  
     end do
     pxnr(211:220) = (/211,212,213,214,215,216,217,218,219,220/)
     
     alphaCE = 1.0
     a_orbi = rl2a(m1,m2,r1)                                                               ! a_orb (Ro)
     !Eorbi = -G*m1*m2*M0**2/(2*a_orbi*R0)                                                 ! Eorb (erg)
     Eorbi = -m1*m2/(2*a_orbi)                                                             ! Eorb (G Mo^2 / Ro)
     !print*,m1,m2,r1,a_orbi,Eorbi
     do i=1,nm
        Eorb = Eorbi + dble(dat(217,nm)) - dble(dat(217,i))/alphaCE                        ! Eorb (G Mo^2 / Ro)
        !print*,i, Eorbi, dat(218,nm), dat(218,i)/alphaCE,Eorb
        a_orb = -dble(dat(pxin(9),i))*m2/(2*Eorb)                                          ! a_orb (Ro)
        call a2p(dble(dat(pxin(9),i)+m2)*M0,a_orb*R0,Porb)                                 ! Porb (s)
        !if(mod(i,10).eq.0)print*,i,dat(pxin(9),i),Eorb,a_orb,Porb
        dat(221,i) = real(Porb/day)                                                        ! Porb (day)
     end do
     pxnr(221:221) = (/221/)
     
     
     if(pxin(60).ne.0) then                                                                ! Brint-Vailasakatralala frequency
        dat(pxin(60),1:nm) = abs(dat(pxin(60),1:nm))
     end if
     
     pxnr(301:303) = (/301,302,303/) !Abundances, Nablas, CEs
  end if !if(plot.eq.0) then
  
  
  
  !***   CHOOSE PLOT VARIABLES
32 continue   
  write(6,*)''
  
  nr = 4 !Number of variable columns
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
  
  !Print derived variables, from number 201 on:
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
  
  !Print special variables, from number 301 on:
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
  
  
  
  
35 write(6,'(A)',advance='no')' Choose the X-axis variable: '
  ab = .false.
  read*,vx
  if(vx.eq.0) goto 9999
  if(pxnr(vx).eq.0) goto 35
  
36 write(6,'(A)',advance='no')' Choose the Y-axis variable: '
  read*,vy
  if(vy.eq.0) goto 9999
  if(pxnr(vy).eq.0) goto 36
  
  
37 continue 
  lx = labels(pxnr(vx))
  ly = labels(pxnr(vy))
  fx = pxfns(pxnr(vx))
  fy = pxfns(pxnr(vy))
  
  ny = 1
  
  ! Abundances plot:
  if(vy.eq.301.or.ab) then
     ab = .true.
     vy = pxin(10)
     yy(1:7,1:nm) = dat(pxin(10):pxin(16),1:nm)
     ny = 7
  end if
  
  ! Nablas plot:
  if(vy.eq.302.or.nab) then
     nab = .true.
     vy = pxin(6)
     yy(1,1:nm) = dat(pxin(6),1:nm)   !Nabla_ad
     yy(2,1:nm) = dat(pxin(202),1:nm) !Nabla_rad
     yy(3,1:nm) = dat(pxin(7),1:nm)   !True Nabla
     print*,pxin(6),pxin(7),pxin(202)
     ny = 3
  end if
  
  ! CE plot:
  if(vy.eq.303.or.ce) then
     CE = .true.
     vy = 220
     yy(1,1:nm) = dat(220,1:nm)       ! P(r(m)=Rrl)
     yy(2,1:nm) = dat(221,1:nm)       ! P(post-alphaCE)
     ny = 2
  end if
  
  xx(1:nm) = dat(vx,1:nm)
  yy(1,1:nm) = dat(vy,1:nm)
  
  
  
  
  
  
  !***   LIN/LOG AXES
  
  write(6,'(A)',advance='no')' Do you want a logarithmic scale: (N)o, (X)-axis, (Y)-axis, (B)oth: '
  read*,log
  if(log.eq.'X') log='x'
  if(log.eq.'Y') log='y'
  if(log.eq.'B') log='b'
  if(log.eq.'N') log='n'
  
  if(log.eq.'x'.or.log.eq.'b') then
     if(xx(1).eq.0.) xx(1) = xx(2)
     xx(1:nm) = log10(abs(xx(1:nm))+1.e-20)
  end if
  if(log.eq.'y'.or.log.eq.'b') then
     do i=1,ny
        if(yy(i,1).eq.0.) yy(i,1) = yy(i,2)
     end do
     yy(1:ny,1:nm) = log10(abs(yy(1:ny,1:nm))+1.e-20)
  end if
  
  xmin = minval(xx(1:nm))
  xmax = maxval(xx(1:nm))
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
        inquire(file=trim(psname), exist=ex) !Check whether the file already exists; ex is True or False
        i = i+1
     end do
     call pgbegin(1,trim(psname)//'/cps',1,1)
     !call pgscf(2)
     call pgslw(2)
  else ! Screen
     !call pgbegin(1,'2/xserve',1,1)
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
     if(white_bg) then     !Create a white background; swap black (ci=0) and white (ci=1)
        call pgscr(0,1.,1.,1.)  !For some reason, this needs to be repeated for AquaTerm, see below
        call pgscr(1,0.,0.,0.)
        call pgsci(1)
        call pgsci(0)
        call pgsvp(0.,1.,0.,1.)
        call pgswin(-1.,1.,-1.,1.)
        call pgrect(-2.,2.,-2.,2.)
        call pgsci(1)
     end if
  end if
  
  if(ny.eq.1) then
     call pgsvp(0.06,0.96,0.07,0.96)
  else                                   ! Multiple lines; need room for legend on right-hand side
     call pgsvp(0.06,0.93,0.07,0.96)
  end if

  call pgswin(xmin,xmax,ymin,ymax)
  if(log.eq.'n') call pgbox('BCNTS',0.0,0,'BCNTS',0.0,0)  !Use logarithmic axes rather than logarithmic variables
  if(log.eq.'x') call pgbox('BCLNTS',0.0,0,'BCNTS',0.0,0)
  if(log.eq.'y') call pgbox('BCNTS',0.0,0,'BCLNTS',0.0,0)
  if(log.eq.'b') call pgbox('BCLNTS',0.0,0,'BCLNTS',0.0,0)
  call pgmtxt('T',0.7,0.5,0.5,'~'//trim(title(12:)))  !13 to remove /home/user/
  call pgmtxt('B',2.4,0.5,0.5,lx)
  call pgmtxt('L',2.4,0.5,0.5,ly)
  
  if(vx.ne.201.and.vy.ne.201) then
     do i=1,ny
        call pgsci(colours(mod(i-1,ncolours)+1))
        yy1(1:nm) = yy(i,1:nm)
        call pgline(nm,xx(1:nm),yy1(1:nm))
        if(ab) call pgmtext('RV',0.5,real(ny+1-i)/20.,0.,trim(abds(i)))
        if(nab) call pgmtext('RV',0.5,real(ny+1-i)/20.,0.,trim(nabs(i)))
        if(CE) call pgmtext('RV',0.5,real(ny+1-i)/20.,0.,trim(CEs(i)))
     end do
  else
     do i=1,ny 
        call pgsci(mod(i-1,6)+1)
        call pgpoint(nm,xx(1:nm),yy(i,1:nm),1)
        if(ab) call pgmtext('RV',0.5,real(ny+1-i)/20.,0.,trim(abds(i)))
     end do
  end if
  
  call pgsch(1.5)
  call pgsci(8)
  if(hmp.ne.0) then
     do i=1,ny
        call pgpoint(1,xx(hmp),yy(i,hmp),2)
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
  end if !if(plot.ne.9) then
  write(6,*)''
  write(6,'(A27)',advance='no')' What do you want to do ?  '
  read*,plot
  if(plot.lt.0 .or. plot.gt.8.and.plot.ne.10) goto 900
  
  if(plot.ne.4.and.plot.ne.10) call pgend
  if(plot.eq.1) goto 32
  if(plot.eq.2) goto 37
  if(plot.eq.3) goto 70
  if(plot.eq.6) goto 4
  if(plot.eq.7) goto 3
  if(plot.eq.8) goto 501
  
  if(plot.eq.4) then  !Select region
941  call pgsci(1)
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
     call identify_closest_mdl_model(nn,ny,xx,yy,xmin,xmax,ymin,ymax)
     goto 900
  end if
  
9999 continue
  write(6,'(A,/)')' Program finished'
end program plotmdl
!***********************************************************************************************************************************

