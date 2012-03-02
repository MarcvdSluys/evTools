!> \file plotmod.f90  Reads an input or output structure model file from ev and lists the properties 
!!                    of each model it contains.


! One can select a model to plot its contents.
! Adapted from listmod.f90
! AF 2004-08-05


! Copyright 2002-2012 AstroFloyd - astrofloyd.org
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



!> \brief Plot the contents of the *.mod output file of ev
program plotmod
  use kinds
  use constants
  
  implicit none
  integer, parameter :: nc=25,nm=1000
  real(double) :: m1,dt,t,p,bms,ecc,p1,enc,horb
  real(double) :: lnf,lnt,x16,lnm,x1,lnr,l,x4,x12,x20
  real(double) :: mi,pr,phi,phis,e,f,x
  real(double) :: r1,l1,ts,tc,hc,hec,zc
  real(double) :: mhe,mco
  integer :: i,j,kh,kp,jmod,jb,jin,n
  integer :: narg,blk
  character :: fname*(99),findfile*(99)
  
  real :: dat(nc,nm),dat1(nc,nm)
  real :: xmin,xmax,ymin,ymax,xmin0,xmax0,ymin0,ymax0
  integer :: vx,vy,hmp,plotagain,system,status
  character :: log,ans
  character :: labels(nc)*(60),lx*(60),ly*(60),title*(100)
  
  call setconstants()
  write(6,*)
  call print_code_version(6)  !To screen
  call evTools_settings()
  
  plotagain = 0
  status = system('pwd > tmppwd.txt')
  open (unit=10,form='formatted',status='old',file='tmppwd.txt')
  rewind 10
  read(10,'(a100)')title
  close(10)
  status = system('rm tmppwd.txt')

  
  narg = command_argument_count()
  if(narg.eq.0) then  !Search for input file in current dir
     fname = findfile('*.mod*')
  else if(narg.eq.1) then
     call get_command_argument(1,fname)
  else
     write(6,'(A)')' Plotmod: plots the contents of a structure model file'
     write(6,'(A)')'          syntax:  plotmod <filename>'
     write(6,'(A)')'              or:  plotmod   to look for a .mod file in the current directory'
     goto 9999
  end if



  ! READ FILE AND LIST MODELS IN FILE

2 write(6,*)''
  write(6,'(A)')' Reading file '//trim(fname)
  open (unit=10,form='formatted',status='old',file=trim(fname))
  rewind 10

  write(6,'(A)')' Nr  Model  Nmesh         Age        dT      M1     Mhe     Mco         R        L     Teff       Tc'//  &
       '      Xc     Yc     Zc    Mtot      Porb       e      Prot'
  do i=1,999
     read(10,*,err=5,end=10)m1,dt,t,p,bms,ecc,p1,enc,kh,kp,jmod,jb,jin
     mhe = 0.d0
     mco = 0.d0
     ts = 1.d0
     do j=1,kh
        read(10,*,err=6,end=10)lnf,lnt,x16,lnm,x1,c,lnr,l,x4,x12,x20,mi,pr,phi,phis,x,horb,e,f,x,x,x,x,x
        if(j.eq.1) then
           r1  = exp(lnr)*1.e11/r0
           l1  = l*1.d33/l0
           ts  = exp(lnt)
        end if
        if(j.eq.kh) then
           tc  = exp(lnt)
           hc  = x1
           hec = x4
           zc  = 1.d0 - x1 - x4
        end if
        if(mhe.eq.0.0.and.x1.lt.0.1) mhe = lnm*1.d33/m0
        if(mco.eq.0.0.and.x4.lt.0.1) mco = lnm*1.d33/m0
     end do !j
     write(6,'(I3,2I7,ES12.4,ES10.2,3F8.3,1x,4ES9.2,1x,3F7.4,2(F8.3,ES10.2))') &
          i,jmod,kh,t,dt,m1,mhe,mco,r1,l1,ts,tc,hc,hec,zc,bms,p,ecc,p1
  end do !i
  write(6,'(A)')'  EOF not reached, array too small!'
  n=999
  goto 12
5 write(6,'(A35,I3)')'  Error reading first line of block',i
  goto 10
6 write(6,'(A36,I3)')'  Error reading second line of block',i
10 n=i-1
  write(6,'(A)')' Nr  Model  Nmesh         Age        dT      M1     Mhe     Mco         R        L     Teff       Tc'//  &
       '      Xc     Yc     Zc    Mtot      Porb       e      Prot'
  write(6,'(I3,A)')n,' blocks read.'
12 if(n.eq.0) goto 999
  write(6,*)''
  blk = 1
  if(n.eq.1) goto 21
20 write(6,'(A37,I2,A15)',advance='no')' Which block do you want to plot (1 -', n,'), 0 to quit: '
  read*,blk
  if(blk.eq.0) goto 9999
  if(blk.lt.1.or.blk.gt.n) goto 20



21 continue  


  labels(1)='\u\(2263) surface\d    Mesh point    \ucentre \(2261)'
  labels(2) = 'M (M\d\(2281)\u)'
  labels(3) = 'R (R\d\(2281)\u)'
  labels(4) = 'T (K)'
  labels(5) = '\(2143) (g cm\u-3\d)'
  labels(6) = 'L (L\d\(2281)\u)'
  labels(7) = 'Degeneracy'
  labels(8) = '\(2047)\dgrav+centr\u'
  labels(9) = 'Mass flux (M\d\(2281)\u yr\u-1\d?)'
  labels(10) = 'Moment of inertia (10\u55\d g cm\u2\d)'
  labels(11) = 'H abundance'
  labels(12) = 'He abundance'
  labels(13) = 'C abundance'
  labels(14) = 'O abundance'
  labels(15) = 'Ne abundance'


  ! READ AND PLOT SELECTED MODEL

  rewind 10
  do i=1,blk-1
     read(10,*,err=991)m1,dt,t,p,bms,ecc,p1,enc,kh,kp,jmod,jb,jin
     do j=1,kh
        read(10,*,err=993)lnf,lnt,x16,lnm,x1,c,lnr,l,x4,x12,x20,mi,pr,phi,phis,x,horb,e,f,x,x,x,x,x
     end do !j
  end do !i

  read(10,*,err=991)m1,dt,t,p,bms,ecc,p1,enc,kh,kp,jmod,jb,jin   !jin = # columns
  !      read(10,*,err=993)lnf,lnt,x16,lnm,x1,c,lnr,l,x4,x12,x20,mi,pr,phi,phis,x,horb,e,f,x,x,x,x,x
  do j=1,kh
     read(10,*,err=993) (dat1(i,j),i=2,25)
     dat(1,j) = real(j)
     dat1(2,j) = exp(dat1(2,j))
     dat1(3,j) = exp(dat1(3,j))
     dat1(5,j) = dat1(5,j)*1.d33/m0
     dat1(8,j) = exp(dat1(8,j))*1.e11/r0
     dat1(9,j) = dat1(9,j)*1.d33/l0
     dat1(20,j) = dat1(20,j)*1.d33/m0*yr
  end do

  !Make more sensible array, without 'eigenvalues' etc
  dat(2,:) = dat1(5,:) !M
  dat(3,:) = dat1(8,:) !R
  dat(4,:) = dat1(3,:) !T
  dat(6,:) = dat1(9,:) !L
  dat(7,:) = dat1(2,:) !Fdegeneracy
  dat(8,:) = dat1(15,:) !Phi - centr-grav potential
  dat(9,:) = dat1(20,:) !Mass flux
  dat(10,:) = dat1(13,:) !Moment of inertia

  dat(11,:) = dat1(6,:) !H
  dat(12,:) = dat1(10,:) !He
  dat(13,:) = dat1(11,:) !C
  dat(14,:) = dat1(4,:) !O
  dat(15,:) = dat1(12,:) !Ne

  !Mass density
  do i=2,kh
     dat(5,i) = (dat(2,i)-dat(2,i-1))*m0/(4.d0*pi/3.d0*(dat(3,i)**3-dat(3,i-1)**3)*r0**3)
  end do
  dat(5,1) = dat(5,2)


32 continue   
  write(6,*)''
  write(6,'(A)')' Variables:                               0: Quit '
  write(6,*)''
  write(6,'(A)')'   1: Mesh point                       Abundances:'
  write(6,'(A)')'   2: Mass                               11: H    '
  write(6,'(A)')'   3: Radius                             12: He   '
  write(6,'(A)')'   4: Temperature                        13: C    '
  write(6,'(A)')'   5: Density                            14: O    '
  write(6,'(A)')'   6: Luminosity                         15: Ne   '
  write(6,'(A)')'   7: Fdegeneracy                                 '
  write(6,'(A)')'   8: Potential (grav+centr)                      '
  write(6,'(A)')'   9: Mass flux                                   '
  write(6,'(A)')'  10: Moment of inertia                           '
  write(6,*)''



35 write(6,'(A36)',advance='no')' Choose the X-axis variable (1-15): '
  read*,vx
  if(vx.eq.0) goto 9999
  if(vx.lt.1.or.vx.gt.15) goto 35
36 write(6,'(A36)',advance='no')' Choose the Y-axis variable (1-15): '
  read*,vy
  if(vy.eq.0) goto 9999
  if(vy.lt.1.or.vy.gt.15) goto 36

37 write(6,'(A68)',advance='no')' Do you want a logarithmic scale: (N)o, (X)-axis, (Y)-axis, (B)oth: '
  read*,log

  lx = labels(vx)
  ly = labels(vy)

  if(log.eq.'x'.or.log.eq.'X'.or.log.eq.'b'.or.log.eq.'B') then
     if(dat(vx,1).eq.0.) dat(vx,1)=dat(vx,2)
     dat(vx,1:kh) = log10(abs(dat(vx,1:kh))+1.e-20)
     lx = trim('log '//lx)
  end if
  if(log.eq.'y'.or.log.eq.'Y'.or.log.eq.'b'.or.log.eq.'B') then
     if(dat(vy,1).eq.0.) dat(vy,1)=dat(vy,2)
     dat(vy,1:kh) = log10(abs(dat(vy,1:kh))+1.e-20)
     ly = trim('log '//ly)
  end if

  xmin = minval(dat(vx,1:kh))
  xmax = maxval(dat(vx,1:kh))
  ymin = minval(dat(vy,1:kh))
  ymax = maxval(dat(vy,1:kh))

  xmin0 = xmin
  xmax0 = xmax
  ymin0 = ymin
  ymax0 = ymax

70 write(6,*)''
  write(6,*)' X-range:',xmin,'-',xmax
  write(6,*)' Y-range:',ymin,'-',ymax
  write(6,'(A66)',advance='no')' Do you want to change a plot range ?  (N)o, (X)-axis, (Y)-axis:  '
  read*,ans

  if(ans.eq.'N'.or.ans.eq.'n'.or.ans.eq.' ') goto 100


  if(ans.eq.'X'.or.ans.eq.'x') then
     write(6,'(A49)',advance='no')' Give the new range for the X-axis (Xmin, Xmax): '
     read*,xmin,xmax
     if(xmin.lt.xmin0) xmin = xmin0
     if(xmax.gt.xmax0) xmax = xmax0
     ymin=1.e33
     ymax=-1.e33
     write(6,'(I5)')kh
     do i=1,kh
        if(dat(vx,i).ge.xmin.and.dat(vx,i).le.xmax) then
           if(dat(vy,i).lt.ymin) ymin = dat(vy,i)
           if(dat(vy,i).gt.ymax) ymax = dat(vy,i)
        end if
     end do !i
  end if

  if(ans.eq.'Y'.or.ans.eq.'y') then
     write(6,'(A49)',advance='no')' Give the new range for the Y-axis (Ymin, Ymax): '
     read*,ymin,ymax
     if(ymin.lt.ymin0) ymin = ymin0
     if(ymax.gt.ymax0) ymax = ymax0
     xmin=1.E38
     xmax=-1.E38
     do i=1,kh
        if(dat(vy,i).ge.ymin.and.dat(vy,i).le.ymax) then
           if(dat(vx,i).lt.xmin) xmin = dat(vx,i)
           if(dat(vx,i).gt.xmax) xmax = dat(vx,i)
        end if
     end do !i
  end if


  write(6,*)''
  write(6,*)' X-range:',xmin,'-',xmax
  write(6,*)' Y-range:',ymin,'-',ymax


100 continue
  x = 0.02*abs(xmax-xmin)
  if(x.eq.0.) x = 0.05*xmax
  xmin = xmin - x
  xmax = xmax + x
  x = 0.02*abs(ymax-ymin)
  if(x.eq.0.) x = 0.05*ymax
  ymin = ymin - x
  ymax = ymax + x

  hmp = 999
  !      if(vx.eq.1) then
  do while(hmp.gt.kh)
     write(6,'(A28,I4,A3)',advance='no')' Highlight a mesh point (1 -',kh,'): '
     read*,hmp
     !if(hmp.gt.kh) goto 111
     if(hmp.lt.1) hmp=0
     !      end if
  end do
  
  !Add block number to plot title:
  write(title,'(A,I6)')trim(title),blk
  
201 continue
  if(plotagain.eq.5) then
     call pgbegin(1,'plot_mod.eps/cps',1,1)
     call pgscf(2)
  else
     call pgbegin(1,'/xserve',1,1)
     call pgpap(scrsz,scrrat)
     call pgscf(1)
     if(white_bg) then           !Create a white background; swap black (ci=0) and white (ci=1)
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
  call pgslw(1)
  call pgenv(xmin,xmax,ymin,ymax,0,0)
  call pglabel(lx,ly,trim(title(13:100)))
  if(plotagain.eq.5) call pgslw(2)
  
  !Plot line:
  call pgsci(2)
  !if(vx.eq.1.or.vy.eq.1) then
  !   call pgpoint(kh,dat(vx,1:kh),dat(vy,1:kh),1)
  !else
  !   call pgline(kh,dat(vx,1:kh),dat(vy,1:kh))
  !end if
  call pgline(kh,dat(vx,1:kh),dat(vy,1:kh))
  call pgsci(1)
  call pgpoint(kh,dat(vx,1:kh),dat(vy,1:kh),1)
  
  call pgsch(1.5)
  call pgsci(4)
  if(hmp.ne.0) call pgpoint(1,dat(vx,hmp),dat(vy,hmp),2)
  call pgsch(1.)
  call pgsci(1)
  call pgend
  



200 write(6,*)''
  write(6,'(A)')' You can:'
  write(6,'(A)')'   0) quit'
  write(6,'(A)')'   1) change variables'
  write(6,'(A)')'   2) change lin/log axes'
  write(6,'(A)')'   3) change axis ranges'
  write(6,'(A)')'   4) change structure model'
  write(6,'(A)')'   5) save plot as postscript'
  write(6,'(A)',advance='no')' What would you like to do:  '
  read*,plotagain
  write(6,*)''

  if(plotagain.eq.0) goto 999
  if(plotagain.eq.1) goto 32
  if(plotagain.eq.2) goto 37
  if(plotagain.eq.3) goto 70
  if(plotagain.eq.4) goto 2
  if(plotagain.eq.5) goto 201
  goto 200
  
  
  
  
  
  
  goto 999
991 write(6,'(A)')' Error reading first line'
  goto 999
993 write(6,'(A)')' Error reading second line'
  
999 close(10)
9999 write(6,*)''
end program plotmod




