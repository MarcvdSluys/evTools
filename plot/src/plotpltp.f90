!>  Plotpltp: Plots the data contained in star.plt1,2 in different panels
!!  This program reads and plots data from the plot output file from Eggletons code, the TWIN version
!!  The idea is to plot np panels with the same horizontal axis and different vertical axes.
!!  The output is written to ps only.
!!
!!   Copyright 2002-2010 AstroFloyd - astrofloyd.org
!!   
!!   
!!   This file is part of the eggleton-plot package.
!!   
!!   This is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published
!!   by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
!!   
!!   This software is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
!!   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details.
!!   
!!   You should have received a copy of the GNU General Public License along with this code.  If not, see 
!!   <http://www.gnu.org/licenses/>.
!<   

program plotpltp      
  use kinds
  
  implicit none
  integer, parameter :: nn=30000, nvar=100
  real(double) :: dat(nvar,nn)
  real :: xx(nn),yy(nn)
  real :: x,system,xmin,xmax,ymin,ymax,ymin0,ymax0
  real :: px(5000),py(5000)
  real :: pi,sigma,l0,m0,r0,g,day,yr
  real :: p1,p2,p3,p4
  integer :: model,p,np
  
  integer :: i,j,n,vx,vy,plotagain,ncols
  integer :: hrd,dhdt,conv,mdots,tscls,ch
  integer :: plotmode,ps
  character findfile*99, fname*99
  character*10 :: psname*11
  character :: rng,log,cnvh
  character :: labels(nvar)*40,lx*40,ly*40,titel*100
  
  !Set constants:
  call setconstants()
  
  
  ! Switches:
  !-----------
  psname            = 'plot_plt.ps'                         ! Name of the output file
  plotmode          = 1        ! 0 - plot all graphs on 1 sheet only,  1 - as 0, but plots each graph on an additional sheet as well
  ps                = 0                                     ! 0 - plot to screen, 1 - plot to postscript file(s).
  
  
  
  pi        =       4.d0*datan(1.d0)
  sigma     =       5.67051d-5
  l0        =       3.83d33
  r0        =       6.9599d10
  m0        =       1.9891d33
  g         =       6.67259d-8
  day       =       8.64d4
  yr        =       3.15569d7
  
  
  
  
  labels(1) = 'Model'
  labels(2) = 't (yr)'
  labels(3) = '\gDt (yr)'
  labels(4) = 'M (M\d\(2281)\u)'
  labels(5) = 'M\dHe\u (M\d\(2281)\u)'
  labels(6) = 'M\dCO\u (M\d\(2281)\u)'
  labels(7) = 'M\dONe\u (M\d\(2281)\u)'
  labels(8) = 'R (R\d\(2281)\u)'
  labels(9) = 'L (L\d\(2281)\u)'
  labels(10) = 'T\deff\u (K)'
  labels(11) = 'T\dc\u (K)'
  labels(12) = 'T\dmax\u (K)'
  labels(13) = '\gr\dc\u (g cm\u-3\d)'
  labels(14) = '\gr\dTmax\u (g cm\u-3\d)'
  labels(15) = 'log U\dbind\u (erg)'
  labels(16) = 'L\dH\u (L\d\(2281)\u)'
  labels(17) = 'L\dHe\u (L\d\(2281)\u)'
  labels(18) = 'L\dC\u (L\d\(2281)\u)'
  labels(19) = 'L\d\gn\u (L\d\(2281)\u)'
  labels(20) = 'L\dth\u (L\d\(2281)\u)'
  labels(21) = 'P\drot\u (d)'
  labels(22) = 'K\u2\d'
  labels(23) = 'R\dcz\u'
  labels(24) = '\gDR\dcz\u'
  labels(25) = 'T\det\u'
  labels(26) = 'R\dalfven\u'
  labels(27) = 'B\dp\u'
  labels(28) = 'P\dorb\u (d)'
  labels(29) = 'FLR'
  labels(30) = 'F1'
  labels(31) = 'dM/dt (M\d\(2281)\u/yr)'
  labels(32) = 'dM\dwind\u/dt (M\d\(2281)\u/yr)'
  labels(33) = 'dM\dmt\u/dt (M\d\(2281)\u/yr)'
  labels(34) = 'H\dorb\u'
  labels(35) = 'dH\dorb\u/dt'
  labels(36) = 'dH\dgw\u/dt'
  labels(37) = 'dH\dwml\u/dt'
  !labels(38) = 'dH\ds-o\u/dt'
  labels(38) = 'dH\dmb\u/dt'
  labels(39) = 'dH\dmtr\u/dt'
  labels(40) = 'M\dcomp\u'
  labels(41) = 'e'
  labels(42) = 'H\dsurf\u'
  labels(43) = 'He\dsurf\u'
  labels(44) = 'C\dsurf\u'
  labels(45) = 'N\dsurf\u'
  labels(46) = 'O\dsurf\u'
  labels(47) = 'Ne\dsurf\u'
  labels(48) = 'Mg\dsurf\u'
  labels(49) = 'H\dTmax\u'
  labels(50) = 'He\dTmax\u'
  labels(51) = 'C\dTmax\u'
  labels(52) = 'N\dTmax\u'
  labels(53) = 'O\dTmax\u'
  labels(54) = 'Ne\dTmax\u'
  labels(55) = 'Mg\dTmax\u'
  labels(56) = 'H\dcentr\u'
  labels(57) = 'He\dcentr\u'
  labels(58) = 'C\dcentr\u'
  labels(59) = 'N\dcentr\u'
  labels(60) = 'O\dcentr\u'
  labels(61) = 'Ne\dcentr\u'
  labels(62) = 'Mg\dcentr\u'
  labels(73) = '\gt (yr)'
  labels(81) = 'Q\dconv\u'
  
  x=system('pwd > tmppwd.txt')
  open (unit=10,form='formatted',status='old',file='tmppwd.txt')
  rewind 10
  read(10,'(a100)')titel
  close(10)
  x=system('rm tmppwd.txt')
  
5 plotagain = 0
  fname = findfile('*.plt*') !Match string
  
  write(6,*)''
  write(6,'(A)')'Reading file '//trim(fname)
  
  open (unit=10,form='formatted',status='old',file=trim(fname))
  rewind 10
  read(10,*)ncols
  write(6,'(A,I4,A)')'  Reading',ncols,' columns of data'
  do j=1,nn
     read(10,*,err=12,end=11) model, (dat(i,j),i=2,nvar)
     dat(1,j) = model
  end do
  write(6,'(A)')'  End of file reached, arrays too small!'
  close(10)
  goto 15
  
11 write(6,'(A,I4,A)')'  End of the file reached,',j-1,' lines read.'
  close(10)
  goto 15
  
12 write(6,'(A,I4)')'  Error reading file, aborting at line ',j
  write(6,'(A)')"  I'll skip the rest of the file and use the first part."
  close(10)
  
15 continue
  
  n = j-1
  
  write(6,*)''
  
  do i=3,nn !skip t,dt
     if(dat(2,i).eq.0.) dat(2,i) = dat(3,i)
  end do
  
  dat(8,1:n) = 10**dat(8,1:n)
  dat(9,1:n) = 10**dat(9,1:n)
  dat(10,1:n) = 10**dat(10,1:n)
  dat(11,1:n) = 10**dat(11,1:n)
  dat(12,1:n) = 10**dat(12,1:n)
  dat(13,1:n) = 10**dat(13,1:n)
  dat(14,1:n) = 10**dat(14,1:n)
  
  !TRUE Magnetic braking in stead of Spin-orbit coupling:
  dat(38,1:n) = 3.8e-30*dat(4,1:n)*m0*(dat(8,1:n)*r0)**4* (2*pi/(dat(21,1:n)*day))**3/1.d50
  do i=1,nn
     if(dat(81,i).lt.0.02) dat(38,i) = dat(38,i)*exp(1.d0-2.d-2/dat(81,i))
  end do
  
  dat(72,1:n) = g*dat(4,1:n)**2*m0*m0/(dat(8,1:n)*r0*dat(9,1:n)*l0)/yr                 !KH timescale
  dat(73,1:n) = dat(34,1:n)/max(dat(36,1:n)*yr,1.d-30)                      !Gravitational waves
  dat(74,1:n) = dat(34,1:n)/max(abs(dat(38,1:n))*yr,1.d-30)                 !Magnetic braking
  dat(75,1:n) = dat(4,1:n)/max(abs(dat(33,1:n)),1.d-30)                     !Mass transfer
  dat(76,1:n) = dat(4,1:n)*m0/1.9891/(dat(9,1:n)*l0)*4.e10                  !Nuclear timescale
  dat(77,1:n) = dat(34,1:n)/max(abs(dat(35,1:n))*yr,1.d-30)                 !Total dOA
  dat(78,1:n) = dat(34,1:n)/max(dat(39,1:n)*yr,1.d-30)                      !dOA due to Mass loss
  
  
30 continue
  call pgbegin(1,'plotpltp.ps/vps',1,1)
  call pgscf(2)
  
  write(6,'(A48)',advance='no')' How many panels do you want to plot ?   (1-6): '
  read*,np
  
  write(6,*)''
  write(6,'(A)')'Variables:                                    0: Quit    '
  write(6,*)''
  write(6,'(A)')'  1: model                                               '
  write(6,'(A)')'  2: t          16: Lh         28: Porb      34: Horb    '
  write(6,'(A)')'  3: dt         17: Lhe        29: FLR       35: dHorb/dt'
  write(6,'(A)')'  4: M          18: Lc         30: F1        36: dHgw/dt '
  write(6,'(A)')'  5: Mhe        19: Lnu        31: dM        37: dHwml/dt'
  write(6,'(A)')'  6: Mco        20: Lth        32: dMwind    38: dHs-o/dt'
  write(6,'(A)')'  7: Mone       21: Prot       33: dMmt      39: dHmtr/dt'
  write(6,'(A)')'  8: R          22: VK2                      40: Mcomp   '
  write(6,'(A)')'  9: L          23: Rcz                      41: e       '
  write(6,'(A)')' 10: Teff       24: dRcz                     81: Qconv   '
  write(6,'(A)')' 11: Tc         25: Tet                                  '
  write(6,'(A)')' 12: Tmax       26: Ralv                                 '
  write(6,'(A)')' 13: Rhoc       27: Bp         93: dH/dt`s               '
  write(6,'(A)')' 14: RhoTm                     94: Convection plot       '
  write(6,'(A)')' 15: Ubind                     95: Mdots                 '
  write(6,'(A)')'                               96: Timescales            '
  write(6,'(A)')'                                                         '
  write(6,'(A)')'       H   He  C   N   O   Ne  Mg                        '
  write(6,'(A)')' Surf  42  43  44  45  46  47  48  Surf                  '
  write(6,'(A)')' Tmax  49  50  51  52  53  54  55  Tmax                  '
  write(6,'(A)')' Core  56  57  58  59  60  61  62  Core                  '
  write(6,'(A)')'                                                         '
  write(6,*)''
35 write(6,'(A36)',advance='no')' Choose the X-axis variable (1-92): '
  vx = 1
  read*,vx
  if(vx.eq.0) goto 9999
  if(vx.lt.0.or.vx.gt.92) goto 35
  if(vx.gt.61.and.vx.lt.92) goto 35
  
  write(6,'(A51)',advance='no')' Do you want a logarithmic X-axis ?   (Y)es, (N)o: '
  log='n'
  read*,log
  
  !Problem with converting to float: small time ranges after a long time are rounded to the same floating number:
  xx = real(dat(vx,1:nn))  
  lx = labels(vx)
  if(log.eq.'Y'.or.log.eq.'y') then
     if(xx(1).le.0.) xx(1)=xx(2)
     xx(1:n) = log10(abs(xx(1:n)))
     lx = 'log '//trim(lx)
  end if
  
  xmin = minval(xx(1:n))
  xmax = maxval(xx(1:n))
  write(6,'(2(A,ES10.3))')'X-range: ',xmin,' - ',xmax
  
  
  write(6,*)''
  write(6,*)''
  
  
  ! **********************************************************************
  ! *****   START LOOP ON DIFFERENT PANELS                           *****
  ! **********************************************************************
  do p=1,np
     
36   write(6,'(A28,I1,A22)',advance='no')' Choose the Y-axis variable ',p,'   (1-62, 81, 92-96): '
     vy = 2*p
     read*,vy
     if(vy.eq.0) goto 9999
     if(vy.lt.0.or.vy.gt.96) goto 36
     if(vy.gt.62.and.vy.lt.93.and.vy.ne.81) goto 36
     
     hrd   = 0
     dhdt  = 0
     conv  = 0
     mdots = 0
     tscls = 0
     
     
     if(vy.eq.93) then
        dhdt = 1
        vy = 35
     end if
     if(vy.eq.94) then
        conv = 1
        vy = 4
     end if
     if(vy.eq.95) then
        mdots = 1
        vy = 31
     end if
     if(vy.eq.96) then
        tscls = 1
        vy = 72
     end if
     
     !Problem with converting to float: small time ranges after a long time are rounded to the same floating number:
     yy = real(dat(vy,1:nn))
     
     write(6,'(A51)',advance='no')' Do you want a logarithmic Y-axis ?   (Y)es, (N)o: '
     read*,log
     
     ly = labels(vy)
     if(log.eq.'Y'.or.log.eq.'y') then
        if(yy(1).le.0.) yy(1)=yy(2)
        yy(1:n) = log10(abs(yy(1:n)))
        ly = 'log '//trim(ly)
     end if
     
     ymin = minval(yy(1:n))
     ymax = maxval(yy(1:n))
     
     if(dhdt.eq.1) then
        do i=36,39
           if(dat(i,1).le.0.) dat(i,1) = dat(i,2)
        end do
        dat(36:39,1:n) = dlog10(abs(dat(36:39,1:n))+1.d-30)
        ymin = minval(dat(36:39,1:n))
        ymax = maxval(dat(36:39,1:n))
     end if
     
     if(tscls.eq.1) then
        do i=72,77
           if(dat(i,1).le.0.) dat(i,1) = dat(i,2)
        end do
        dat(72:77,1:n) = dlog10(abs(dat(72:77,1:n))+1.d-30)
        ymin = minval(dat(72:77,1:n))
        ymax = maxval(dat(72:77,1:n))
     end if
     
     if(conv.eq.1) ymin = 0.d0
     
     !Limit ranges for logged axes like Mdot
     if(log.eq.'X'.or.log.eq.'x'.or.log.eq.'B'.or.log.eq.'b') then
        if(vx.ge.31.and.vx.le.33.and.xmin.lt.-12.) xmin = -12.
     end if
     if(log.eq.'Y'.or.log.eq.'y'.or.log.eq.'B'.or.log.eq.'b') then
        if(vy.ge.31.and.vy.le.33.and.ymin.lt.-12.) ymin = -12.
        if(vy.ge.35.and.vy.le.39.and.ymin.lt.-18.) ymin = -18.
     end if
     if(vy.eq.72.and.ymin.lt.7.) ymin = 7.
     if(vy.eq.72.and.ymax.gt.12.) ymax = 12.
     
     !      xmin0 = xmin
     !      xmax0 = xmax
     ymin0 = ymin
     ymax0 = ymax
     
     
     write(6,'(A2,I1,A8,ES14.7,A3,ES14.7)')'Y',p,'-range: ',ymin,' - ',ymax
     
     
     
     
     
     
     
     
     
     goto 100  !Skip rescale (at least for x)
     write(6,'(A73)',advance='no')' Do you want to change a plot range ?  (N)o, (X)-axis, (Y)-axis, (B)oth: '
     read*,rng
     
     if(rng.eq.'X'.or.rng.eq.'x'.or.rng.eq.'B'.or.rng.eq.'b') then
        write(6,'(A51)',advance='no')'  Give the new range for the X-axis (Xmin, Xmax): '
        read*,xmin,xmax
        if(xmin.gt.xmax) then
           x = xmin
           xmin = xmax
           xmax = x
           write(6,'(A)')'  Swapped xmin and xmax'
        end if !if(xmin.gt.xmax)
     end if !if(rng.eq.'X'.or.rng.eq.'x'.or.rng.eq.'B'.or.rng.eq.'b')
     
     if(rng.eq.'Y'.or.rng.eq.'y'.or.rng.eq.'B'.or.rng.eq.'b') then
        write(6,'(A51)',advance='no')'  Give the new range for the Y-axis (Ymin, Ymax): '
        read*,ymin,ymax
        if(ymin.gt.ymax) then
           x = ymin
           ymin = ymax
           ymax = x
           write(6,'(A)')'  Swapped ymin and ymax'
        end if !if(ymin.gt.ymax)
     end if !if(rng.eq.'Y'.or.rng.eq.'y'.or.rng.eq.'B'.or.rng.eq.'b')
     
     
     !Limit ranges for logged axes like Mdot
     if(log.eq.'X'.or.log.eq.'x'.or.log.eq.'B'.or.log.eq.'b'.and.p.eq.1) then
        if(vx.ge.31.and.vx.le.33.and.xmin.lt.-12.) xmin = -12.
     end if
     if(log.eq.'Y'.or.log.eq.'y'.or.log.eq.'B'.or.log.eq.'b') then
        if(vy.ge.31.and.vy.le.33.and.ymin.lt.-12.) ymin = -12.
        if(vy.ge.35.and.vy.le.39.and.ymin.lt.-18.) ymin = -18.
     end if
     if(vy.eq.72.and.ymin.lt.2.) ymin = 2.
     if(vy.eq.72.and.ymax.gt.11.) ymax = 11.
     
     write(6,'(2(A,ES10.3))')'X-range: ',xmin,' - ',xmax
     write(6,'(2(A,ES10.3),I6)')'Y-range: ',ymin,' - ',ymax,vy
     
     
     
     
     
     
     
     
     
     
     
     
     
     
100  continue
     if(dhdt.eq.1) then
        write(6,*)''
        write(6,'(A)')'  --------- : total angular momentum loss'
        write(6,'(A)')'  -...-...- : gravitational radiation AM loss'
        write(6,'(A)')'  ......... : wind AM loss'
        !        write(6,'(A)')'  -.-.-.-.- : SO coupling AM loss (MB)'
        write(6,'(A)')'  -.-.-.-.- : magnetic braking AM loss'
        write(6,'(A)')'  - - - - - : non-conservative MT AM loss'
        write(6,*)''
     end if
     
     if(tscls.eq.1) then
        write(6,*)''
        write(6,'(A)')'  --------- : Kelvin-Helmholz timescale'
        write(6,'(A)')'  - - - - - : gravitational radiation timescale'
        write(6,'(A)')'  -.-.-.-.- : magnetic braking timescale'
        write(6,'(A)')'  ......... : mass loss timescale'
        write(6,'(A)')'  -...-...- : nuclear evolution timescale'
        !        write(6,'(A)')'  -...-...- : orbital angular momentum timescale'
        !        write(6,'(A)')'  --------- : OA due to system mass loss timescale'
        write(6,*)''
     end if
     
     if(conv.eq.1) then
        ch = 0
        write(6,'(A42)',advance='no')' Do you want to plot hatches (Y)es/(N)o: '
        read*,cnvh
        if(cnvh.eq.'y'.or.cnvh.eq.'Y') ch = 1
     end if
     
     if(p.eq.1) then
        x = 0.02*abs(xmax-xmin)
        if(hrd.eq.1) x = -1.*x  !HRD
        if(x.eq.0.) x = 0.05*xmax
        xmin = xmin - x
        xmax = xmax + x
     end if
     x = 0.02*abs(ymax-ymin)
     if(x.eq.0.) x = 0.05*ymax
     ymin = ymin - x
     ymax = ymax + x
     
     
     
     ! **********************************************************************      
     ! *****    CREATE PLOT                                             *****
     ! **********************************************************************      
     write(6,*)''     
     !Spread panels equally in y between 0.09 and 0.99, so 0.90/np per panel:
     p1 = 0.08
     p2 = 0.98
     p3 = (p2-p1)/real(np)
     p4 = p2 - p3
     p3 = p3*real(p-1)
     
     call pgsch(0.8)
     call pgsvp(0.07,0.99,p4-p3,p2-p3)
     call pgswin(xmin,xmax,ymin,ymax)
     if(p.ne.np) call pgbox('BCTS',0.0,0,'BCNTS',0.0,0)
     if(p.eq.np) call pgbox('BCNTS',0.0,0,'BCNTS',0.0,0)
     if(p.eq.1)  call pgmtxt('T',0.2,0.5,0.5,titel(13:100))
     if(p.eq.np) call pgmtxt('B',2.2,0.5,0.5,lx)
     call pgmtxt('L',2.2,0.5,0.5,ly)
     
     call pgline(n,xx(1:n),yy(1:n))
     
     if(dhdt.eq.1) then
        do j=36,39
           call pgsls(40-j)
           call pgline(n,xx(1:n),real(dat(j,1:n)))
        end do !j
        call pgsls(1)
     end if !if(dhdt.eq.1)
     
     if(tscls.eq.1) then
        do j=72,76
           if(j.le.76) call pgsls(j-71)
           if(j.gt.76) call pgsls(j-76)
           call pgline(n,xx(1:n),real(dat(j,1:n)))
        end do !j
        call pgsls(1)
     end if !if(tscls.eq.1)
     
     if(conv.eq.1) then
        call pgslw(3)
        call pgline(n,xx(1:n),yy(1:n))
        call pgslw(1)
        call pgsls(2)
        do j=64,67
           call pgline(n,xx(1:n),real(dat(j,1:n)))
        end do !j
        call pgsls(4)
        do j=5,7
           call pgline(n,xx(1:n),real(dat(j,1:n))) !core masses
        end do !j
        call pgsls(2)
        if(ch.eq.1) then
           call pgsls(4)
           i=1
           do j=1,n/10
              px(j) = xx(i)
              py(j) = real(dat(64,i))
              px(n/5-j-1) = xx(i)
              py(n/5-j-1) = real(dat(65,i))
              i = i+10
           end do
           call pgsls(4)
           call pgsfs(4)
           call pgpoly(n/5-2,px(1:n/5-2),py(1:n/5-2))
           call pgsls(1)
        end if !if(ch.eq.1)
     end if !if(conv.eq.1)
     
     if(mdots.eq.1) then
        call pgsls(2)
        do j=32,33
           call pgline(n,xx(1:n),real(dat(j,1:n)))
        end do !j
     end if !if(mdots.eq.1)
     
     if(vy.eq.81) then !Qconv-plot
        call pgsls(4)
        call pgline(2,(/xmin,xmax/),(/0.02,0.02/))
        call pgsls(1)
     end if
     
     
     
  end do !p: different panels
  
  call pgend
  
  
  
  
  
200 write(6,*)''
  write(6,'(A)')'Do you want to:'
  write(6,'(A)')'  0) quit'
  write(6,'(A)')'  1) change variables'
  !      write(6,'(A)')'  2) change lin/log axes'
  !      write(6,'(A)')'  3) change axis ranges'
  !      write(6,'(A)')'  4) reread file and make same plot'
  write(6,'(A)')'  5) change input file'
  write(6,*)''
  read*,plotagain
  
  if(plotagain.eq.1) goto 30
  !      if(plotagain.eq.2) goto 37
  !      if(plotagain.eq.3) goto 70
  !      if(plotagain.eq.4) goto 7
  if(plotagain.eq.5) goto 5
  if(plotagain.lt.0.or.plotagain.gt.5) goto 200
  
9999 write(6,'(A)')'Program finished'
  write(6,*)''
end program plotpltp
