!Plots the data contained in a plt* file, highlight selected points
!Lines are longer than 72 chars, so add --wide (lf) or -132 (ifort) to compile
!Uses code in functions.f
!Requires the file ~/usr/lib/UBVRI.Kur to calculate colours
!Uses PGPLOT window 1 to plot to
!AF, 19-04-2006. Works for ifort on MacOS, 11-10-2006.
!
!   Copyright 2002-2009 AstroFloyd - astrofloyd.org
!   
!   
!   This file is part of the eggleton-plot package.
!   
!   This is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by
!   the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
!   
!   This software is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
!   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details.
!   
!   You should have received a copy of the GNU General Public License along with this code.  If not, see <http://www.gnu.org/licenses/>.



program plotplt
  use constants
  use ubvdata
  
  implicit none
  integer,parameter :: nmax=10000,nvar=229,nc=81,nl=7,nfmax=50
  real*8 :: d(nvar)
  
  integer, allocatable :: n(:), strmdls(:,:),hp(:,:),nhp(:)
  real, allocatable :: xx(:,:),yy(:,:),miny(:),excly(:)
  real*8, allocatable :: dat(:,:,:),datf(:,:)
  
  real :: yy1(nmax),minx,dist,mindist
  real :: x,y,system,xmin,xmax,ymin,ymax,dx,dy,xmin0,xmax0,ymin0,ymax0
  real :: xsel(4),ysel(4),xc,yc,xm,ym
  
  integer :: f,nf,nfi,i,i0,j,pl0,vx,vy,plot,npl,pl,drawlines,version,verbose
  integer :: hrd,djdt,conv,mdots,tscls,ch,dpdt,sabs,tabs,cabs,io
  integer :: nt,wait,lums,lgx,lgy,nsel,os,whitebg
  integer :: ansi,xwini,pgopen,defvar(0:nvar)
  integer :: colours(29),ncolours,col,lng
  real :: sch
  
  character :: fname*99,fnames(nfmax)*99,psname*99
  character :: rng,log,hlp,hlp1,hlbl,hlbls*5,leglbl(29)*29
  character :: xwin*19,tmpstr,boxx*19,boxy*19
  character :: pglabels(nvar)*99,asclabels(nvar)*99,pglx*99,pgly*99,title*99,title1*99
  character :: pstitle*99,asclx*99,ascly*99
  logical :: ex,prleg
  
  !Set constants:
  call setconstants()
  
  sch = 1.0
  
  !os = getos() !1-Linux, 2-MacOS
  os = 1       !Don't use Mac OS's silly AquaTerm (watta mistaka to maka)
  whitebg = 1  !0: black background on screen, 1: white
  drawlines = 1 !0: no; draw points, 1: yes: draw lines, 2: draw both
  
  !Line colours:
  ncolours = 13 !Number of colours used to distinguish tracks.  Default: 13
  colours(1:ncolours) = (/2,3,4,5,6,8,9,10,11,12,13,7,1/)  !Use black as last resort
  
  !Read atmosphere-model data
  open(unit=10, file=trim(homedir)//'/usr/lib/UBVRI.Kur',status='old',action='read',iostat=io)
  if(io.eq.0) then
     read(10,*)tmpstr
     read(10,*)tmpstr
     read(10,*)ubv
     close(10)
  else
     write(6,'(A)')" Warning:  I can't find the file ~/usr/lib/UBVRI.Kur, so I can't calculate colours and magnitudes..."
  end if
  
  
  !Read current path and use it as plot title
  x=system('pwd > '//trim(homedir)//'/tmppwd.txt')
  open(unit=10,form='formatted',status='old',file=trim(homedir)//'/tmppwd.txt')
  rewind 10
  read(10,'(a99)')title
  close(10)
  x=system('rm '//trim(homedir)//'/tmppwd.txt')
  do i=10,99
     if(title(i:i).ne.' ') nt = i
  end do
  
  
  plot = 0
  xwini = 1  !Number of X window to try first
5 continue
  
  !Search for input files in current dir:
  nf = command_argument_count()
  if(nf.ge.1.and.plot.eq.0) then
     do f=1,nf
        call get_command_argument(f,fname)
        fnames(f) = fname
     end do
  else
     !fname = findfile('*.plt*') !Match string
     !if(fname(1:10).eq.'          ') goto 9999
     call findfiles('*.plt*',nfmax,0,fnames,nf)   !all=0
     if(nf.le.0) goto 9999
  end if
  plot = 1
  
  
  !Allocate arrays:
  npl = max(nf,nl)
  
  !> \todo allocate nf iso npl in dat()?
  allocate(dat(npl,nvar,nmax), datf(nvar,nmax))
  allocate(n(npl), strmdls(npl,nmax))
  allocate(xx(npl,nmax), yy(npl,nmax), miny(npl), excly(npl))
  allocate(hp(npl,1000), nhp(npl))
  
  call getpltlabels(nf,nvar,pglabels,asclabels,defvar)                                                               !Get the labels for the plot axes; defvar = 0 for non-defined variables
  
  
  !************************************************************************      
  !***   READ THE INPUT FILE
  !************************************************************************      
  
7 if(plot.ne.7) write(6,'(/,A)')' Reading file '//trim(fname)
  
  verbose = 1
  if(plot.eq.7) verbose = 0
  do f=1,nf
     call readplt(10,trim(fnames(f)),nmax,nvar,nc,verbose,datf,nfi,version)  !Use unit 10
     if(version.eq.2005) strmdls(f,:) = nint(datf(83,:)) !Structure model was saved (1) or not (0)
     call changepltvars(nmax,nvar,nfi,datf,pglabels,dpdt)  !Change (e.g. de-log) and add plot variables
     dat(f,:,:) = datf(:,:)
     n(f) = nfi
  end do
  
  
  !************************************************************************      
  !***   CHOOSE PLOT VARIABLES
  !************************************************************************      
  
30 if(plot.ne.6.and.plot.ne.7) then      
     call printpltvarlist(nf)  !Print the list of variables in a *.plt? file to screen, for input menu
     
     
35   write(6,'(A,$)')'  Choose the X-axis variable: '
     read*,vx
     if(vx.eq.0) goto 9999
     if(vx.lt.0.or.vx.gt.201) goto 35
     if(defvar(vx).eq.0) goto 35
     
     hrd   = 0
     djdt  = 0
     conv  = 0
     mdots = 0
     tscls = 0
     lums  = 0
     sabs  = 0
     tabs  = 0
     cabs  = 0
  end if   !if(plot.ne.6.and.plot.ne.7) then   
  
  
  npl = nf  !npl is the number of curves that will be plotted.  This can be >1 because nf>1, or because nf=1, but we plot >1 variable
  prleg = .false.  !Don't print legenda by default
  if(nf.gt.1) prleg = .true.
  
  if(vx.eq.201.or.hrd.eq.1) then  !HRD
     hrd = 1
     do pl=1,npl
        xx(pl,1:n(pl)) = real(log10(abs(dat(pl,10,1:n(pl) ))))
        yy(pl,1:n(pl)) = real(log10(abs(dat(pl, 9,1:n(pl) ))))
     end do
     pglx = trim(pglabels(10))
     pgly = trim(pglabels(9))
     asclx = 'HRD'
     ascly = 'HRD'
     vy = 0
     lgx = 1
     lgy = 1
     goto 50
  end if
  
  if(plot.lt.2) then      
36   write(6,'(A,$)')'  Choose the Y-axis variable: '
     read*,vy
     if(vy.eq.0) goto 9999
     if(vy.lt.0) goto 36
     if(defvar(vy).eq.0) goto 36
     if(vy.eq.201) goto 36   !Can't take HRD as y-variable
  end if   !if(plot.lt.2) then   
  
  
37 continue
  
  if(nf.eq.1) then  !Then you can do multi-variable plots
     f = 1
     
     
     if(vy.eq.25) then !Tet + analytic Tet
        npl = 2
        yy(2,1:nmax) = real(dat(f,123,1:nmax))
     end if
     if(vy.eq.127) then !Rrl
        npl = 2
        yy(2,1:nmax) = real(dat(f,8,1:nmax))
     end if
     if(vy.eq.202.or.conv.eq.1) then  !Convection plot
        conv = 1
        vy = 4
     end if
     if(vy.eq.221) then  !dJ/dt
        djdt = 1
        npl = 5
        yy(1:5,1:nmax) = real(dat(f,35:39,1:nmax))
        leglbl(1:5) = (/'dJ\dtot\u','dJ\dGW\u ','dJ\dSMB\u','dJ\dRMB\u','dJ\dML\u '/)
        prleg = .true.
     end if
     if(vy.eq.222) then !Mdots
        mdots = 1
        npl = 3
        yy(1:3,1:nmax) = real(dat(f,31:33,1:nmax))
        leglbl(1:3) = (/'dM\dtot\u ','dM\dwind\u','dM\dMT\u  '/)
        prleg = .true.
     end if
     if(vy.eq.211) then !Timescales
        tscls = 1
        npl = 5
        yy(1:5,1:nmax) = real(dat(f,201:205,1:nmax))
        leglbl(1:5) = (/'\(0645)\dnuc\u ','\(0645)\dth\u  ','\(0645)\dML\u  ','\(0645)\dGW\u  ','\(0645)\ddyn\u '/)  !Line labels for Timescales plot
        npl = 6
        yy(6,1:nmax) = real(dat(f,119,1:nmax))
        leglbl(6) = '\(0645)\ddR/dt\u  '
        prleg = .true.
     end if
     if(vy.eq.212) then !Luminosities
        lums = 1
        npl = 6
        yy(1,1:nmax) = real(dat(f,9,1:nmax))
        yy(2:6,1:nmax) = real(dat(f,16:20,1:nmax))
        leglbl(1:npl) = (/'L\dsurf\u  ','L\dH\u     ','L\dHe\u    ','L\dC\u     ','L\d\(639)\u','L\dth\u    '/)  !Line labels for Luminosities plot
        prleg = .true.
     end if
     
     if(vy.eq.213) then !Surface abundances
        sabs = 1
        npl = 7
        yy(1:npl,1:nmax) = real(dat(f,42:48,1:nmax))
        leglbl(1:npl) = (/'H ','He','C ','N ','O ','Ne','Mg'/)                                                  !Line labels for Abundances plots
        prleg = .true.
     end if
     if(vy.eq.214) then !Tmax abundances
        tabs = 1
        npl = 7
        yy(1:npl,1:nmax) = real(dat(f,49:55,1:nmax))
        leglbl(1:npl) = (/'H ','He','C ','N ','O ','Ne','Mg'/)                                                  !Line labels for Abundances plots
        prleg = .true.
     end if
     if(vy.eq.215) then !Core abundances
        cabs = 1
        npl = 7
        yy(1:npl,1:nmax) = real(dat(f,56:62,1:nmax))
        leglbl(1:npl) = (/'H ','He','C ','N ','O ','Ne','Mg'/)                                                  !Line labels for Abundances plots
        prleg = .true.
     end if
     
     do pl=1,npl
        n(pl) = n(1)
        xx(pl,1:nmax) = real(dat(1,vx,1:nmax))
        if(vy.lt.200) yy(pl,1:nmax) = real(dat(pl,vy,1:nmax))  
     end do
     
  else !if(nf.ne.1)
     
     do pl=1,npl
        xx(pl,1:nmax) = real(dat(pl,vx,1:nmax))
        yy(pl,1:nmax) = real(dat(pl,vy,1:nmax))
     end do
     
  end if  !if(nf.eq.1) / else
  
  
  !do pl=1,npl
  !   xx(pl,1:nmax) = real(dat(pl,vx,1:nmax))         
  !   !if(vy.lt.200) yy(1,1:n) = real(dat(pl,vy,1:nmax))  
  !   if(vy.lt.200) yy(pl,1:nmax) = real(dat(pl,vy,1:nmax))  
  !end do
  
  
  
  
  
  
  
  !************************************************************************      
  !***   LIN/LOG AXES
  !************************************************************************      
  if(plot.ne.6.and.plot.ne.7) then      
     write(6,'(A,$)')' Do you want a logarithmic scale:  (N)o, (X)-axis, (Y)-axis, (B)oth: '
     read*,log
     if(log.eq.'X') log='x'
     if(log.eq.'Y') log='y'
     if(log.eq.'B') log='b'
     if(log.eq.'N') log='n'
  end if  !if(plot.ne.6.and.plot.ne.7) then   
  
  lgx = 0
  lgy = 0
  if(log.eq.'x'.or.log.eq.'b') lgx = 1
  if(log.eq.'y'.or.log.eq.'b') lgy = 1
  
  
  pglx = pglabels(vx)
  pgly = pglabels(vy)
  asclx = asclabels(vx)
  ascly = asclabels(vy)
  
  if(lgx.eq.1) then
     do pl=1,npl
        if(xx(pl,1).le.0.) xx(pl,1) = xx(pl,2)
     end do
     minx = 1.e33
     do pl=1,npl
        do j=1,n(pl)
           if(abs(xx(pl,j)).lt.minx.and.abs(xx(pl,j)).ne.0.) minx = abs(xx(pl,j))
        end do
        xx(pl,1:n(pl)) = log10(abs(xx(pl,1:n(pl)))+minx*1.e-3)
     end do
  end if
  
  if(djdt.eq.1) lgy = 1
  excly = 0
  if(lgy.eq.1) then
     do pl=1,npl
        if(yy(pl,1).eq.0.) yy(pl,1) = yy(pl,2)
        miny(pl) = 1.e33
        do j=1,n(pl)
           if(abs(yy(pl,j)).lt.miny(pl).and.abs(yy(pl,j)).ne.0.) miny(pl) = abs(yy(pl,j))
        end do
        yy(pl,1:n(pl)) = log10(abs(yy(pl,1:n(pl)))+miny(pl)*1.e-3)
        if(abs(miny(pl)-1.e33).lt.1e32) excly(pl) = 1  !Exclude it in determining ranges
     end do !pl
  end if
  
  
  
50 continue !HRD   
  !xmin = minval(xx(f,1:n))
  !xmax = maxval(xx(f,1:n))
  xmin = 1.e33
  xmax = -1.e33
  do pl=1,npl
     !if(exclx(pl).eq.1) cycle
     xmin = min(minval(xx(pl,1:n(pl))),xmin)
     xmax = max(maxval(xx(pl,1:n(pl))),xmax)
  end do
  
  ymin = 1.e33
  ymax = -1.e33
  do pl=1,npl
     if(excly(pl).eq.1) cycle
     ymin = min(minval(yy(pl,1:n(pl))),ymin)
     ymax = max(maxval(yy(pl,1:n(pl))),ymax)
  end do
  
  if(vx.eq.119) then !R/(dR/dt)
     !if(xmin.lt.1.e4.and.lgx.eq.0) xmin = 1.e4
     !if(xmin.lt.4..and.lgx.eq.1) xmin = 4.
     if(xmax.gt.1.e12.and.lgx.eq.0) xmax = 1.e12
     if(xmax.gt.12..and.lgx.eq.0) xmax = 12.
  end if
  if(vy.eq.119) then !R/(dR/dt)
     !if(ymin.lt.1.e4.and.lgy.eq.0) ymin = 1.e4
     !if(ymin.lt.4..and.lgy.eq.1) ymin = 4.
     if(ymax.gt.1.e12.and.lgy.eq.0) ymax = 1.e12
     if(ymax.gt.12..and.lgy.eq.1) ymax = 12.
  end if

  if(lums.eq.1.and.lgy.eq.1) ymin = max(ymin,ymax-10.)
  
  if(conv.eq.1) ymin = 0.d0
  
  
  
  !Limit ranges for logged axes like Mdot
  if(lgx.eq.1) then
     if(vx.ge.31.and.vx.le.33.and.xmin.lt.-12.) xmin = -12.
  end if
  if(lgy.eq.1) then
     if(vy.ge.31.and.vy.le.33.and.ymin.lt.-12.) ymin = -12.
     if(vy.eq.222.and.ymin.lt.-12.) ymin = -12.
     if((vy.ge.35.and.vy.le.39).or.vy.eq.221) then
        if(ymin.lt.-18..and.dpdt.eq.0) ymin = -18.
        if(ymin.lt.-15..and.dpdt.eq.1) ymin = -15.
        if(ymax.gt.12..and.dpdt.eq.2) ymax = 12.
     end if
  end if
  
  if(vy.eq.211.and.lgy.eq.1.and.ymin.lt.1.) ymin = 1.
  if(vy.eq.211.and.lgy.eq.1.and.ymax.gt.15.) ymax = 15.
  if(vy.eq.122.and.ymin.lt.-20.) ymin = -20.
  
  
  
  
  
  
  
  
  
  !************************************************************************      
  !***   PLOT RANGE
  !************************************************************************      
  
  xmin0 = xmin
  xmax0 = xmax
  ymin0 = ymin
  ymax0 = ymax
  
  
70 if(plot.ne.6.and.plot.ne.7) then      
     write(6,*)''
     write(6,*)' X-range:',xmin,'-',xmax
     write(6,*)' Y-range:',ymin,'-',ymax
     write(6,'(A,$)')' Do you want to change a plot range ?  (N)o, (X)-axis, (Y)-axis, (B)oth: '
     read*,rng
     
     if(rng.eq.'N') rng='n'
     if(rng.eq.'X') rng='x'
     if(rng.eq.'Y') rng='y'
     if(rng.eq.'B') rng='b'
     
     if(rng.eq.'n'.or.rng.eq.' ') goto 100
     
     if(rng.eq.'x'.or.rng.eq.'b') then
        write(6,'(A51,$)')'  Give the new range for the X-axis (Xmin, Xmax): '
        read*,xmin,xmax
        if(xmin.gt.xmax) then
           x = xmin
           xmin = xmax
           xmax = x
           write(6,'(A)')'  I swapped Xmin and Xmax'
        end if !if(xmin.gt.xmax)
     end if !if(rng.eq.'x'.or.rng.eq.'b')
     
     
     if(rng.eq.'y'.or.rng.eq.'b') then
        write(6,'(A51,$)')'  Give the new range for the Y-axis (Ymin, Ymax): '
        read*,ymin,ymax
        if(ymin.gt.ymax) then
           x = ymin
           ymin = ymax
           ymax = x
           write(6,'(A)')'  I swapped Ymin and Ymax'
        end if !if(ymin.gt.ymax)
     end if !if(rng.eq.'y'.or.rng.eq.'b')
  end if  !if(plot.ne.6.and.plot.ne.7) then   
  
  
  if(plot.ne.7) write(6,*)''  
  !Limit ranges for logged axes like Mdot
  if(lgx.eq.1) then
     if(vx.ge.31.and.vx.le.33.and.xmin.lt.-12.) xmin = -12.
  end if
  if(lgy.eq.1) then
     if(vy.ge.31.and.vy.le.33.and.ymin.lt.-12.) ymin = -12.
     if(vy.eq.222.and.ymin.lt.-12.) ymin = -12.
     if((vy.ge.35.and.vy.le.39).or.vy.eq.221) then
        if(ymin.lt.-18..and.dpdt.eq.0) ymin = -18.
        if(ymin.lt.-15..and.dpdt.eq.1) ymin = -15.
        if(ymax.gt.12..and.dpdt.eq.2) ymax = 12.
     end if
  end if
  if(vy.eq.122.and.ymin.gt.-20.) ymin = -20.
  
  
  
  
  
  
  
  
  
  !************************************************************************      
  !***   Line style legend for some cases, want to replace these by colours and in-plot legends
  !************************************************************************      
  
100 continue
  if(djdt.eq.1) then
     write(6,*)''
     write(6,'(A)')'  dJ_tot  : total angular-momentum loss'
     write(6,'(A)')'  dJ_GW   : gravitational-wave AM loss'
     write(6,'(A)')'  dJ_SMB  : Sills MB AM loss (was: wind AM loss)'
     write(6,'(A)')'  dJ_RMB  : Rappaport MB AML (was: SO coupling AML)'
     write(6,'(A)')'  dJ_ML   : non-conservative MT AM loss'
     write(6,*)''
  end if
  
  if(tscls.eq.1) then
     write(6,*)
     write(6,'(A)')'  tau_nuc  : nuclear evolution timescale'
     write(6,'(A)')'  tau_th   : Kelvin-Helmholz timescale'
     write(6,'(A)')'  tau_ML   : mass loss timescale'
     write(6,'(A)')'  tau_Gw   : gravitational-wave timescale'
     write(6,'(A)')'  tau_dyn  : dynamical timescale'
     write(6,*)
  end if
  
  !if(conv.eq.1) then
  !   ch = 0
  !   !write(6,'(A42,$)')' Do you want to plot hatches (Y)es/(N)o: '
  !   !read*,cnvh
  !   !if(cnvh.eq.'y'.or.cnvh.eq.'Y') ch = 1
  !end if
  ch = 1
  
  
  
  if(plot.eq.3.and.rng.eq.'n') goto 129
  if(plot.eq.3.and.rng.eq.'y') goto 125
  x = 0.02*abs(xmax-xmin)
  if(x.eq.0.) x = 0.05*xmax
  xmin = xmin - x
  xmax = xmax + x
125 if(plot.eq.3.and.rng.eq.'x') goto 129
  x = 0.02*abs(ymax-ymin)
  if(x.eq.0.) x = 0.05*ymax
  ymin = ymin - x
  ymax = ymax + x
129 continue
  
  
  
  !************************************************************************      
  !***   Highlight models
  !************************************************************************      
  
  if(plot.lt.2.or.plot.eq.8) then 
     write(6,*)''
     nhp = 0
     hp = 0
     hlbl = 'n'
     hlp = 'n'
     write(6,'(A47,$)')' Do you want to highlight model points (y/n) ? '
     read*,hlp
     if(hlp.eq.'Y') hlp='y'
     if(hlp.eq.'N') hlp='n'
     
     if(hlp.eq.'y') then
        hlp1 = 's'
        if(nf.eq.1) then
           write(6,'(A,$)')' Do you want show (S)tructure models or type model numbers (M)anually?  (S/M) ? '
           read*,hlp1
           if(hlp1.eq.'S') hlp1='s'
           if(hlp1.eq.'M') hlp1='m'
        end if
        
        !Use saved structure models, store them in hp()
        if(hlp1.eq.'s') then
           do pl=1,npl
              write(6,'(/,A)')'      Nr    Line   Model'
              i = 0
              do j=1,n(pl)
                 if(strmdls(pl,j).eq.1) then
                    i = i+1
                    hp(pl,i) = j
                    write(6,'(3I8)')i,hp(pl,i),nint(dat(pl,1,j))
                 end if
              end do
              nhp(pl) = i
              write(6,'(I5,A)')nhp(pl),' points selected.'
           end do !pl
        end if
        
        
        !Enter points manually
        if(nf.eq.1 .and. hlp1.eq.'m') then
           pl = 1
           write(6,'(A67,I7,A17)')' Enter the number(s) of the model(s) that you want to highlight: (1-',nint(dat(pl,1,n)),'), -1: end list: '
           nhp(pl) = 1
           do j=1,1000
              read*,hp(pl,nhp(pl))
              if(hp(pl,nhp(pl)).eq.-1) goto 131
              if(hp(pl,nhp(pl)).lt.1 .or. hp(pl,nhp(pl)).gt.nint(dat(pl,1,n(pl)))) nhp(pl) = nhp(pl)-1
              nhp(pl) = nhp(pl)+1
           end do
           
           !Convert model number to line number
131        nhp(pl) = nhp(pl)-1
           write(6,'(I5,A)')nhp(pl),' points selected:'
           write(6,'(A)')'      Nr   Model    Line'
           do i=1,nhp(pl)
              call locate(dat(pl,1,1:n(pl)),n,dble(hp(pl,i)),j)
              if(dabs(dat(pl,1,j+1)-dble(hp(pl,i))).lt.dabs(dat(pl,1,j)-dble(hp(pl,i)))) j = j+1
              if(j.gt.n(pl)) j = n(pl)
              write(6,'(3I8)')i,hp(pl,i),j
              hp(pl,i) = j
           end do
        end if
        
        write(6,*)''     
        write(6,'(A43,$)')' Do you want to label these points (y/n) ? '
        read*,hlbl
        if(hlbl.eq.'Y') hlbl='y'
        if(hlbl.eq.'N') hlbl='n'
        
     end if !if(hlp.eq.'y') then
  end if !if(plot.lt.2.or.plot.eq.8) then
  
  
  !Redetermine which structure models were saved after rereading file:
  if(plot.eq.6.or.plot.eq.7.and.hlp.eq.'y'.and.hlp1.eq.'s') then
     !Use saved structure models, store them in hp()
     !write(6,'(/,A)')'      Nr    Line   Model'
     i = 0
     do pl=1,npl
        do j=1,n(pl)
           if(strmdls(pl,j).eq.1) then
              i = i+1
              hp(pl,i) = j
              !write(6,'(3I8)')i,hp(pl,i),nint(dat(pl,1,j))
           end if
        end do
        nhp(pl) = i
        !write(6,'(I5,A)')nhp(pl),' points selected.'
     end do !pl
  end if
        
  
  
  
  
  
  
  
  
  
  
  !************************************************************************    
  !***   PLOT TO SCREEN OR FILE
  !************************************************************************      
  
501 continue
  
  if(hrd.eq.1.or.vx.eq.133.or.vx.eq.101) then
     x = min(xmin,xmax)
     xmin = max(xmin,xmax)
     xmax = x
  end if
  if((vy.eq.133.or.vy.eq.101) .and. plot.ne.9) then
     y = min(ymin,ymax)
     ymin = max(ymin,ymax)
     ymax = y
  end if
  
  if(plot.ne.0.and.plot.ne.7.and.plot.ne.9) then
     write(6,*)''     
     write(6,*)' X-range:',xmin,'-',xmax
     write(6,*)' Y-range:',ymin,'-',ymax
     write(6,*)''     
  end if
  
  
  if(plot.eq.9) then
     io = pgopen('plot_plt_000.eps/cps')
     if(io.le.0) then
        write(0,'(A,/)')'  Error opening postscript file, aborting'
        stop
     end if
     
     call pgpap(11.0,0.70) !Make it fit on letter
     !call pgpap(10.,1.)  !Talk, plot
     !call pgpap(30.,0.33)  !Talk, plot
     
     call pgslw(5)
     sch = 1.5
     
  else !plot.ne.9
     
     if(plot.eq.7) call pgend  !Unlike pgbegin, pgopen can't seem to open an open window - why is this no problem for plot.eq.6?
     if(os.eq.1) then
        io = 0
        do while(io.le.0)
           write(xwin,'(I3.3,A7)')xwini,'/xserve'
           io = pgopen(trim(xwin))
           if(io.le.0) then
              write(6,'(A,I3,A,I3)')' X window',xwini," is unavailable, I'll try",xwini+1
              xwini = xwini + 1
           end if
        end do
     end if
     if(os.eq.2) call pgbegin(1,'/aqt',1,1)          !Use Aquaterm on MacOSX
     call pgpap(scrsz,scrrat)
     call pgslw(1)
     sch = 1.0
     if(whitebg.eq.1) then     !Create a white background; swap black (ci=0) and white (ci=1)
        call pgscr(0,1.,1.,1.)  !For some reason, this needs to be repeated for AquaTerm, see below
        call pgscr(1,0.,0.,0.)
        call pgsci(1)
        call pgsci(0)
        call pgsvp(0.,1.,0.,1.)
        call pgswin(-1.,1.,-1.,1.)
        call pgrect(-2.,2.,-2.,2.)
        call pgsci(1)
     end if
  end if !plot.ne.9
  
  if(whitebg.eq.1.and.plot.ne.9) then     !Create a white background; swap black (ci=0) and white (ci=1)
     call pgscr(0,1.,1.,1.)  !Repeat this, to make it work for AquaTerm, for which it was designed
     call pgscr(1,0.,0.,0.)
     call pgsci(1)
     call pgsci(0)
     call pgsvp(0.,1.,0.,1.)
     call pgswin(-1.,1.,-1.,1.)
     call pgrect(-2.,2.,-2.,2.)
     call pgsci(1)
  end if
  
  
  call pgscf(1)
  !if(os.eq.2.or.plot.eq.9) call pgscf(2)
  call pgsch(sch)
  if(plot.eq.9) then
     if(prleg) then
        call pgsvp(0.10,0.90,0.12,0.95)
     else
        call pgsvp(0.10,0.95,0.12,0.95)
     end if
  else
     if(prleg) then
        call pgsvp(0.06,0.90,0.07,0.96)
     else
        call pgsvp(0.06,0.95,0.07,0.96)
     end if
  end if
  call pgswin(xmin,xmax,ymin,ymax)
  boxx = 'BCNTS'
  boxy = 'BCNTS'
  if(lgx.ge.1) boxx = 'BCLNTS'
  if(lgy.ge.1) boxy = 'BCLNTS'
  call pgbox(trim(boxx),0.0,0,trim(boxy),0.0,0)
  if(nf.eq.1 .and. plot.eq.7) then
     f = 1
     write(title1,'(A5,ES12.4,3(A,F6.2),A,ES11.3)')'Age:',dat(f,2,n),' M:',dat(f,4,n),' Mhe:',dat(f,5,n),' Mco:',dat(f,6,n),' Porb:',dat(f,28,n)
     call pgmtxt('T',0.7,0.5,0.5,trim(title1))
  else if(plot.ne.9) then
     call pgmtxt('T',0.7,0.5,0.5,'~/'//trim(title(13:99))//'/'//fname)
  end if
  call pgmtxt('B',2.4,0.5,0.5,trim(pglx))
  call pgmtxt('L',2.0,0.5,0.5,trim(pgly))
  
  
  !Draw curves/points:
  call pgsci(2)
  do pl=1,npl
     col = colours(mod(pl-1,ncolours)+1)  !2,3,...,ncolours,1,2,...
     call pgsci(col)
     !if(npl.eq.1) call pgsci(2)
     yy1(1:n(pl)) = yy(pl,1:n(pl))
     if(drawlines.eq.0) call pgpoint(n,xx(pl,1:n(pl)),yy1(1:n(pl)),1)
     if(drawlines.ge.1) call pgline(n,xx(pl,1:n(pl)),yy1(1:n(pl)))
     if(drawlines.eq.2) call pgpoint(n,xx(pl,1:n(pl)),yy1(1:n(pl)),20)
  end do
  call pgsci(1)
  
  
  !Highlight points:
  call pgsch(1.5*sch)
  call pgsci(2)
  if(hlp.eq.'y') then
     do pl=1,npl
        col = colours(mod(pl-1,ncolours)+1)  !2,3,...,ncolours,1,2,...
        call pgsci(col)
        call pgpoint(nhp(pl),xx(pl,hp(pl,1:nhp(pl))),yy(pl,hp(pl,1:nhp(pl))),2)
        
        if(hlbl.eq.'y') then
           call pgsch(0.7*sch)
           do i=1,nhp(pl)
              write(hlbls,'(I5)')nint(dat(pl,1,hp(pl,i)))
              call pgtext(xx(pl,hp(pl,i)),yy(pl,hp(pl,i)),hlbls)
           end do
           call pgsch(sch)
           call pgsci(1)
        end if !if(hlbl.eq.'y') then
        
     end do !pl
  end if
  
  
  !Print legenda:
  call pgsch(sch)
  if(prleg) then
     if(nf.eq.1) then  !Then multi-variable plot
        do pl=1,npl
           col = colours(mod(pl-1,ncolours)+1)  !2,3,...,ncolours,1,2,...
           call pgsci(col)
           call pgmtext('RV',0.5,real(20-pl)/20.,0.,trim(leglbl(pl)))
        end do !pl
        
     else  !Then: multi-file plot
        do f=1,nf
           fname = fnames(f)
           lng = len_trim(fname)
           if(fname(lng:lng).eq.'1'.or.fname(lng:lng).eq.'2') lng = lng-1
           lng = lng-4 !get rid of '.plt'
           col = colours(mod(f-1,ncolours)+1)  !2,3,...,ncolours,1,2,...
           call pgsci(col)
           call pgmtext('RV',0.5,real(46.-real(f))/45.,0.,fname(1:lng))
        end do
     end if
     call pgsci(1)
  end if
  
  call pgsch(sch)
  
  
  
  if(nf.eq.1 .and. conv.eq.1) then
     call pgsci(1)
     pl = 1
     
     !call pltconvection(nmax,nvar,n(pl),dat(pl,:,:),vx,ymin,ymax,nhp(pl),hp(pl,:),hlp,hlbl)   !Convection plot - replots axes at the end
     call pltconvection(n(pl),nvar,n(pl),dat(pl,1:nvar,1:n(pl)),vx,ymin,ymax,nhp(pl),hp(pl,:),hlp,hlbl)   !Convection plot - replots axes at the end
     call pgsci(2)
  end if
  
  
  
  
  if(vy.eq.29) then !FLR
     call pgsls(4)
     call pgline(2,(/xmin,xmax/),(/0.,0./))
     call pgsls(1)
  end if
  
  if(vy.eq.81) then !Qconv-plot
     call pgsls(4)
     call pgline(2,(/xmin,xmax/),(/0.02,0.02/))
     call pgsls(1)
  end if
  
  if(vy.eq.121) then !Prot,critical for sat-MB, add Prot
     call pgsls(4)
     do pl=1,npl
        if(lgy.eq.0) then
           call pgline(n,xx(pl,1:n(pl)),real(dat(pl,21,1:n(pl))))
        else 
           call pgline(n,xx(pl,1:n(pl)),log10(real(dat(pl,21,1:n(pl)))))
        end if
     end do !pl
     call pgsls(1)
  end if
  
  if(vy.eq.122) then !Comp sat-MB with RVJ83-MB
     call pgsls(4)
     do pl=1,npl
        call pgline(n,xx(pl,1:n(pl)),log10(real(dat(pl,38,1:n(pl)))))
     end do !pl
     call pgsls(1)
  end if
  
  call pgsci(1)
  
  if(plot.eq.9) then
     call pgend
     ex = .true.
     pstitle = 'PlotPlt output: '//trim(asclx)//' vs. '//trim(ascly)//'.'
     if(vx.eq.201) pstitle = 'PlotPlt output: HRD.'
     i = 1
     do while(ex)
        write(psname,'(A,I3.3,A)')'plot_plt__'//trim(asclx)//'--'//trim(ascly)//'_',i,'.eps'
        if(vx.eq.201) write(psname,'(A,I3.3,A)')'plot_plt__HRD_',i,'.eps'
        inquire(file=trim(psname), exist=ex)                                                   !Check whether the file already exists
        if(.not.ex) then
           j = system('mv -f plot_plt_000.eps '//trim(psname))
           call set_PGPS_title(trim(psname),trim(pstitle))
        end if
        i = i+1
     end do
     write(6,'(A)')' Plot saved to '//trim(psname)
     plot = 0
     goto 501 !Redo the plot on the screen, in case you select '4' next
  end if
  !End of the plotting
  
  
  
  
  if(plot.eq.7) then
     call sleep(wait)
     goto 7
  end if
  
  
  
  
  
  
  
  
  !************************************************************************    
  !***   POST-PLOT MENU   ***
  !************************************************************************      
  
900 if(plot.ne.0.and.plot.ne.9) then
     write(6,*)''
     write(6,'(A)')' You can:'
     write(6,'(A)')'  0) quit'
     write(6,'(A)')'  1) change variables'
     write(6,'(A)')'  2) change lin/log axes'
     write(6,'(A)')'  3) change axis ranges'
     write(6,'(A)')'  4) select zoom region'
     write(6,'(A)')'  5) zoom out'
     write(6,'(A)')'  6) reread file and make same plot'
     write(6,'(A)')'  7) auto-update this plot'
     write(6,'(A)')'  8) change input file'
     write(6,'(A)')'  9) save plot as postscript'
     write(6,'(A)')' 10) identify a point in the graph'
     write(6,'(A)')' 11) toggle drawing line/points'
  end if !if(plot.ne.9) then
  write(6,*)''
  write(6,'(A27,$)')' What do you want to do ?  '
  read*,plot
  if(plot.lt.0.or.plot.gt.11) goto 900
  
  if(plot.ne.4.and.plot.ne.10) call pgend
  
  if(plot.eq.1) goto 30
  if(plot.eq.2) goto 37
  if(plot.eq.3) goto 70
  if(plot.eq.6) goto 7
  if(plot.eq.8) goto 5
  if(plot.eq.9) goto 501
  
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
     call pgend
     goto 501
  end if
  
  if(plot.eq.5) then  !Zoom out
     xc = (xmin+xmax)/2.
     yc = (ymin+ymax)/2.
     xm = xmin
     ym = ymin
     xmin = xc - 2*abs(xc-xm) !Central value - 2x the 'radius', 'radius' = central value - minimum
     xmax = xc + 2*abs(xc-xm)
     ymin = yc - 2*abs(yc-ym)
     ymax = yc + 2*abs(yc-ym)
     goto 501
  end if
  
  if(plot.eq.7) then  !Auto update
     wait = 2  !Pause in seconds
     goto 7
  end if
  
  if(plot.eq.10) then  !Identify closest model
     xsel = 0.
     ysel = 0.
     write(6,'(A)')' Select a point in the graph and press "x" to finish'
     nsel=0
     call pgsci(1)
     call pgolin(1,nsel,xsel,ysel,2)
     !write(6,'(10ES10.2)')xsel,xmin,xmax
     !write(6,'(10ES10.2)')ysel,ymin,ymax
     
     
     dx = abs(xmax-xmin)
     dy = abs(ymax-ymin)
     mindist = 1.e30
     do pl=1,npl
        do i=1,n(pl)
           dist = (abs(xsel(1)-xx(pl,i))/dx)**2 + (abs(ysel(1)-yy(pl,i))/dy)**2
           if(dist.lt.mindist) then
              i0 = i
              pl0 = pl
              mindist = dist
           end if
        end do
     end do
     write(6,*)''
     write(6,'(A,ES12.4,A,ES12.4)')          ' Selected point:    x =',xsel(1),',  y =',ysel(1)
     write(6,'(A,ES12.4,A,ES12.4,A,I5,A,I6)')' Closest model:     x =',xx(pl0,i0),',  y =',yy(pl0,i0),'    line =',i0+1,',  model =',nint(dat(pl0,1,i0))
     
     dx = 0
     dy = 0
     if(i0.gt.1.and.i0.lt.n(pl0)) then
        dx = xx(pl0,i0+1)-xx(pl0,i0-1)
        dy = yy(pl0,i0+1)-yy(pl0,i0-1)
     else if(i0.gt.1) then
        dx = xx(pl0,i0)-xx(pl0,i0-1)
        dy = yy(pl0,i0)-yy(pl0,i0-1)
     else if(i0.lt.n(pl0)) then
        dx = xx(pl0,i0+1)-xx(pl0,i0)
        dy = yy(pl0,i0+1)-yy(pl0,i0)
     end if
     
     write(6,'(3(A,ES12.4))')' Derivative:       dx =',dx,', dy =',dy,',  dy/dx =',dy/dx
     
     write(6,*)''
     !From listplt
     write(6,'(A)')' Line   Mdl     t (yr)   M(Mo)   Mhe   Mco   Menv    R (Ro)   L (Lo)    Te (K)   Tc (K)       V    B-V     Xc    Yc   Porb(d)     dM/dt  M2/Mo'
     d = dat(pl0,:,i0)
     write(6,'(I5,I6,ES11.4,F8.3,2F6.3,F7.3,2(1x,2ES9.2),1x,2F7.3,1x,2F6.3,2ES10.2,F7.3)')i0+1,nint(d(1)),d(2),d(4),d(5),d(6),d(63),d(8),d(9),d(10),d(11),d(101),d(103),d(56),d(57),d(28),dabs(d(31)),d(40)
     write(6,*)''
     
     !col = 2
     col = colours(mod(pl0-1,ncolours)+1)  !2,3,...,ncolours,1,2,...
     call pgsci(col)
     
     call pgpoint(1,xx(pl0,i0),yy(pl0,i0),2)
     write(hlbls,'(I5)')nint(dat(pl0,1,i0))
     call pgptxt(xx(pl0,i0),yy(pl0,i0),0.,0.,hlbls)
     
     call pgsci(1)
     goto 900
  end if
  
  if(plot.eq.11) then !Toggle between drawing points, lines or both
     ansi=-1
     do while(ansi.lt.0.or.ansi.gt.3)
        write(6,'(A)')'  You can plot:'
        write(6,'(A)')'  0: keep the current choice'
        write(6,'(A)')'  1: dots'
        write(6,'(A)')'  2: lines'
        write(6,'(A)')'  3: both'
        write(6,'(A,$)')'  What would you like to plot?  '
        read*,ansi
     end do
     if(ansi.gt.0) drawlines = ansi-1 !0-2
     goto 501
  end if
  
  
9999 continue
  write(6,'(/,A,/)')' Program finished.'
end program plotplt
!************************************************************************      




