!> \file plotplt.f90  Plots the data contained in .plt[12] files, highlights selected points.
!
!  2006-04-19

! Copyright 2002-2024 Marc van der Sluys - marc.vandersluys.nl
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
!> \brief Plot the contents of .plt[12] files
!!
!! - Uses routines from functions.f90, plt_functions.f90
!! - Requires the file [libdir]/UBVRI.Kur to calculate colours
!!
!! \todo allocate nf iso npl in dat()? -> allocate(dat(npl,nvar,nmax), datf(nvar,nmax))

program plotplt
  use SUFR_kinds, only: double,dbl
  use SUFR_constants, only: homedir
  use SUFR_numerics, only: seq0,sne0
  use SUFR_dummy, only: dumstr
  
  use constants, only: libdir, colours,ncolours, scrrat,scrsz, white_bg
  use ubvdata, only: ubv
  
  implicit none
  integer,parameter :: nmax=10000,nvar=229,nc=81,nl=7,nfmax=50
  real(double) :: d(nvar)
  
  integer, allocatable :: n(:),oldn(:),unchanged(:), strmdls(:,:),hp(:,:),nhp(:),excly(:)
  real, allocatable :: xx(:,:),yy(:,:),miny(:)
  real(double), allocatable :: dat(:,:,:),datf(:,:)
  real(double) :: mint,maxt,dt
  logical :: logt,lgx,lgy
  
  real :: yy1(nmax),minx,dist,mindist
  real :: x,y,xmin,xmax,ymin,ymax,dx,dy
  real :: xsel(4),ysel(4),xc,yc,xm,ym
  
  integer :: f,nf,nfi,i,i0,j,pl0,vx,vy,plot,npl,pl,plotstyle,version,verbose
  integer :: hrd,djdt,conv,tscls,dpdt,io
  integer :: wait,lums,nsel,os
  integer :: ansi,xwini,pgopen,defvar(0:nvar)
  integer :: col,lng
  real :: sch
  
  character :: fname*(99),fnames(nfmax)*(99),psname*(99)
  character :: ans,rng,log,hlp1,hlbls*(5),leglbl(29)*(99)
  character :: xwin*(19),boxx*(19),boxy*(19)
  character :: pglabels(nvar)*(99),asclabels(nvar)*(99),pglx*(99),pgly*(99),title*(99),title1*(99)
  character :: pstitle*(99),asclx*(99),ascly*(99), complbl*(3), mdlnr*(9)
  logical :: ex,prleg, hlp,hlbl
  
  ! Set constants:
  call setconstants()
  write(6,*)
  call print_code_version(6)  !To screen
  
  call evTools_settings()
  
  
  sch = 1.0
  
  !os = getos() !1-Linux, 2-MacOS
  os = 1        ! Don't use Mac OS's silly AquaTerm (watta mistaka to maka)
  plotstyle = 1 ! 1: draw lines, 2: draw dots, 3: draw both
  
  ! Remove 'uninitialised' compiler warnings:
  hrd   = 0
  djdt  = 0
  conv  = 0
  tscls = 0
  lums  = 0
  i0 = 1
  pl0 = 1
  
  ! Read atmosphere-model data
  open(unit=10, file=trim(libdir)//'/UBVRI.Kur',status='old',action='read',iostat=io)
  if(io.eq.0) then
     read(10,*) dumstr
     read(10,*) dumstr
     read(10,*) ubv
     close(10)
  else
     write(6,'(A)')" Warning:  I can't find the file "//trim(libdir)//"/UBVRI.Kur, so I can't calculate colours and magnitudes..."
  end if
  
  
  !Read current path and use it as plot title
  call system('pwd > '//trim(homedir)//'/tmppwd.txt')
  open(unit=10,form='formatted',status='old',file=trim(homedir)//'/tmppwd.txt')
  rewind 10
  read(10,'(a99)')title
  close(10)
  call system('rm '//trim(homedir)//'/tmppwd.txt')
  
  
  plot = 0
  xwini = 1  ! Number of X window to try first
5 continue
  
  ! Search for input files in current dir:
  nf = command_argument_count()
  if(nf.ge.1.and.plot.eq.0) then
     do f=1,nf
        call get_command_argument(f,fname)
        fnames(f) = fname
     end do
  else
     !fname = findfile('*.plt*') !Match string
     !if(fname(1:10).eq.'          ') goto 9999
     call findfiles('*.plt*',nfmax,0,fnames,nf)   ! all=0
     if(nf.le.0) goto 9999
  end if
  plot = 1
  
  
  ! Allocate arrays:
  npl = max(nf,nl)
  
  allocate(dat(npl,nvar,nmax))
  allocate(n(npl), oldn(npl), unchanged(npl),  strmdls(npl,nmax))
  allocate(xx(npl,nmax), yy(npl,nmax), miny(npl), excly(npl))
  allocate(hp(npl,1000), nhp(npl))
  oldn = 0
  unchanged = 0
  
  ! Get the labels for the plot axes; defvar = 0 for non-defined variables:
  call getpltlabels(nf,nvar,pglabels,asclabels,defvar)
  
  
  !************************************************************************      
  !***   READ THE INPUT FILE
  !************************************************************************      
  
7 continue
  
  verbose = 1
  if(plot.eq.7) verbose = 0
  if(verbose.eq.1) write(6,*)
  allocate(datf(nvar,nmax))
  do f=1,nf
     call read_plt_bse(10,trim(fnames(f)),nmax,nvar,nc,verbose,datf,nfi,version)  ! Use unit 10
     if(version.eq.2005) strmdls(f,:) = nint(datf(83,:))   ! Structure model was saved (1) or not (0)
     if(version.ge.2011) strmdls(f,:) = nint(datf(92,:))   ! Structure model was saved (1) or not (0)
     call changepltvars(nmax,nvar,nfi,datf,pglabels,dpdt)  ! Change (e.g. de-log) and add plot variables
     dat(f,:,:) = datf(:,:)
     n(f) = nfi
  end do
  deallocate(datf)
  
  
  !************************************************************************      
  !***   CHOOSE PLOT VARIABLES
  !************************************************************************      
  
30 if(plot.ne.6.and.plot.ne.7) then      
     call printpltvarlist(nf)  !Print the list of variables in a *.plt? file to screen, for input menu
     
     io = -1
     do while(io.ne.0)
        write(6,'(A)', advance='no') '  Choose the X-axis variable: '
        read(*,*, iostat=io) vx
        if(io.ne.0) cycle
        
        if(vx.eq.0) goto 9999
        io = 0
        if(vx.lt.0.or.vx.gt.201) io = -1
        if(defvar(vx).eq.0)  io = -1
     end do
     
     hrd   = 0
     djdt  = 0
     conv  = 0
     tscls = 0
     lums  = 0
  end if   !if(plot.ne.6.and.plot.ne.7) then   
  
  
  npl = nf  !npl is the number of curves that will be plotted. This can be >1 because nf>1, or because nf=1, but we plot >1 variable
  prleg = .false.  !Don't print legenda by default
  if(nf.gt.1) prleg = .true.
  
  if(vx.eq.201.or.hrd.eq.1) then  ! HRD
     hrd = 1
     pl = 1
     mint = minval( dat(pl,10,1:n(pl)) )
     maxt = maxval( dat(pl,10,1:n(pl)) )
     dt = 2*(maxt-mint)/(maxt+mint)  !Relative dt
     logt = .true.
     if(dt.lt.1.0_dbl) logt = .false.  ! No logarithmic axis for small T intervals
     
     do pl=1,npl
        if(logt) then
           xx(pl,1:n(pl)) = real(log10(abs(dat(pl,10,1:n(pl) ))))
        else
           xx(pl,1:n(pl)) = real(dat(pl,10,1:n(pl) ))
        end if
        yy(pl,1:n(pl)) = real(log10(abs(dat(pl, 9,1:n(pl) ))))
     end do
     pglx = trim(pglabels(10))
     pgly = trim(pglabels(9))
     asclx = 'HRD'
     ascly = 'HRD'
     vy = 0
     lgx = .false.
     if(logt) lgx = .true.
     lgy = .true.
     goto 50
  end if
  
  if(plot.lt.2) then      
     
     io = -1
     do while(io.ne.0)
        write(6,'(A)', advance='no') '  Choose the Y-axis variable: '
        read(*,*, iostat=io) vy
        if(io.ne.0) cycle
        
        if(vy.eq.0) goto 9999
        io = 0
        if(vy.lt.0.or.vy.eq.201) io = -1  ! Can't take HRD as y-variable
        if(defvar(vy).eq.0)  io = -1
     end do
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
        leglbl(1:npl) = (/'dJ\dtot\u','dJ\dGW\u ','dJ\dSMB\u','dJ\dRMB\u','dJ\dML\u '/)
        prleg = .true.
     end if
     if(vy.eq.222) then !Mdots
        npl = 3
        yy(1:3,1:nmax) = real(dat(f,31:33,1:nmax))
        leglbl(1:npl) = (/'dM\dtot\u ','dM\dwind\u','dM\dMT\u  '/)
        prleg = .true.
     end if
     if(vy.eq.223) then ! Winds
        npl = 3
        yy(1,1:nmax) = real(dat(f,32,1:nmax))
        yy(2,1:nmax) = real(dat(f,136,1:nmax))
        yy(3,1:nmax) = real(dat(f,137,1:nmax))
        leglbl(1:npl) = (/'dM\dwind\u','dM\dR\u   ','dM\dRlk\u '/)
        prleg = .true.
     end if
     if(vy.eq.224) then  ! Zetas = dlogR/dlogMs: model
        npl = 2
        yy(1,1:nmax) = real(dat(f,161,1:nmax))
        yy(2,1:nmax) = real(dat(f,162,1:nmax))
        leglbl(1:npl) = [character(len=99) :: '\(0632)\d*\u','\(0632)\dRL\u']
        prleg = .true.
     end if
     if(vy.eq.225) then  ! Zetas = dlogR/dlogMs: analytic
        npl = 4
        yy(1,1:nmax) = real(dat(f,163,1:nmax))
        yy(2,1:nmax) = real(dat(f,164,1:nmax))
        yy(3,1:nmax) = real(dat(f,165,1:nmax))
        yy(4,1:nmax) = real(dat(f,166,1:nmax))
        leglbl(1:npl) = [character(len=99) :: '\(0632)\dad\u','\(0632)\dRL,an\u, \(0628)=0.0','\(0632)\dRL,an\u, \(0628)=0.5', &
             '\(0632)\dRL,an\u, \(0628)=1.0']
        prleg = .true.
     end if
     if(vy.eq.226) then  ! Zetas = dlogR/dlogMs: all
        npl = 6
        yy(1,1:nmax) = real(dat(f,161,1:nmax))
        yy(2,1:nmax) = real(dat(f,162,1:nmax))
        yy(3,1:nmax) = real(dat(f,163,1:nmax))
        yy(4,1:nmax) = real(dat(f,164,1:nmax))
        yy(5,1:nmax) = real(dat(f,165,1:nmax))
        yy(6,1:nmax) = real(dat(f,166,1:nmax))
        leglbl(1:npl) = [character(len=99) :: '\(0632)\d*\u','\(0632)\dRL\u','\(0632)\dad\u','\(0632)\dRL,an\u, \(0628)=0.0', &
             '\(0632)\dRL,an\u, \(0628)=0.5','\(0632)\dRL,an\u, \(0628)=1.0']
        prleg = .true.
     end if
     
     if(vy.eq.211) then  ! Timescales
        tscls = 1
        npl = 5
        yy(1:5,1:nmax) = real(dat(f,201:205,1:nmax))
        
        ! Line labels for Timescales plot:
        leglbl(1:npl) = (/'\(0645)\dnuc\u ','\(0645)\dth\u  ','\(0645)\dML\u  ','\(0645)\dGW\u  ','\(0645)\ddyn\u '/)
        
        ! Add R/(dR/dt)
        npl = 6
        yy(6,1:nmax) = real(dat(f,119,1:nmax))
        leglbl(npl) = '\(0645)\ddR/dt\u  '
        
        prleg = .true.
     end if
     if(vy.eq.212) then  ! Luminosities
        lums = 1
        npl = 6
        yy(1,1:nmax) = real(dat(f,9,1:nmax))
        yy(2:6,1:nmax) = real(dat(f,16:20,1:nmax))
        !Line labels for Luminosities plot:
        leglbl(1:npl) = (/'L\dsurf\u  ','L\dH\u     ','L\dHe\u    ','L\dC\u     ','L\d\(639)\u','L\dth\u    '/)
        prleg = .true.
     end if
     
     if(vy.eq.213) then  ! Surface abundances
        npl = 7
        yy(1:npl,1:nmax) = real(dat(f,42:48,1:nmax))
        !Line labels for Abundances plots:
        leglbl(1:npl) = (/'H ','He','C ','N ','O ','Ne','Mg'/)
        prleg = .true.
     end if
     if(vy.eq.214) then  ! Tmax abundances
        npl = 7
        yy(1:npl,1:nmax) = real(dat(f,49:55,1:nmax))
        !Line labels for Abundances plots:
        leglbl(1:npl) = (/'H ','He','C ','N ','O ','Ne','Mg'/)
        prleg = .true.
     end if
     if(vy.eq.215) then !Core abundances
        npl = 7
        yy(1:npl,1:nmax) = real(dat(f,56:62,1:nmax))
        !Line labels for Abundances plots:
        leglbl(1:npl) = (/'H ','He','C ','N ','O ','Ne','Mg'/)
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
     write(6,'(A)', advance='no')' Do you want a logarithmic scale:  (N)o, (X)-axis, (Y)-axis, (B)oth: '
     read*,log
     if(log.eq.'X') log='x'
     if(log.eq.'Y') log='y'
     if(log.eq.'B') log='b'
     if(log.eq.'N') log='n'
  end if  !if(plot.ne.6.and.plot.ne.7) then   
  
  lgx = .false.
  lgy = .false.
  if(log.eq.'x'.or.log.eq.'b') lgx = .true.
  if(log.eq.'y'.or.log.eq.'b') lgy = .true.
  
  
  pglx = pglabels(vx)
  pgly = pglabels(vy)
  asclx = asclabels(vx)
  ascly = asclabels(vy)
  
  if(lgx) then
     do pl=1,npl
        if(xx(pl,1).le.0.) xx(pl,1) = xx(pl,2)
     end do
     minx = huge(minx)
     do pl=1,npl
        do j=1,n(pl)
           if(abs(xx(pl,j)).lt.minx.and.sne0(abs(xx(pl,j)))) minx = abs(xx(pl,j))
        end do
        xx(pl,1:n(pl)) = log10(abs(xx(pl,1:n(pl)))+minx*1.e-3)
     end do
  end if
  
  if(djdt.eq.1) lgy = .true.
  excly = 0
  if(lgy) then
     do pl=1,npl
        if(seq0(yy(pl,1))) yy(pl,1) = yy(pl,2)
        miny(pl) = huge(miny(pl))
        do j=1,n(pl)
           if(abs(yy(pl,j)).lt.miny(pl).and.sne0(abs(yy(pl,j)))) miny(pl) = abs(yy(pl,j))
        end do
        yy(pl,1:n(pl)) = log10(abs(yy(pl,1:n(pl)))+miny(pl)*1.e-3)
        if(abs(miny(pl)-huge(miny(pl))).lt.1e32) excly(pl) = 1  !Exclude it in determining ranges
     end do !pl
  end if
  
  
  
50 continue !HRD jumps here
  
  xmin = huge(xmin)
  xmax = -huge(xmax)
  do pl=1,npl
     !if(exclx(pl).eq.1) cycle
     xmin = min(minval(xx(pl,1:n(pl))),xmin)
     xmax = max(maxval(xx(pl,1:n(pl))),xmax)
  end do
  
  ymin = huge(ymin)
  ymax = -huge(ymax)
  do pl=1,npl
     if(excly(pl).eq.1) cycle
     if(lgy) then
        ymin = min(minval(yy(pl,1:n(pl)), yy(pl,1:n(pl)).gt.-log10(huge(ymin))), ymin)
        ymax = max(maxval(yy(pl,1:n(pl)), yy(pl,1:n(pl)).lt. log10(huge(ymin))), ymax)
     else
        ymin = min(minval(yy(pl,1:n(pl)), yy(pl,1:n(pl)).gt.-huge(ymin)), ymin)
        ymax = max(maxval(yy(pl,1:n(pl)), yy(pl,1:n(pl)).lt. huge(ymax)), ymax)
     end if
  end do
  
  if(vx.eq.119) then !R/(dR/dt)
     !if(xmin.lt.1.e4.and..not.lgx) xmin = 1.e4
     !if(xmin.lt.4..and.lgx) xmin = 4.
     if(xmax.gt.1.e12.and..not.lgx) xmax = 1.e12
     if(xmax.gt.12..and..not.lgx) xmax = 12.
  end if
  if(vy.eq.119) then !R/(dR/dt)
     !if(ymin.lt.1.e4.and..not.lgy) ymin = 1.e4
     !if(ymin.lt.4..and.lgy) ymin = 4.
     if(ymax.gt.1.e12.and..not.lgy) ymax = 1.e12
     if(ymax.gt.12..and.lgy) ymax = 12.
  end if
  
  if(lums.eq.1.and.lgy) ymin = max(ymin,ymax-10.)
  
  if(conv.eq.1) ymin = 0.d0
  
  
  
  ! Limit ranges for logged axes like Mdot:
  if(lgx) then
     !if(vx.ge.31.and.vx.le.33.and.xmin.lt.-12.) xmin = -12.  ! Mdot
  end if
  if(lgy) then
     !if(vy.ge.31.and.vy.le.33.and.ymin.lt.-12.) ymin = -12.  ! Mdot
     !if(vy.eq.222.and.ymin.lt.-12.) ymin = -12.  ! Mdots
     if((vy.ge.35.and.vy.le.39).or.vy.eq.221) then
        if(ymin.lt.-18..and.dpdt.eq.0) ymin = -18.
        if(ymin.lt.-15..and.dpdt.eq.1) ymin = -15.
        if(ymax.gt.13..and.dpdt.eq.2) ymax = 13.
     end if
  end if
  
  if(vy.eq.211.and.lgy.and.ymin.lt.1.) ymin = 1.
  if(vy.eq.211.and.lgy.and.ymax.gt.15.) ymax = 15.
  if(vy.eq.122.and.ymin.lt.-20.) ymin = -20.
  
  
  
  
  
  
  
  
  
  !************************************************************************      
  !***   PLOT RANGE
  !************************************************************************      
  
70 if(plot.ne.6.and.plot.ne.7) then      
     write(6,*)''
     write(6,*)' X-range:',xmin,'-',xmax
     write(6,*)' Y-range:',ymin,'-',ymax
     write(6,'(A)', advance='no')' Do you want to change a plot range ?  (N)o, (X)-axis, (Y)-axis, (B)oth: '
     read*,rng
     
     if(rng.eq.'N') rng='n'
     if(rng.eq.'X') rng='x'
     if(rng.eq.'Y') rng='y'
     if(rng.eq.'B') rng='b'
     
     if(rng.eq.'n'.or.rng.eq.' ') goto 100
     
     if(rng.eq.'x'.or.rng.eq.'b') then
        write(6,'(A51)', advance='no')'  Give the new range for the X-axis (Xmin, Xmax): '
        read*,xmin,xmax
        if(xmin.gt.xmax) then
           x = xmin
           xmin = xmax
           xmax = x
           write(6,'(A)')'  I swapped Xmin and Xmax'
        end if !if(xmin.gt.xmax)
     end if !if(rng.eq.'x'.or.rng.eq.'b')
     
     
     if(rng.eq.'y'.or.rng.eq.'b') then
        write(6,'(A51)', advance='no')'  Give the new range for the Y-axis (Ymin, Ymax): '
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
  if(lgx) then
     if(vx.ge.31.and.vx.le.33.and.xmin.lt.-12.) xmin = -12.
  end if
  if(lgy) then
     if(vy.ge.31.and.vy.le.33.and.ymin.lt.-12.) ymin = -12.
     if(vy.eq.222.and.ymin.lt.-12.) ymin = -12.
     if((vy.ge.35.and.vy.le.39).or.vy.eq.221) then
        if(ymin.lt.-18..and.dpdt.eq.0) ymin = -18.
        if(ymin.lt.-15..and.dpdt.eq.1) ymin = -15.
        if(ymax.gt.13..and.dpdt.eq.2) ymax = 13.
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
  
  
  
  if(plot.eq.3.and.rng.eq.'n') goto 129
  if(plot.eq.3.and.rng.eq.'y') goto 125
  x = 0.02*abs(xmax-xmin)
  if(seq0(x)) x = 0.05*xmax
  xmin = xmin - x
  xmax = xmax + x
125 if(plot.eq.3.and.rng.eq.'x') goto 129
  x = 0.02*abs(ymax-ymin)
  if(seq0(x)) x = 0.05*ymax
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
     hlp = .false.
     hlbl = .false.
     write(6,'(A47)', advance='no')' Do you want to highlight model points (y/n) ? '
     read*,ans
     if(ans.eq.'Y' .or. ans.eq.'y') hlp = .true.
     
     if(hlp) then
        hlp1 = 's'
        if(nf.eq.1) then
           write(6,'(A)', advance='no')' Do you want show (S)tructure models or type model numbers (M)anually?  (S/M) ? '
           read*,hlp1
           if(hlp1.eq.'S') hlp1='s'
           if(hlp1.eq.'M') hlp1='m'
        end if
        
        ! Use saved structure models, store them in hp()
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
           write(6,'(A67,I7,A17)')' Enter the number(s) of the model(s) that you want to highlight: (1-',nint(dat(pl,1,n)), &
                '), -1: end list: '
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
              if(abs(dat(pl,1,j+1)-dble(hp(pl,i))).lt.abs(dat(pl,1,j)-dble(hp(pl,i)))) j = j+1
              if(j.gt.n(pl)) j = n(pl)
              write(6,'(3I8)')i,hp(pl,i),j
              hp(pl,i) = j
           end do
        end if
        
        write(6,*)''     
        write(6,'(A43)', advance='no')' Do you want to label these points (y/n) ? '
        read*,ans
        if(ans.eq.'Y' .or. ans.eq.'y') hlbl=.true.
        
     end if !if(hlp) then
  end if !if(plot.lt.2.or.plot.eq.8) then
  
  
  ! Redetermine which structure models were saved after rereading file:
  if(plot.eq.6.or.plot.eq.7.and.hlp.and.hlp1.eq.'s') then
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
     
     call pgpap(10.5,0.68) !Make it fit on letter paper
     !call pgpap(10.,1.)  !Talk, plot
     !call pgpap(30.,0.33)  !Talk, plot
     
     call pgslw(5)
     sch = 1.5
     
  else !plot.ne.9
     
     if(plot.eq.7) call pgend  ! Unlike pgbegin, pgopen can't seem to open an open window - why is this no problem for plot.eq.6?
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
     if(white_bg) then     !Create a white background; swap black (ci=0) and white (ci=1)
        call pgscr(0,1.,1.,1.)  !For some reason, this needs to be repeated for AquaTerm, see below
        call pgscr(1,0.,0.,0.)
        call pgsci(0)
        call pgsvp(0.,1.,0.,1.)
        call pgswin(-1.,1.,-1.,1.)
        call pgrect(-2.,2.,-2.,2.)
        call pgsci(1)
        
        call pgscr(3,0.0,0.8,0.0)  ! Dark green
        call pgscr(5,0.0,0.8,0.8)  ! Dark cyan
     end if
  end if !plot.ne.9
  
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
  if(lgx) boxx = 'BCLNTS'
  if(lgy) boxy = 'BCLNTS'
  call pgbox(trim(boxx),0.0,0,trim(boxy),0.0,0)
  if(nf.eq.1 .and. plot.eq.7) then
     f = 1
     write(title1,'(A,I5,A,ES11.4,A,ES9.2,3(A,F6.2),A,ES9.2)') 'Model:',nint(dat(f,1,n(f))),',  age:',dat(f,2,n(f)), &
          ',  dt:',dat(f,3,n(f)),',  M:',dat(f,4,n(f)),',  Mhe:',dat(f,5,n(f)),',  Mco:',dat(f,6,n(f)),',  Porb:',dat(f,28,n(f))
     call pgmtxt('T',0.7,0.5,0.5,trim(title1))
  else if(plot.ne.9) then
     if(nf.eq.1) then
        call pgmtxt('T',0.7,0.5,0.5,'~/'//trim(title(13:99))//'/'//trim(fnames(1)))
     else
        call pgmtxt('T',0.7,0.5,0.5,'~/'//trim(title(13:99))//'/*.plt*')
     end if
  end if
  call pgmtxt('B',2.4,0.5,0.5,trim(pglx))
  call pgmtxt('L',2.0,0.5,0.5,trim(pgly))
  
  
  ! Draw curves/points:
  call pgsci(2)
  do pl=1,npl
     col = colours(mod(pl-1,ncolours)+1)
     call pgsci(col)
     !if(npl.eq.1) call pgsci(2)
     yy1(1:n(pl)) = yy(pl,1:n(pl))
     select case(plotstyle)
     case(1) 
        call pgline(n(pl),xx(pl,1:n(pl)),yy1(1:n(pl)))
     case(2) 
        call pgpoint(n(pl),xx(pl,1:n(pl)),yy1(1:n(pl)),1)
     case(3)
        call pgline(n(pl),xx(pl,1:n(pl)),yy1(1:n(pl)))
        call pgsci(1)
        call pgpoint(n(pl),xx(pl,1:n(pl)),yy1(1:n(pl)),20)
     call pgsci(col)
     end select
     
     if(plot.eq.7) then
        if(n(pl).eq.oldn(pl)) then
           unchanged(pl) = unchanged(pl) + 1
        else
           unchanged(pl) = 0
        end if
        oldn(pl) = n(pl)
        
        call pgpoint(1,xx(pl,1),yy(pl,1),4)              ! Draw beginning of track for auto-update
        if(unchanged(pl).ge.5) then                      ! The model hasn't changed for a while
           call pgpoint(1,xx(pl,n(pl)),yy(pl,n(pl)),17)  ! Draw end of track
        else
           call pgpoint(1,xx(pl,n(pl)),yy(pl,n(pl)),2)   ! Draw end of track
        end if
        
        if(pl.eq.npl .and. minval(unchanged(1:npl)).ge.10) then
           write(*,'(/,A,/)') 'All models that were tracked have stopped evolving, stopping auto-update'
           plot = 1           ! Stop auto-update
        end if
     end if
  end do
  call pgsci(1)
  
  
  ! Highlight points:
  call pgsch(1.5*sch)
  call pgsci(2)
  if(hlp) then
     do pl=1,npl
        col = colours(mod(pl-1,ncolours)+1)
        call pgsci(col)
        call pgpoint(nhp(pl),xx(pl,hp(pl,1:nhp(pl))),yy(pl,hp(pl,1:nhp(pl))),2)
        
        if(hlbl) then
           call pgsch(0.7*sch)
           do i=1,nhp(pl)
              write(hlbls,'(I5)')nint(dat(pl,1,hp(pl,i)))
              call pgtext(xx(pl,hp(pl,i)),yy(pl,hp(pl,i)),hlbls)
           end do
           call pgsch(sch)
           call pgsci(1)
        end if !if(hlbl) then
        
     end do !pl
  end if
  
  
  ! Print legenda:
  call pgsch(sch*0.5)
  if(prleg) then
     if(nf.eq.1) then  ! Then multi-variable plot
        do pl=1,npl
           col = colours(mod(pl-1,ncolours)+1)
           call pgsci(col)
           call pgmtext('RV',0.5,real(20-pl)/20.,0.,trim(leglbl(pl)))
        end do !pl
        
     else  ! Then: multi-file plot
        do f=1,nf
           fname = fnames(f)
           lng = len_trim(fname)
           complbl = '   '
           if(fname(lng:lng).eq.'1') complbl = ' *1'
           if(fname(lng:lng).eq.'2') complbl = ' *2'
           if(fname(lng:lng).eq.'1'.or.fname(lng:lng).eq.'2') lng = lng-1
           lng = lng-4 !get rid of '.plt'
           write(mdlnr,'(I6)') n(f)
           if(n(f).le.9999) write(mdlnr,'(I5)') n(f)
           if(n(f).le.999) write(mdlnr,'(I4)') n(f)
           col = colours(mod(f-1,ncolours)+1)
           call pgsci(col)
           call pgmtext('RV',0.5,real(46.-real(f))/45.,0.,trim(fname(1:lng))//trim(complbl)//trim(mdlnr))
        end do
     end if
     call pgsci(1)
  end if
  
  call pgsch(sch)
  
  
  
  if(nf.eq.1 .and. conv.eq.1) then
     call pgsci(1)
     pl = 1
     
     !Convection plot - replots axes at the end:
     !call plt_convection(nmax,nvar,n(pl),dat(pl,:,:),vx,ymin,ymax,nhp(pl),hp(pl,:),hlp,hlbl)
     call plt_convection(n(pl),nvar,n(pl),dat(pl,1:nvar,1:n(pl)), vx, lgx,lgy, ymin,ymax, nhp(pl),hp(pl,:),hlp,hlbl)
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
        if(lgy) then
           call pgline(n(pl),xx(pl,1:n(pl)),log10(real(dat(pl,21,1:n(pl)))))
        else 
           call pgline(n(pl),xx(pl,1:n(pl)),real(dat(pl,21,1:n(pl))))
        end if
     end do !pl
     call pgsls(1)
  end if
  
  if(vy.eq.122) then !Comp sat-MB with RVJ83-MB
     call pgsls(4)
     do pl=1,npl
        call pgline(n(pl),xx(pl,1:n(pl)),log10(real(dat(pl,38,1:n(pl)))))
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
        inquire(file=trim(psname), exist=ex)                                                 !Check whether the file already exists
        if(.not.ex) then
           call system('mv -f plot_plt_000.eps '//trim(psname))
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
  
900 continue
  if(plot.ne.0.and.plot.ne.9) then
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
  
  io = -1
  write(*,*)
  do while(io.ne.0)
     write(6,'(A)', advance='no') ' What do you want to do ?  '
     read(*,*, iostat=io) plot
  end do
  if(plot.lt.0.or.plot.gt.11) goto 900
  
  if(plot.ne.4.and.plot.ne.10) call pgend
  
  if(plot.eq.1) goto 30
  if(plot.eq.2) goto 37
  if(plot.eq.3) goto 70
  if(plot.eq.6) goto 7
  if(plot.eq.8) then
     deallocate(dat, n,strmdls, xx,yy,miny,excly, hp,nhp)
     goto 5
  end if
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
  
  if(plot.eq.7) then  ! Auto update
     write(*,'(/,A)', advance='no') 'Auto-updating...  '
     wait = 2         ! Pause in seconds
     goto 7
  end if
  
  if(plot.eq.10) then  !Identify closest model
     xsel = 0.
     ysel = 0.
     write(6,'(A)')' Select a point in the graph and press "x" to finish'
     nsel=0
     call pgsci(1)
     call pgolin(1,nsel,xsel,ysel,2)
     
     
     dx = abs(xmax-xmin)
     dy = abs(ymax-ymin)
     mindist = huge(mindist)
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
     write(6,'(A,ES12.4,A,ES12.4,A,I5,A,I6)')' Closest model:     x =',xx(pl0,i0),',  y =',yy(pl0,i0),  &
          '    line =',i0+1,',  model =',nint(dat(pl0,1,i0))
     
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
     write(6,'(A)')' Line   Mdl     t (yr)   M(Mo)   Mhe   Mco   Menv    R (Ro)   L (Lo)    Te (K)   Tc (K)'//  &
          '       V    B-V     Xc    Yc   Porb(d)     dM/dt  M2/Mo'
     d = dat(pl0,:,i0)
     write(6,'(I5,I6,ES11.4,F8.3,2F6.3,F7.3,2(1x,2ES9.2),1x,2F7.3,1x,2F6.3,2ES10.2,F7.3)')i0+1,nint(d(1)),d(2),d(4),d(5),d(6),  &
          d(63),d(8),d(9),d(10),d(11),d(101),d(103),d(56),d(57),d(28),abs(d(31)),d(40)
     write(6,*)''
     
     !col = 2
     col = colours(mod(pl0-1,ncolours)+1)
     call pgsci(col)
     
     call pgpoint(1,xx(pl0,i0),yy(pl0,i0),2)
     write(hlbls,'(I5)')nint(dat(pl0,1,i0))
     call pgptxt(xx(pl0,i0),yy(pl0,i0),0.,0.,hlbls)
     
     call pgsci(1)
     goto 900
  end if
  
  if(plot.eq.11) then  ! Toggle between drawing lines, dots, or both
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
  write(6,'(/,A,/)')' Program finished.'
  
end program plotplt
!***********************************************************************************************************************************




