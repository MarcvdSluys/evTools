!Plots the data contained in different star.plt*-files into one graph
!This program reads and plots data from the plot output file from Eggletons code, the TWIN version of 2003 or 2005
!This is free-format fortran, so compile with -free (ifort) or --nfix (lf95)  (or rename files to .f90)
!Uses code in functions.f
!Requires the file ~/usr/lib/UBVRI.Kur to calculate colours
!AF, 18-05-2005. Works for ifort on MacOS, 12-10-2006.
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


program plotpltn  
  use constants
  use ubvdata
  implicit none
  integer, parameter :: nn=10000,nc=100,nff=120  !10000x100x120 takes ~1Gb!
  real*8, allocatable :: dat(:,:,:)
  real*8 :: mbol,bc
  real :: xx(nff,nn),yy(nff,nn),xx1(nn),yy1(nn),minx(nff),miny(nff)
  real :: xmin,xmax,dx,ymin,ymax,dy
  real ::xsel(4),ysel(4)
  integer :: io,col,lgx,lgy,nsel,exclx(nff),excly(nff),os,whitebg,strmdls(nn),system,colours(99),ncolours
  
  integer i,j,nf,f,n(nff),vx,vy,hrd,plot,ncols,l,drawlines,ansi,plhrdrad,prtitle,prlegend
  character :: fnames(nff)*99,fname*99,psname*99,tmpstr*10,boxx*19,boxy*19
  character :: rng,log,pglabels(100)*99,pglx*99,pgly*99,asclabels(100)*99,asclx*99,ascly*99,title*100,pstitle*99
  logical :: ex
  
  call setconstants()
  
  
  !****************************************************************************************************
  !Settings:
  
  !os = getos() !1-Linux, 2-MacOS
  os = 1        !Don't use Mac OS's silly AquaTerm
  whitebg = 1   !0: black background on screen, 1: white
  drawlines = 1 !0: no; draw points, 1: yes: draw lines, 2: draw both
  plhrdrad  = 1 !Draw lines of constant radius in HRD
  prtitle   = 0 !Print dir as title: 0-no, 1-yes
  prlegend  = 1 !Print input file names to right of plot as legend: 0-no, 1-yes
  
  ncolours = 13 !Number of colours used to distinguish tracks.  Default: 13
  colours(1:ncolours) = (/2,3,4,5,6,7,8,9,10,11,12,13,1/)  !Use black as last resort
  !ncolours = 4
  !colours(1:ncolours) = (/15,2,15,4/) !Grey-red-grey-blue
  
  
  !Predefined colours in PGPlot:
  ! 0: background 
  ! 1: foreground 
  ! 2: red                       9: light green
  ! 3: dark green               10: light blue-green?
  ! 4: dark blue                11: light blue
  ! 5: very light blue          12: dark purple
  ! 6: light purple             13: red-purple
  ! 7: yellow                   14: dark grey
  ! 8: orange                   15: light grey
  
  !****************************************************************************************************
  
  
  
  
  
  
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
  
  lgx = 0
  lgy = 0
  
  call set_plotpltn_labels(pglabels,asclabels,100)
  
  i = system('pwd > tmppwd.txt')
  open(unit=10,form='formatted',status='old',file='tmppwd.txt')
  rewind 10
  read(10,'(a100)')title
  close(10)
  i = system('rm tmppwd.txt')

  plot = 0
5 nf = iargc()
  if(nf.gt.0.and.nf.le.nff) then
     do i=1,nf
        call getarg(i,fnames(i))
     end do
  end if
  if(nf.eq.0) then
     call findfiles('*.plt*',nff,0,fnames,nf)
     if(nf.eq.0) goto 9999
  end if
  if(nf.gt.nff) write(6,'(A,I4)')'  Too many input files.  Maximum is',nff
  
  allocate(dat(nf,nc,nn))
  
  
  plot = 1
  
  !************************************************************************      
  !***   READ THE INPUT FILE
  !************************************************************************      
  
7 write(6,*)''
  do f=1,nf
     write(6,'(A,$)')'  Reading file '//trim(fnames(f))//','
     if(nf.eq.1) write(*,*)

     dat(f,:,:) = 0.d0
     open(unit=10,form='formatted',status='old',file=fnames(f))
     rewind 10
     read(10,*)ncols
     write(6,'(I4,A,$)')ncols,' columns.'
     do j=1,nn
        read(10,*,err=12,end=11) (dat(f,i,j),i=1,ncols)
        !dat(f,1,j) = j
     end do
     write(6,'(A)')'  End of file reached, arrays too small!'
     close(10)
     goto 15
11   write(6,'(A,I6,A)')'  End of the file reached,',j-1,' lines read.'
     close(10)
     goto 15
12   if(nf.eq.1) write(6,'(A,I6)')'  Error reading file, line',j
     close(10)
     if(j.lt.3) goto 19
     write(6,'(A)')"  I'll skip the rest of the file and use the first part."
15   continue
     do i=4,nc !skip t,dt
        if(dat(f,i,1).eq.0.) dat(f,i,1) = dat(f,i,2)
     end do !i
     n(f) = j-1   !Number of models in the file
     goto 29
     
     
     !*** New output format (2005)
19   if(nf.eq.1) write(6,'(A)')'  I will try the new output format...'
     dat(f,:,:) = 0.d0
     open(unit=20,form='formatted',status='old',file=fnames(f))
     rewind 20
     read(20,*)ncols
     do j=1,nn
        !if(ncols.eq.81) read(20,'(F6.0,E17.9,E14.6,12E13.5,7E12.4,3E13.5,16E12.4,39E13.5,E14.6)',err=22,end=21) (dat(f,i,j),i=1,81)  !81 Columns
        if(ncols.eq.81) read(20,*,err=22,end=21) (dat(f,i,j),i=1,81)  !81 Columns
        if(ncols.gt.81) then
           !read(20,'(F6.0,E17.9,E14.6,12E13.5,7E12.4,3E13.5,16E12.4,39E13.5,E14.6,ES13.5,F2.0)',err=22,end=21) (dat(f,i,j),i=1,83)  !83 Columns, Evert(?) added 82, 83=strmdl flag
           read(20,*,err=22,end=21) (dat(f,i,j),i=1,83)  !83 Columns, Evert(?) added 82, 83=strmdl flag
           strmdls(j) = nint(dat(f,83,j)) !Structure model was saved (1) or not (0)
        end if
     end do
     write(6,'(A)')'  End of file reached, arrays too small!'
     close(20)
     goto 25
21   write(6,'(A,I6,A)')'  End of the file reached,',j-1,' lines read.'
     close(20)
     goto 25
22   write(6,'(A,I6)')'  Error reading file, aborting at line',j
     if(j.lt.3) goto 9999
     write(6,'(A)')"  I'll skip the rest of the file and use the first part."
     close(20)
25   continue
     if(nf.eq.1) write(6,*)''
     n(f) = j-1   !Number of models in the file


     !************************************************************************
     !***  CHANGE/ADD VARIABLES   ***
     !************************************************************************

29   continue
     do i=8,14
        dat(f,i,1:n(f)) = 10**dat(f,i,1:n(f))
     end do
     dat(f,15,1:n(f)) = dat(f,15,1:n(f))*m0*1.d-40                                         !Ubind in 10^40 erg
     dat(f,71,1:n(f)) = dat(f,5,1:n(f)) - dat(f,6,1:n(f))                                  !Mhe-CO: intershell mass
     dat(f,72,1:n(f)) = dat(f,22,1:n(f))*dat(f,4,1:n(f))*dat(f,8,1:n(f))**2                !Moment of inertia
     dat(f,73,1:n(f)) = dat(f,4,1:n(f))*m0/(4/3.d0*pi*(dat(f,8,1:n(f))*r0)**3)             !Average Rho
     dat(f,74,1:n(f)) = dat(f,81,1:n(f))                                                   !Move Qconv from 81 to 74
     dat(f,75,1:n(f)) = 1.d0 - dat(f,42,1:n(f)) - dat(f,43,1:n(f))                         !Z_surf = 1 - X - Y
     dat(f,76,1:n(f)) = dat(f,2,n(f))-min(dat(f,2,1:n(f)),dat(f,2,n(f))-1.d4)              !t_f - t
     dat(f,77,1:n(f)) = dat(f,2,1:n(f))/(dat(f,2,n(f))+1.e-30)                             !t/t_f
     dat(f,78,1:n(f)) = dat(f,17,1:n(f))/(dat(f,16,1:n(f))+1.e-30)                         !L_He/L_H
     
     !Colours
     do i=1,n(f)
        call lt2ubv(dlog10(dat(f,9,i)),dlog10(dat(f,10,i)),dat(f,4,i),dlog10(dat(f,75,i)/2.d-2),mbol,bc,dat(f,81,i),dat(f,82,i),dat(f,83,i),dat(f,84,i),dat(f,85,i))
        dat(f,86,i) = dat(f,82,i)+dat(f,83,i)  !(U-V) = (U-B) + (B-V)
        dat(f,87,i) = dat(f,84,i)+dat(f,85,i)  !(V-I) = (V-R) + (R-I)
     end do
     
     dat(f,88,1:n(f)) = dat(f,8,1:n(f))**2*dat(f,22,1:n(f))  !k^2*R^2
     
     dat(f,89,1:n(f)) = dat(f,4,1:n(f)) - dat(f,5,1:n(f))                         !H-envelope mass
     dat(f,90,1:n(f)) = g*dat(f,4,1:n(f))*dat(f,89,1:n(f))*m0**2 / (dat(f,15,1:n(f))*dat(f,8,1:n(f))*r0*1.d40+1.d-30)  !lambda_env = G*M*M_env/(Ubind*R)
     !dat(f,90,1:n(f)) = dabs(dat(f,90,1:n(f)))    !This 'hides' the fact that Ubind changes sign
     dat(f,90,1:n(f)) = max(dat(f,90,1:n(f)),0.d0)
     do i=1,n(f)
        if(dabs(dat(f,5,i)).lt.1.d-29) dat(f,90,i) = 0.d0  !If there's no He core mass, there's no lambda
        !write(*,'(I6,9ES20.5)')i,dat(f,4:5,i),dat(f,83,i),dat(f,15,i),dat(f,8,i),dat(f,90,i)
     end do
     
     !91: Ratio of the two terms in the "Reimers-like" mass-loss prescription in the Eggleton code:
     do i=1,n(f)
        if(dat(f,15,i).gt.0.d0) dat(f,91,i) = 3.29d-5*dat(f,4,i)/(dat(f,15,i)/1.d10)  !3.29e-5 * (M/Mo) * (Ebind/10^50 erg)^-1
     end do
     do i=n(f)-1,1,-1
        if(dat(f,91,i).eq.0.d0) dat(f,91,i) = dat(f,91,i+1)
     end do
  end do !f, file
  
  
  write(6,*)''
  
  
  
  !************************************************************************      
  !***   CHOOSE PLOT VARIABLES
  !************************************************************************      
  
30 if(plot.ne.6) then      
     write(6,*)''
     write(6,'(A70)')'  Variables:                                   0: Quit                '
     write(6,'(A70)')'                                                                      '
     write(6,'(A70)')'  1: model      15: Ubind      28: Porb       34: Horb                '
     write(6,'(A70)')'  2: t          16: Lh         29: FLR        35: dHorb/dt            '
     write(6,'(A70)')'  3: dt         17: Lhe        30: F1         36: dHgw/dt             '
     write(6,'(A70)')'  4: M          18: Lc         31: dM         37: dHwml/dt            '
     write(6,'(A70)')'  5: Mhe        19: Lnu        32: dMwind     38: dHs-o/dt            '
     write(6,'(A70)')'  6: Mco        20: Lth        33: dMmt       39: dHmtr/dt            '
     write(6,'(A70)')'  7: Mone       21: Prot                      40: Mcomp               '
     write(6,'(A70)')'  8: R          22: k^2                       41: e                   '
     write(6,'(A70)')'  9: L          23: Rcz                                               '
     write(6,'(A70)')' 10: Teff       24: dRcz             H   He  C   N   O   Ne  Mg       '
     write(6,'(A70)')' 11: Tc         25: Tet        Surf  42  43  44  45  46  47  48  Surf '
     write(6,'(A70)')' 12: Tmax       26: Ralv       Tmax  49  50  51  52  53  54  55  Tmax '
     write(6,'(A70)')' 13: Rhoc       27: Bp         Core  56  57  58  59  60  61  62  Core '
     write(6,'(A70)')' 14: RhoTm                                                            '
     write(6,'(A70)')'                                                                      '
     write(6,'(A70)')' 71: Mhe-MCO                   81: V     86: U-V             101: HRD '
     write(6,'(A70)')' 72: M.I.                      82: U-B   87: V-R                      '
     write(6,'(A70)')' 73: Rho_avg                   83: B-V   88: (kR)^2                   '
     write(6,'(A70)')' 74: Qconv                     84: V-I   89: M_env                    '
     write(6,'(A70)')' 75: Zsurf                     85: I-R   90: lambda_env               '
     write(6,'(A70)')' 76: t_f-t                               91: Reimers ratio            '
     write(6,'(A70)')' 77: t/t_f                                                            '
     write(6,'(A70)')' 78: Lhe/Lh                                                           '
     write(6,'(A70)')'                                                                      '
35   write(6,'(A,$)')'  Choose the X-axis variable (1-101): '
     read*,vx
     if(vx.eq.0) goto 9999
     if(vx.lt.0.or.vx.gt.101) goto 35
     if(vx.gt.78.and.vx.lt.81) goto 35
     if(vx.gt.91.and.vx.lt.101) goto 35
  end if   !if(plot.ne.6) then   
  
  
  if(plot.ne.6) hrd = 0
  if(vx.eq.101) hrd = 1
  if(hrd.eq.1) goto 50
  
  if(plot.lt.2) then      
36   write(6,'(A,$)')'  Choose the Y-axis variable (1-91): '
     read*,vy
     if(vy.eq.0) goto 9999
     if(vy.lt.0.or.vy.gt.91) goto 36
     if(vy.gt.78.and.vy.lt.81) goto 36
  end if   !if(plot.lt.2) then   
  
  do f=1,nf
     xx(f,1:nn) = real(dat(f,vx,1:nn))  !Problem with converting to float: small time ranges after a long time are rounded 
     yy(f,1:nn) = real(dat(f,vy,1:nn))  !to the same floating number
  end do !f
  
  
  
  
  !************************************************************************      
  !***   LIN/LOG AXES
  !************************************************************************      
37 if(plot.ne.6) then      
     write(6,'(A,$)')'  Do you want a logarithmic scale:  (N)o, (X)-axis, (Y)-axis, (B)oth: '
     read*,log
     if(log.eq.'X') log='x'
     if(log.eq.'Y') log='y'
     if(log.eq.'B') log='b'
     if(log.eq.'N') log='n'
  end if  !if(plot.ne.6) then   
  
  lgx = 0
  lgy = 0
  if(log.eq.'x'.or.log.eq.'b') lgx = 1
  if(log.eq.'y'.or.log.eq.'b') lgy = 1
  
  
  pglx = pglabels(vx)
  pgly = pglabels(vy)
  asclx = asclabels(vx)
  ascly = asclabels(vy)
  
  exclx = 0
  if(lgx.eq.1) then
     do f=1,nf
        if(xx(f,1).eq.0.) xx(f,1) = xx(f,2)
        minx(f) = 1.e33
        do j=1,n(f)
           if(abs(xx(f,j)).lt.minx(f).and.abs(xx(f,j)).ne.0.) minx(f) = abs(xx(f,j))
        end do
        xx(f,1:n(f)) = log10(abs(xx(f,1:n(f)))+minx(f)*1.e-3)
        if(abs(minx(f)-1.e33).lt.1e32) exclx(f) = 1  !Exclude it in determining ranges
     end do
  end if
  excly = 0
  if(lgy.eq.1) then
     do f=1,nf
        if(yy(f,1).eq.0.) yy(f,1) = yy(f,2)
        miny(f) = 1.e33
        do j=1,n(f)
           if(abs(yy(f,j)).lt.miny(f).and.abs(yy(f,j)).ne.0.) miny(f) = abs(yy(f,j))
        end do
        yy(f,1:n(f)) = log10(abs(yy(f,1:n(f)))+miny(f)*1.e-3)
        if(abs(miny(f)-1.e33).lt.1e32) excly(f) = 1  !Exclude it in determining ranges
     end do
  end if
  
  
50 if(hrd.eq.1) then
     vx = 10
     vy = 9
     pglx = trim(pglabels(vx))
     pgly = trim(pglabels(vy))
     asclx = trim(asclabels(vx))
     ascly = trim(asclabels(vy))
     lgx = 1
     lgy = 1
     
     do f=1,nf
        xx(f,1:n(f)) = real(dlog10(abs(dat(f,vx,1:n(f)))))
        yy(f,1:n(f)) = real(dlog10(abs(dat(f,vy,1:n(f)))))
     end do
  end if
  
  
  !Find plot range
  xmin = 1.e33
  xmax = -1.e33
  do f=1,nf
     if(exclx(f).eq.1) cycle
     xmin = min(minval(xx(f,1:n(f))),xmin)
     xmax = max(maxval(xx(f,1:n(f))),xmax)
  end do
  ymin = 1.e33
  ymax = -1.e33
  do f=1,nf
     if(excly(f).eq.1) cycle
     ymin = min(minval(yy(f,1:n(f))),ymin)
     ymax = max(maxval(yy(f,1:n(f))),ymax)
  end do
  
  
  
  !Limit ranges for logged axes like Mdot
  if(lgx.eq.1) then
     if(vx.ge.31.and.vx.le.33.and.xmin.lt.-12.) xmin = -12.
  end if
  if(lgy.eq.1) then
     if(vy.ge.31.and.vy.le.33.and.ymin.lt.-12.) ymin = -12.
     if(vy.ge.35.and.vy.le.39.and.ymin.lt.-18.) ymin = -18.
     if(vy.ge.35.and.vy.le.39.and.ymax.gt.-12.) ymax = -12.
  end if
  
  
  
  
  
  
  
  
  
  
  
  
  !************************************************************************      
  !***   PLOT RANGE
  !************************************************************************      
  
70 if(plot.ne.6) then      
     write(6,*)''
     write(6,*)'  X-range:',xmin,'-',xmax
     write(6,*)'  Y-range:',ymin,'-',ymax
     write(6,'(A,$)')'  Do you want to change a plot range ?  (N)o, (X)-axis, (Y)-axis, (B)oth: '
     read*,rng
     
     if(rng.eq.'N') rng='n'
     if(rng.eq.'X') rng='x'
     if(rng.eq.'Y') rng='y'
     if(rng.eq.'B') rng='b'
     
     if(rng.eq.'n'.or.rng.eq.' ') goto 100
     
     if(rng.eq.'x'.or.rng.eq.'b') then
        write(6,'(A,$)')'  Give the new range for the X-axis (Xmin, Xmax): '
        read*,xmin,xmax
        if(xmin.gt.xmax) then
           call rswap(xmin,xmax)
           write(6,'(A)')'  I swapped Xmin and Xmax'
        end if !if(xmin.gt.xmax)
     end if !if(rng.eq.'x'.or.rng.eq.'b')
     
     
     if(rng.eq.'y'.or.rng.eq.'b') then
        write(6,'(A51,$)')'  Give the new range for the Y-axis (Ymin, Ymax): '
        read*,ymin,ymax
        if(ymin.gt.ymax) then
           call rswap(ymin,ymax)
           write(6,'(A)')'  I swapped Ymin and Ymax'
        end if !if(ymin.gt.ymax)
     end if !if(rng.eq.'y'.or.rng.eq.'b')
  end if  !if(plot.ne.6) then   
  
  
  write(6,*)'' 
  !Limit ranges for logged axes like Mdot, again!
  if(lgx.eq.1) then
     if(vx.ge.31.and.vx.le.33.and.xmin.lt.-12.) xmin = -12.
  end if
  if(lgy.eq.1) then
     if(vy.ge.31.and.vy.le.33.and.ymin.lt.-12.) ymin = -12.
     if(vy.ge.35.and.vy.le.39.and.ymin.lt.-18.) ymin = -18.
     if(vy.ge.35.and.vy.le.39.and.ymax.gt.-12.) ymax = -12.
  end if
  
  
100 continue
  if(plot.eq.3.and.rng.eq.'n') goto 129
  if(plot.eq.3.and.rng.eq.'y') goto 125
  dx = 0.02*abs(xmax-xmin)
  if(dx.eq.0.) dx = 0.05*xmax
  xmin = xmin - dx
  xmax = xmax + dx
125 if(plot.eq.3.and.rng.eq.'x') goto 129
  dy = 0.02*abs(ymax-ymin)
  if(dy.eq.0.) dy = 0.05*ymax
  ymin = ymin - dy
  ymax = ymax + dy
129 continue
  
  
  
  
  
  !************************************************************************    
  !***   PLOT TO SCREEN/FILE
  !************************************************************************      
  
501 continue
  
  if((hrd.eq.1.or.vx.eq.76.or.vx.eq.81) .and. (xmin.lt.xmax)) call rswap(xmin,xmax)
  if((vy.eq.76.or.vy.eq.81) .and. (ymin.lt.ymax)) call rswap(ymin,ymax)
  if(plot.ne.0.and.plot.ne.8) then
     write(6,*)''    
     write(6,*)'  X-range:',xmin,'-',xmax
     write(6,*)'  Y-range:',ymin,'-',ymax
     write(6,*)''    
  end if
  
  
  
  if(plot.eq.8) then
     call pgbegin(1,'plot_pltn_000.eps/cps',1,1)
     !call pgpap(11.0,0.70) !Make it fit on letter
     call pgpap(10.0,1.0)   !Talk,poster?
  else
     if(os.eq.1) call pgbegin(1,'/xserve',1,1)
     if(os.eq.2) call pgbegin(1,'/aqt',1,1)                !Use Aquaterm on MacOSX
     call pgpap(scrsz,scrrat)
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
  end if
  
  if(whitebg.eq.1.and.plot.ne.8) then     !Create a white background; swap black (ci=0) and white (ci=1)
     call pgscr(0,1.,1.,1.)  !Repeat this, to make it work for AquaTerm, for which it was designed
     call pgscr(1,0.,0.,0.)
     call pgsci(1)
     call pgsci(0)
     call pgsvp(0.,1.,0.,1.)
     call pgswin(-1.,1.,-1.,1.)
     call pgrect(-2.,2.,-2.,2.)
     call pgsci(1)
  end if
  
  
  call pgscf(1)  !'Arial'
  !if(os.eq.2.or.plot.eq.8) call pgscf(2)  !'Roman'
  if(prlegend.eq.0) then
     call pgsvp(0.10,0.96,0.07,0.96)
  else  !Make room for 'legend'
     call pgsvp(0.10,0.90,0.07,0.96)
  end if
  if(hrd.eq.1.and.plhrdrad.eq.1) then  !Make room for Radius labels
     dx = (xmax - xmin)*0.02
     dy = (ymax - ymin)*0.05
     call pgswin(xmin-dx,xmax,ymin,ymax+dy)
  else
     call pgswin(xmin,xmax,ymin,ymax)
  end if
  
  boxx = 'BCNTS'
  boxy = 'BCNTS'
  if(lgx.ge.1) boxx = 'BCLNTS'
  if(lgy.ge.1) boxy = 'BCLNTS'
  call pgbox(trim(boxx),0.0,0,trim(boxy),0.0,0)
  if(prtitle.eq.1) call pgmtxt('T',0.7,0.5,0.5,'~/'//trim(title(13:100))//'/')
  call pgmtxt('B',2.4,0.5,0.5,trim(pglx))
  call pgmtxt('L',2.0,0.5,0.5,trim(pgly))
  
  !Plot lines of constant R in HRD
  call pgsch(0.8)
  if(hrd.eq.1.and.plhrdrad.eq.1) call plotlinesofconstantradius(xmin,xmax,ymin,ymax)

  
  call pgsch(0.6)
  do f=1,nf
     col = colours(mod(f-1,ncolours)+1)  !2,3,...,ncolours,1,2,...
     call pgsci(col)
     xx1(1:n(f)) = xx(f,1:n(f))
     yy1(1:n(f)) = yy(f,1:n(f)) 
     if(drawlines.eq.0) call pgpoint(n(f),xx1(1:n(f)),yy1(1:n(f)),1)
     if(drawlines.ge.1) call pgline(n(f),xx1(1:n(f)),yy1(1:n(f)))
     if(drawlines.eq.2) call pgpoint(n(f),xx1(1:n(f)),yy1(1:n(f)),20)
     if(prlegend.ge.1) then !Print legend
        fname = fnames(f)
        l = len_trim(fname)
        if(fname(l:l).eq.'1'.or.fname(l:l).eq.'2') l = l-1
        l = l-4 !get rid of '.plt'
        call pgmtext('RV',0.5,real(46.-f)/45.,0.,fname(1:l))
     end if
  end do
  
  call pgsls(2)
  call pgsls(1)
  
  call pgsch(2.)
  if(1.eq.2) then
     if(vx.eq.83.and.vy.eq.81) then
        write(6,'(A)')'  Plotting Gamma Leonis'
        call pgsci(1)
        call pgsch(2.)
        
        !Simbad
        call pgerrx(1,1.15-0.07,1.15+0.07,-0.32,0.5)  !Gamma Leo A: V=2.61(+-0.05?), B-V=+1.15, Mv=-0.32
        call pgerry(1,1.15,-0.32-0.08,-0.32+0.08,0.5)
        call pgerrx(1,1.3-0.14,1.3+0.14,0.57,0.5)  !Gamma Leo B: V=3.5(+-0.1?), B-V=+1.3, Mv=+0.57
        call pgerry(1,1.3,0.57-0.12,0.57+0.12,0.5)
        
        call pgsch(1.)
     end if
     if(vx.eq.10.and.vy.eq.81) then
        write(6,'(A)')'  Plotting Gamma Leonis'
        call pgsci(1)
        call pgsch(2.)
        
        !Simbad
        call pgerry(1,4470.,-0.32-0.08,-0.32+0.08,0.5)  !Gamma Leo A: V=2.61(+-0.05?), Teff=4470K, Mv=-0.32
        call pgerry(1,4980.,0.57-0.12,0.57+0.12,0.5)    !Gamma Leo B: V=3.5(+-0.1?), Teff=4980K, Mv=+0.57
        call pgerry(1,log10(4470.),-0.32-0.08,-0.32+0.08,0.5)  !Gamma Leo A: V=2.61(+-0.05?), Teff=4470K, Mv=-0.32
        call pgerry(1,log10(4980.),0.57-0.12,0.57+0.12,0.5)    !Gamma Leo B: V=3.5(+-0.1?), Teff=4980K, Mv=+0.57
        
        call pgsch(1.)
     end if
  end if
  
  if(plot.eq.8) then
     call pgend
     ex = .true.
     pstitle = 'PlotPltN output: '//trim(asclx)//' vs. '//trim(ascly)//'.'
     i = 1
     do while(ex)
        write(psname,'(A,I3.3,A)')'plot_pltn__'//trim(asclx)//'--'//trim(ascly)//'_',i,'.eps'
        if(hrd.ge.1) write(psname,'(A,I3.3,A)')'plot_pltn__HRD_',i,'.eps'
        inquire(file=trim(psname), exist=ex) !Check whether the file already exists; ex is True or False
        if(.not.ex) then
           j = system('mv -f plot_pltn_000.eps '//trim(psname))
           call set_PGPS_title(trim(psname),trim(pstitle))
        end if
        i = i+1
     end do
     write(6,'(A)')' Plot saved to '//trim(psname)
     plot = 0
     goto 501  !Redo the screen plot, in case option 4 gets selected
  end if
  !End of the plotting
  
  
  
  
  
  
  
  
  
  
  
  
  
  !************************************************************************    
  !***   POST-PLOT MENU   ***
  !************************************************************************      
  
900 if(plot.ne.0.and.plot.ne.8) then
     write(6,*)''
     write(6,'(A)')'  You can:'
     write(6,'(A)')'   0) quit'
     write(6,'(A)')'   1) change variables'
     write(6,'(A)')'   2) change lin/log axes'
     write(6,'(A)')'   3) change axis ranges'
     write(6,'(A)')'   4) select zoom region'
     write(6,'(A)')'   5) zoom out'
     write(6,'(A)')'   6) reread files and make same plot'
     write(6,'(A)')'   7) change input file'
     write(6,'(A)')'   8) save plot to postscript'
     write(6,'(A)')'   9) toggle drawing line/points'
  end if !if(plot.ne.8) then
  write(6,*)''
  write(6,'(A,$)')'  What do you want to do ?  '
  read(5,*,iostat=io) plot
  if(io.ne.0) goto 900
  if(plot.lt.0.or.plot.gt.9) goto 900
  
  if(plot.ne.4) call pgend
  if(plot.eq.1) goto 30
  if(plot.eq.2) goto 37
  if(plot.eq.3) goto 70
  if(plot.eq.6) goto 7
  if(plot.eq.7) then
     deallocate(dat)
     goto 5
  end if
  if(plot.eq.8) goto 501
  
  if(plot.eq.4) then  !Select region
941  call pgsci(1)
     xsel = 0.
     ysel = 0.
     write(6,'(A)')'  Select 2-4 corner points with your left mouse button and press "x" to finish'
     nsel=0
     call pgolin(4,nsel,xsel,ysel,2)
     if(nsel.lt.2) then
        write(6,'(A)')'  I need at least 2 corner points...'
        goto 941
     end if
     xmin = minval(xsel(1:nsel))  !The new window is drawn for the extreme values of these points
     xmax = maxval(xsel(1:nsel))
     ymin = minval(ysel(1:nsel))
     ymax = maxval(ysel(1:nsel))
     write(6,*)''
     write(6,*)'  X-range:',xmin,'-',xmax
     write(6,*)'  Y-range:',ymin,'-',ymax
     write(6,*)''
     call pgend
     goto 501
  end if
  
  if(plot.eq.5) then  !Zoom out
     xmin = (xmin+xmax)/2. - 2*abs((xmin+xmax)/2.-xmin) !Central value - 2x the 'radius', 'radius' = central value - minimum
     xmax = (xmin+xmax)/2. + 2*abs((xmin+xmax)/2.-xmin)
     ymin = (ymin+ymax)/2. - 2*abs((ymin+ymax)/2.-ymin)
     ymax = (ymin+ymax)/2. + 2*abs((ymin+ymax)/2.-ymin)
     if(hrd.eq.1) call rswap(xmin,xmax)
     goto 501
  end if
  
  
  if(plot.eq.9) then !Toggle between drawing points, lines or both
     ansi=-1
     do while(ansi.lt.0.or.ansi.gt.3)
        write(*,'(A)')'  You can plot:'
        write(*,'(A)')'  0: keep the current choice'
        write(*,'(A)')'  1: dots'
        write(*,'(A)')'  2: lines'
        write(*,'(A)')'  3: both'
        write(*,'(A,$)')'  What would you like to plot?  '
        read*,ansi
     end do
     if(ansi.gt.0) drawlines = ansi-1 !0-2
     goto 501
  end if
  
  
  
  
9999 write(6,'(A)')'  Program finished'
  write(6,*)''
end program plotpltn
!************************************************************************      














