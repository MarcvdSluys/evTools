!Plots the data contained in a plt* file, highlight selected points
!Lines are longer than 72 chars, so add --wide (lf) or -132 (ifort) to compile
!Uses code in functions.f
!Requires the file ~/usr/lib/UBVRI.Kur to calculate colours
!Uses PGPLOT window 1 to plot to
!AF, 19-04-2006. Works for ifort on MacOS, 11-10-2006.

program plotplt
  use constants
  use ubvdata
  implicit none
  integer,parameter :: nn=30000,nvar=210,nc=81,nl=10
  real*8 :: dat(nvar,nn),d(nvar)
  real :: xx(nn),yy(nl,nn),yy1(nn),minx,miny(nl),dist,mindist
  real :: x,system,xmin,xmax,ymin,ymax,dx,dy,xmin0,xmax0,ymin0,ymax0
  real :: xsel(4),ysel(4),xc,yc,xm,ym
  
  integer :: i,i0,j,j0,n,vx,vy,plot,ny,excly(nl),drawlines,ver,verbose
  integer :: hrd,dhdt,conv,mdots,tscls,ch,dpdt,sabs,tabs,cabs,io
  integer :: nt,hp(1000),nhp,wait,lums,lgx,lgy,nsel,os,whitebg,strmdls(nn)
  integer :: ansi,xwini,pgopen
  real :: sch
  character :: findfile*99,fname*99,psname*99
  character :: rng,log,hlp,hlp1,hlbl,hlbls*5,lstr(6)*11
  character :: abstr(7)*2,xwin*19,tmpstr
  character :: labels(nvar)*99,lx*99,ly*99,title*99,title1*99
  logical :: ex
  
  !Set constants:
  call setconstants()
  
  sch = 1.0
  
  !os = getos() !1-Linux, 2-MacOS
  os = 1       !Don't use Mac OS's silly AquaTerm (watta mistaka to maka)
  whitebg = 1  !0: black background on screen, 1: white
  drawlines = 1 !0: no; draw points, 1: yes: draw lines, 2: draw both
  
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
  
  
  !Labels:
  lstr = (/'L\dsurf\u  ','L\dH\u     ','L\dHe\u    ','L\dC\u     ','L\d\(639)\u','L\dth\u    '/)  !Line labels for Luminosities plot
  abstr = (/'H ','He','C ','N ','O ','Ne','Mg'/)                                                  !Line labels for Abundances plots
  call getpltlabels(nvar,labels)                                                                  !Get the labels for the plot axes
  
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
  
  
  !Search for input file in current dir
  plot = 0
  xwini = 1  !Number of X window to try first
5 continue
  if(iargc().eq.1.and.plot.eq.0) then
     call getarg(1,fname)
  else
     fname=findfile('*.plt*',6) !Match string and its length
     if(fname(1:10).eq.'          ') goto 9999
  end if
  plot = 1
  
  
  !************************************************************************      
  !***   READ THE INPUT FILE
  !************************************************************************      
  
7 if(plot.ne.7) write(6,'(/,A)')' Reading file '//trim(fname)
  
  verbose = 1
  if(plot.eq.7) verbose = 0
  call readplt(10,fname,nn,nvar,nc,verbose,dat,n,ver)  !Use unit 10
  if(ver.eq.2005) strmdls(:) = nint(dat(83,:)) !Structure model was saved (1) or not (0)
  
  
  !************************************************************************      
  !***   CHANGE VARIABLES
  !************************************************************************      
  
  call changepltvars(nn,nvar,n,dat,labels,dpdt)  !Change (e.g. de-log) and add plot variables
  
  
  !************************************************************************      
  !***   CHOOSE PLOT VARIABLES
  !************************************************************************      
  
30 if(plot.ne.6.and.plot.ne.7) then      
     call printpltvarlist  !Print the list of variables in a *.plt? file to screen, for input menu
     
     
35   write(6,'(A37,$)')' Choose the X-axis variable (0-201): '
     read*,vx
     if(vx.eq.0) goto 9999
     if(vx.lt.0.or.vx.gt.201) goto 35
     if(vx.gt.62.and.vx.lt.81) goto 35
     if(vx.gt.114.and.vx.lt.201) goto 35
     
     hrd   = 0
     dhdt  = 0
     conv  = 0
     mdots = 0
     tscls = 0
     lums  = 0
     sabs  = 0
     tabs  = 0
     cabs  = 0
  end if   !if(plot.ne.6.and.plot.ne.7) then   
  
  ny = 1
  if(vx.eq.201.or.hrd.eq.1) then  !HRD
     hrd = 1
     xx = real(dlog10(abs(dat(10,1:nn))))
     yy(1,1:n) = real(dlog10(abs(dat(9,1:nn))))
     lx = trim('log '//labels(10))
     ly = trim('log '//labels(9))
     vy = 0
     lgx = 1
     lgy = 1
     goto 50
  end if
  
  if(plot.lt.2) then      
36   write(6,'(A53,$)')' Choose the Y-axis variable (0-62, 81-114, 202-209): '
     read*,vy
     if(vy.eq.0) goto 9999
     if(vy.lt.0.or.vy.gt.209) goto 36
     if(vy.gt.62.and.vy.lt.81) goto 36
     if(vy.gt.114.and.vy.lt.202) goto 36
  end if   !if(plot.lt.2) then   
  
  
37 continue
  if(vy.eq.25) then !Tet + analytic Tet
     ny = 2
     yy(2,1:n) = real(dat(89,1:n))
  end if
  if(vy.eq.93) then !Rrl
     ny = 2
     yy(2,1:n) = real(dat(8,1:n))
  end if
  if(vy.eq.202) then  !dH/dt
     dhdt = 1
     ny = 5
     yy(1:5,1:n) = real(dat(35:39,1:n))
  end if
  if(vy.eq.203.or.conv.eq.1) then  !Convection plot
     conv = 1
     vy = 4
  end if
  if(vy.eq.204) then !Mdots
     mdots = 1
     ny = 3
     yy(1:3,1:n) = real(dat(31:33,1:n))
  end if
  if(vy.eq.205) then !Timescales
     tscls = 1
     ny = 5
     yy(1:5,1:n) = real(dat(201:205,1:n))
  end if
  if(vy.eq.206) then !Luminosities
     lums = 1
     ny = 6
     yy(1,1:n) = real(dat(9,1:n))
     yy(2:6,1:n) = real(dat(16:20,1:n))
  end if
  
  if(vy.eq.207) then !Surface abundances
     sabs = 1
     ny = 7
     yy(1:7,1:n) = real(dat(42:48,1:n))
  end if
  if(vy.eq.208) then !Tmax abundances
     tabs = 1
     ny = 7
     yy(1:7,1:n) = real(dat(49:55,1:n))
  end if
  if(vy.eq.209) then !Core abundances
     cabs = 1
     ny = 7
     yy(1:7,1:n) = real(dat(56:62,1:n))
  end if
  
  xx = real(dat(vx,1:nn))         
  if(vy.lt.200) yy(1,1:n) = real(dat(vy,1:nn))  
  
  
  
  
  
  
  
  
  
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
  
  
  lx = labels(vx)
  ly = labels(vy)
  
  if(lgx.eq.1) then
     if(xx(1).le.0.) xx(1)=xx(2)
     minx = 1.e33
     do j=1,n
        if(abs(xx(j)).lt.minx.and.abs(xx(j)).ne.0.) minx = abs(xx(j))
     end do
     xx(1:n) = log10(abs(xx(1:n))+minx*1.e-3)
     lx = trim('log '//lx)
  end if
  if(dhdt.eq.1) lgy = 1
  excly = 0
  if(lgy.eq.1) then
     do i=1,ny
        if(yy(i,1).eq.0.) yy(i,1) = yy(i,2)
        miny(i) = 1.e33
        do j=1,n
           if(abs(yy(i,j)).lt.miny(i).and.abs(yy(i,j)).ne.0.) miny(i) = abs(yy(i,j))
        end do
        yy(i,1:n) = log10(abs(yy(i,1:n))+miny(i)*1.e-3)
        if(abs(miny(i)-1.e33).lt.1e32) excly(i) = 1  !Exclude it in determining ranges
     end do
     ly = trim('log '//ly)
  end if
  
50 continue !HRD   
  xmin = minval(xx(1:n))
  xmax = maxval(xx(1:n))
  
  ymin = 1.e33
  ymax = -1.e33
  do i=1,ny
     if(excly(i).eq.1) cycle
     ymin = min(minval(yy(i,1:n)),ymin)
     ymax = max(maxval(yy(i,1:n)),ymax)
  end do
  
  if(vx.eq.85) then !R/(dR/dt)
     if(xmin.lt.1.e6.and.lgx.eq.0) xmin = 1.e6
     if(xmin.lt.6..and.lgx.eq.1) xmin = 6.
     if(xmax.gt.1.e12.and.lgx.eq.0) xmax = 1.e12
     if(xmax.gt.12..and.lgx.eq.0) xmax = 12.
  end if
  if(vy.eq.85) then !R/(dR/dt)
     if(ymin.lt.1.e6.and.lgy.eq.0) ymin = 1.e6
     if(ymin.lt.6..and.lgy.eq.1) ymin = 6.
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
     if(vy.eq.204.and.ymin.lt.-12.) ymin = -12.
     if((vy.ge.35.and.vy.le.39).or.vy.eq.202) then
        if(ymin.lt.-18..and.dpdt.eq.0) ymin = -18.
        if(ymin.lt.-15..and.dpdt.eq.1) ymin = -15.
        if(ymax.gt.12..and.dpdt.eq.2) ymax = 12.
     end if
  end if
  if(vy.eq.205.and.lgy.eq.1.and.ymin.lt.4.) ymin = 4.
  if(vy.eq.205.and.lgy.eq.1.and.ymax.gt.15.) ymax = 15.
  if(vy.eq.88.and.ymin.lt.-20.) ymin = -20.
  
  
  
  
  
  
  
  
  
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
     if(vx.ge.31.and.vx.le.33.and.xmin.lt.-12.) xmin= -12.
  end if
  if(lgy.eq.1) then
     if(vy.ge.31.and.vy.le.33.and.ymin.lt.-12.) ymin = -12.
     if(vy.eq.204.and.ymin.lt.-12.) ymin = -12.
     if((vy.ge.35.and.vy.le.39).or.vy.eq.202) then
        if(ymin.lt.-18..and.dpdt.eq.0) ymin = -18.
        if(ymin.lt.-15..and.dpdt.eq.1) ymin = -15.
        if(ymax.gt.12..and.dpdt.eq.2) ymax = 12.
     end if
  end if
  if(vy.eq.88.and.ymin.gt.-20.) ymin = -20.
  
  
  
  
  
  
  
  
  
  !************************************************************************      
  !***   Line style legend for some cases, want to replace these by colours and in-plot legends
  !************************************************************************      
  
100 continue
  if(dhdt.eq.1) then
     write(6,*)''
     write(6,'(A)')'  White       : total angular momentum loss'
     write(6,'(A)')'  Red         : gravitational radiation AM loss'
     write(6,'(A)')'  Green       : Sills MB AM loss (was: wind AM loss)'
     write(6,'(A)')'  Dark blue   : Rappaport MB AML (was: SO coupling AML)'
     write(6,'(A)')'  Light blue  : Non-conservative MT AM loss'
     write(6,*)''
  end if
  
  if(tscls.eq.1) then
     write(6,*)''
     write(6,'(A)')' White        : Kelvin-Helmholz timescale'
     write(6,'(A)')' Red          : nuclear evolution timescale'
     write(6,'(A)')' Green        : mass loss timescale'
     write(6,'(A)')' Dark Blue    : gravitational radiation timescale'
     write(6,'(A)')' Light Blue   : dynamical timescale'
     write(6,*)''
  end if
  
  if(conv.eq.1) then
     ch = 0
     !write(6,'(A42,$)')' Do you want to plot hatches (Y)es/(N)o: '
     !read*,cnvh
     !if(cnvh.eq.'y'.or.cnvh.eq.'Y') ch = 1
  end if
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
     hlbl = 'n'
     hlp = 'n'
     write(6,'(A47,$)')' Do you want to highlight model points (y/n) ? '
     read*,hlp
     if(hlp.eq.'Y') hlp='y'
     if(hlp.eq.'N') hlp='n'

     if(hlp.eq.'y') then
        hlp1 = 'm'
        write(6,'(A,$)')' Do you want show (S)tructure models or type model numbers (M)anually?  (S/M) ? '
        read*,hlp1
        if(hlp1.eq.'S') hlp1='s'
        if(hlp1.eq.'M') hlp1='m'
        
        !Use saved structure models, store them in hp()
        if(hlp1.eq.'s') then
           write(6,'(/,A)')'      Nr    Line   Model'
           i = 0
           do j=1,n
              if(strmdls(j).eq.1) then
                 i = i+1
                 hp(i) = j
                 write(6,'(3I8)')i,hp(i),nint(dat(1,j))
              end if
           end do
           nhp = i
           write(6,'(I5,A)')nhp,' points selected.'
        end if
        
        !Enter points manually
        if(hlp1.eq.'m') then
           write(6,'(A67,I7,A17)')' Enter the number(s) of the model(s) that you want to highlight: (1-',nint(dat(1,n)),'), -1: end list: '
           nhp = 1
           do j=1,1000
              read*,hp(nhp)
              if(hp(nhp).eq.-1) goto 131
              if(hp(nhp).lt.1.or.hp(nhp).gt.nint(dat(1,n))) nhp = nhp-1
              nhp = nhp+1
           end do
           
           !Convert model number to line number
131        nhp = nhp-1
           write(6,'(I5,A)')nhp,' points selected:'
           write(6,'(A)')'      Nr   Model    Line'
           do i=1,nhp
              call locate(dat(1,1:n),n,dble(hp(i)),j)
              if(dabs(dat(1,j+1)-dble(hp(i))).lt.dabs(dat(1,j)-dble(hp(i)))) j = j+1
              if(j.gt.n) j = n
              write(6,'(3I8)')i,hp(i),j
              hp(i) = j
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
     do j=1,n
        if(strmdls(j).eq.1) then
           i = i+1
           hp(i) = j
           !write(6,'(3I8)')i,hp(i),nint(dat(1,j))
        end if
     end do
     nhp = i
     !write(6,'(I5,A)')nhp,' points selected.'
  end if
        
  
  
  
  
  
  
  
  
  
  
  !************************************************************************    
  !***   PLOT TO SCREEN OR FILE
  !************************************************************************      
  
501 continue
  
  if(hrd.eq.1.or.vx.eq.99.or.vx.eq.101) then
     x = min(xmin,xmax)
     xmin = max(xmin,xmax)
     xmax = x
  end if
  if((vy.eq.99.or.vy.eq.101) .and. plot.ne.9) then
     x = ymin
     ymin = ymax
     ymax = x
  end if
  
  if(plot.ne.0.and.plot.ne.7.and.plot.ne.9) then
     write(6,*)''     
     write(6,*)' X-range:',xmin,'-',xmax
     write(6,*)' Y-range:',ymin,'-',ymax
     write(6,*)''     
  end if
  
  
  if(plot.eq.9) then
     !call pgbegin(1,'plot_plt_000.eps/cps',1,1)
     io = pgopen('plot_plt_000.eps/cps')
     if(io.le.0) then
        write(0,'(A,/)')'  Error opening postscript file, aborting'
        stop
     end if
     call pgpap(10.,1.)
     call pgslw(5)
     sch = 1.5
  else
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
  
  
  call pgscf(1)
  !if(os.eq.2.or.plot.eq.9) call pgscf(2)
  call pgsch(sch)
  call pgsvp(0.06,0.95,0.07,0.96)
  if(plot.eq.9) call pgsvp(0.10,0.95,0.12,0.95)
  call pgswin(xmin,xmax,ymin,ymax)
  call pgbox('BCNTS',0.0,0,'BCNTS',0.0,0)
  if(plot.eq.7) then
     write(title1,'(A5,ES12.4,3(A,F6.2),A,F7.3)')'Age:',dat(2,n),' M:',dat(4,n),' Mhe:',dat(5,n),' Mco:',dat(6,n),' Porb:',dat(28,n)
     call pgmtxt('T',0.7,0.5,0.5,trim(title1))
  else
     if(plot.ne.9) call pgmtxt('T',0.7,0.5,0.5,'~/'//trim(title(13:99))//'/'//fname)
  end if
  call pgmtxt('B',2.4,0.5,0.5,trim(lx))
  call pgmtxt('L',2.0,0.5,0.5,trim(ly))
  
  call pgsci(2)
  do i=1,ny
     call pgsci(i+1)
     !if(ny.eq.1) call pgsci(2)
     yy1(1:n) = yy(i,1:n)
     if(drawlines.eq.0) call pgpoint(n,xx(1:n),yy1(1:n),1)
     if(drawlines.ge.1) call pgline(n,xx(1:n),yy1(1:n))
     if(drawlines.eq.2) call pgpoint(n,xx(1:n),yy1(1:n),20)
  end do
  call pgsci(1)
  
  !Highlight points
  call pgsch(1.5*sch)
  call pgsci(2)
  if(hlp.eq.'y') call pgpoint(nhp,xx(hp(1:nhp)),yy(1,hp(1:nhp)),2)
  if(hlbl.eq.'y') then
     call pgsch(0.7*sch)
     do i=1,nhp
        write(hlbls,'(I5)')nint(dat(1,hp(i)))
        call pgtext(xx(hp(i)),yy(1,hp(i)),hlbls)
     end do
     call pgsch(sch)
     call pgsci(1)
  end if !if(hlbl.eq.'y') then
  
  call pgsch(sch)
  if(lums.eq.1) then  !Luminosity legenda
     do j=1,6
        call pgsci(j)
        call pgmtext('RV',0.5,real(20-j)/20.,0.,lstr(j))
     end do !j
     call pgsci(1)
  end if !if(lums.eq.1)
  if(sabs+tabs+cabs.gt.0) then  !Abundances legenda
     do j=1,7
        call pgsci(j)
        call pgmtext('RV',0.5,real(20-j)/20.,0.,abstr(j))
     end do !j
     call pgsci(1)
  end if !if(sabs+tabs+cabs.gt.1)
  call pgsch(sch)
  
  
  
  
  
  if(conv.eq.1) then
     call pgsci(1)
     call pltconvection(nn,nvar,n,nl,dat,xx,yy,ymin,ymax,nhp,hp,hlp,hlbl)   !Convection plot
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
  
  if(vy.eq.87) then !Prot,critical for sat-MB, add Prot
     call pgsls(4)
     if(lgy.eq.0) call pgline(n,xx(1:n),real(dat(21,1:n)))
     if(lgy.eq.1) call pgline(n,xx(1:n),log10(real(dat(21,1:n))))
     call pgsls(1)
  end if
  
  if(vy.eq.88) then !Comp sat-MB with RVJ83-MB
     call pgsls(4)
     call pgline(n,xx(1:n),log10(real(dat(38,1:n))))
     call pgsls(1)
  end if
   
  call pgsci(1)
     
  if(plot.eq.9) then
     call pgend
     ex = .true.
     i = 1
     do while(ex)
        write(psname,'(A9,I3.3,A4)')'plot_plt_',i,'.eps'
        inquire(file=trim(psname), exist=ex) !Check whether the file already exists; ex is True or False
        if(.not.ex) j = system('mv -f plot_plt_000.eps '//trim(psname))
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
     do i=1,n
        do j=1,ny
           dist = (abs(xsel(1)-xx(i))/dx)**2 + (abs(ysel(1)-yy(j,i))/dy)**2
           if(dist.lt.mindist) then
              i0 = i
              j0 = j
              mindist = dist
           end if
        end do
     end do
     write(6,*)''
     write(6,'(A,ES12.4,A,ES12.4)')          ' Selected point:    x =',xsel(1),',  y =',ysel(1)
     write(6,'(A,ES12.4,A,ES12.4,A,I5,A,I6)')' Closest model:     x =',xx(i0),',  y =',yy(j0,i0),'    line =',i0+1,',  model =',nint(dat(1,i0))
     
     dx = 0
     dy = 0
     if(i0.gt.1.and.i0.lt.n) then
        dx = xx(i0+1)-xx(i0-1)
        dy = yy(j0,i0+1)-yy(j0,i0-1)
     else if(i0.gt.1) then
        dx = xx(i0)-xx(i0-1)
        dy = yy(j0,i0)-yy(j0,i0-1)
     else if(i0.lt.n) then
        dx = xx(i0+1)-xx(i0)
        dy = yy(j0,i0+1)-yy(j0,i0)
     end if
     
     write(6,'(3(A,ES12.4))')' Derivative:       dx =',dx,', dy =',dy,',  dy/dx =',dy/dx
     
     d = dat(:,i0)
     write(6,*)''
     !From listplt
     write(6,'(A)')' Line   Mdl     t (yr)   M(Mo)   Mhe   Mco   Menv    R (Ro)   L (Lo)    Te (K)   Tc (K)       V    B-V     Xc    Yc   Porb(d)     dM/dt  M2/Mo'
     write(6,'(I5,I6,ES11.4,F8.3,2F6.3,F7.3,2(1x,2ES9.2),1x,2F7.3,1x,2F6.3,2ES10.2,F7.3)')i0+1,nint(d(1)),d(2),d(4),d(5),d(6),d(63),d(8),d(9),d(10),d(11),d(101),d(103),d(56),d(57),d(28),dabs(d(31)),d(40)
     write(6,*)''
     
     call pgsci(2)
     call pgpoint(1,xx(i0),yy(j0,i0),2)
     write(hlbls,'(I5)')nint(dat(1,i0))
     call pgptxt(xx(i0),yy(j0,i0),0.,0.,hlbls)
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




