!> plotmdl.f90  Plots the data contained in a mdl* file

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
  
  implicit none
  integer, parameter :: nn=2001,nq=300  !nq: max number of columns
  integer :: nm,nc,nv_der,nr,mdl,ny,nsel,pxnr(nq),pxin(nq),io,system
  real(double) :: dat1(nq)
  real :: dat(nq,nn),age,ver,x
  real :: xmin,xmax,ymin,ymax,xmin0,xmax0,ymin0,ymax0
  real :: xx(nn),yy(10,nn),yy1(nn),xsel(4),ysel(4),x2(2),y2(2)
  real :: mm,rr,pp,rrh,tt,kk,nnad,nnrad,hh,hhe,ccc,nnn,oo,nne,mmg
  real :: ll,eeth,eenc,eenu,ss,uuint
  real :: m1,r1,l1,ts,tc,mhe,mco,rhoc
  real :: hc,hec,cc,oc,zc,hs,hes,cs,os,zs
  
  integer i,ii,j,blk,nblk,vx,vy,hmp,plot,ab,nab
  character findfile*99,fname*99,rng,log,abds(7)*2,nabs(3)*3,bla*3
  character :: labels(nq)*60,lx*60,ly*60,title*100,pxns(0:nq)*99,psname*99
  logical :: ex
  
  !Set constants:
  call setconstants()
  write(6,*)
  call print_code_version(6)  !To screen
  
  call eggletonplot_settings()
  
  plot = 0
  log = 'n'
  pxnr = 0
  pxin = 0
  do i=200,nq
     pxin(i) = i
  end do
  
  abds = [character(len=2) :: 'H ','He','C ','N ','O ','Ne','Mg']   !Line labels in abundances plot
  nabs = [character(len=3) :: 'ad ','rad','tru']   !Line labels in nablas plot
  nab = 0
  
  !Names of the variables in px
  pxns(0) = ''
  pxns(1:10)  = [character(len=99) :: 'Psi','P','Rho','T','k','Nad','Ntrue','Nrad-Nad','M','H']
  pxns(11:20) = [character(len=99) :: 'He','C','N','O','Ne','Mg','R','L','Eth','Enuc']
  pxns(21:30) = [character(len=99) :: 'Enu','dM','...','Thom','Uhom','Vhom','Uint','S','L/Ledd','wxl']
  pxns(31:40) = [character(len=99) :: 'mu','wt?','Nel','NeO','w?','MI','phi','Fm','DGOS','...']
  pxns(41:50) = [character(len=99) :: '...','LDRK','Enth','V^2','FAC','','','','','Rpp']
  pxns(51:60) = [character(len=99) :: 'Rpc','Rpng','Rpn','Rpo','Ran','','','','','N^2']
  
  pxns(201:210) = [character(len=99) :: 'Mesh pt','Nrad','m/M*','r/R*','C/O','Ne/O','Ugr-Uint','M.f.p.','n.dens','g']
  pxns(211:218) = [character(len=99) :: 'mu','n','Prad','Pgas','Pr/Pg','Ub,*','Ub,env','P/rho']
  pxns(251:252) = [character(len=99) :: 'Abundances','Nablas']
  
  !Axis labels
  labels = ''
  labels(1) = 'M (M\d\(2281)\u)'
  labels(2) = 'R (R\d\(2281)\u)'
  labels(3) = 'P (dyn cm\u-2\d)'
  labels(4) = '\gr (g cm\u-3\d)'
  labels(5) = 'T (K)'
  labels(6) = 'k (cm\u2\d g\u-1\d)'
  labels(7) = '\(2266)\dad\u'
  labels(8) = '\(2266)\drad\u - \(2266)\dad\u:  1 = convection'
  labels(9) = 'H abundance'
  labels(10) = 'He abundance'
  labels(11) = 'C abundance'
  labels(12) = 'N abundance'
  labels(13) = 'O abundance'
  labels(14) = 'Ne abundance'
  labels(15) = 'Mg abundance'
  labels(16) = 'L (L\d\(2281)\u)'
  labels(17) = '\ge\dth\u'
  labels(18) = '\ge\dnucl\u'
  labels(19) = '\ge\d\gn\u'
  labels(20) = 'S (cgs)'
  labels(21) = 'U\dint\u (erg g\u-1\d)'
  
  !Axis labels, px numbers
  labels = ''
  labels(1)  = '\gq' !Psi
  labels(2)  = 'P (dyn cm\u-2\d)'
  labels(3)  = '\gr (g cm\u-3\d)'
  labels(4)  = 'T (K)'
  labels(5)  = '\gk (cm\u2\d g\u-1\d)'
  labels(6)  = '\(2266)\dad\u'
  labels(7)  = '\(2266)\dtrue\u'
  labels(8)  = '\(2266)\drad\u - \(2266)\dad\u:  1 = convection'
  labels(9)  = 'M (M\d\(2281)\u)'
  labels(10) = 'H abundance'
  labels(11) = 'He abundance'
  labels(12) = 'C abundance'
  labels(13) = 'N abundance'
  labels(14) = 'O abundance'
  labels(15) = 'Ne abundance'
  labels(16) = 'Mg abundance'
  labels(17) = 'R (R\d\(2281)\u)'
  labels(18) = 'L (L\d\(2281)\u)'
  labels(19) = '\ge\dth\u'
  labels(20) = '\ge\dnucl\u'
  labels(21) = '\ge\d\gn\u'
  labels(22) = 'dM (M\d\(2281)\u)'
  labels(24) = 'T\dhom\u'
  labels(25) = 'U\dhom\u'
  labels(26) = 'V\dhom\u'
  labels(27) = 'U\dint\u (erg g\u-1\d)'
  labels(28) = 'S (cgs)'
  labels(29) = 'L/L\dedd\u'
  labels(30) = 'wxl'
  labels(31) = '\gm'
  labels(32) = 'wt?'
  labels(33) = 'Nel'
  labels(34) = 'NelO'
  labels(35) = 'v\dconv\u?'
  labels(36) = 'M.I.'
  labels(37) = '\gf' !Phi
  labels(38) = 'F\dm\u'
  labels(39) = 'DGOS'
  labels(40) = 'DLRK'   !Variables from here on were different or non-existent in the 2003 version of the code
  labels(41) = '\gD(enth)'
  labels(42) = 'XIK'
  labels(43) = 'V\u2\d'
  labels(44) = 'FAC2'
  labels(45) = 'FAC1'
  labels(50) = 'R\dpp\u'
  labels(51) = 'R\dpC\u'
  labels(52) = 'R\dpNG\u'
  labels(53) = 'R\dpN\u'
  labels(54) = 'R\dpO\u'
  labels(55) = 'R\dAN\u'
  labels(60) = 'N\u2\d'
  
  
  labels(201) = '\u\(2263) centre\d    Mesh point    \usurface \(2261)'
  labels(202) = '\(2266)\drad\u'
  labels(203) = 'm/M\d*\u'
  labels(204) = 'r/R\d*\u'
  labels(205) = 'C/O'
  labels(206) = 'Ne/O'
  labels(207) = 'U\dgr\u - U\dint\u'
  labels(208) = 'mean free path (cm)'
  labels(209) = 'n (cm\u-3\d)'
  labels(210) = 'g (cm s\u-2\d)'
  labels(211) = '\(2138)'  !\mu - mean molecular weight
  labels(212) = 'n (cm\u-3\d)'
  labels(213) = 'P\drad\u (dyn cm\u-2\d)'
  labels(214) = 'P\dgas\u (dyn cm\u-2\d)'
  labels(215) = '\(2128) = P\drad\u/P\dgas\u'  !\beta - Prad/Pgas
  labels(216) = 'U\db,*\u ()'    !Binding energy of the star
  labels(217) = 'U\db,env\u ()'  !Binding energy of the envelope
  labels(218) = 'P/\(2143) (cgs)'  !P/rho
  
  nv_der = 18  !Number of derived variables
  
  labels(251) = 'Abundances'
  labels(252) = "\(2266)'s"
  
  
  !Read currend path and use it as plot title
3 i = system('pwd > tmppwd.txt')
  open(unit=10,form='formatted',status='old',file='tmppwd.txt')
  rewind(10)
  read(10,'(a100)')title
  close(10)
  i = system('rm -f tmppwd.txt')
  
  if(command_argument_count().eq.1) then
     call get_command_argument(1,fname)
  else
     fname=findfile('*.mdl*')  !Search for input file in current dir
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
  if(io.ne.0) then
     write(6,'(A,/)')'  Error reading first line of file, aborting...'
     close(10)
     stop
  end if
  
  write(6,'(A,I4,A,I3,A)')' Reading',nm,' meshpoints,',nc,' columns of data.'
  if(ver.gt.1.) then
     read(10,*)bla 
  else
     pxnr(1:21)=(/9,17,2,3,4,5,6,8,10,11,12,13,14,15,16,18,19,20,21,28,27/)!,50,51,52,53,54,55,31,7,24,25,26,60
  end if
5 format (2x,I4,4x,I2,F7.3)
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
  if(ver.gt.1.) read(10,'(60I4)')pxnr(1:nc)
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
  
  !Add model number to plot title
  write(title,'(A,I6)')trim(title),mdl
  
  
  
  
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
     dat(207,1:nm) = g*dat(pxin(9),1:nm)*m0/(dat(pxin(17),1:nm)*r0) - dat(pxin(27),1:nm)   !Ugr - Uint
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
     dat(216,1) = 0.
     dat(217,1) = 0.
     do i=2,nm
        dat(216,i) = dat(216,i-1) + dat(207,i)                                             !BE of whole star
        if(dat(pxin(10),i).lt.0.1) dat(217,i) = dat(217,i-1) + dat(207,i)                  !Start at core-envelope boundary
     end do
     
     dat(218,1:nm) = dat(3,1:nm)/dat(4,1:nm)                                               !P/rho
     
     pxnr(211:218) = (/211,212,213,214,215,216,217,218/)
     
     
     if(pxin(60).ne.0) then !Brint-Vailasakatralala frequency
        dat(pxin(60),1:nm) = abs(dat(pxin(60),1:nm))
     end if
     
     pxnr(251:252) = (/251,252/) !Abundances, Nablas
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
  !nr = 3 !Number of variable columns
  !j = 0
  !do i=201,nq
  !   if(j.eq.nr) then
  !      write(6,*)''
  !      j = 0
  !   end if
  !   if(pxnr(i).gt.0) then
  !      write(6,'(I5,A15,5x)',advance='no')i,': '//pxns(i)
  !      j = j+1
  !   end if
  !end do
  
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
  
  write(6,*)''
  write(6,*)''
  
  
  
  
35 write(6,'(A)',advance='no')' Choose the X-axis variable: '
  ab = 0
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
  
  ny = 1
  if(vy.eq.251.or.ab.eq.1) then
     ab = 1
     vy = pxin(10)
     yy(1:7,1:nm) = dat(pxin(10):pxin(16),1:nm)
     ny = 7
  end if
  
  if(vy.eq.252.or.nab.eq.1) then
     nab = 1
     vy = pxin(6)
     yy(1,1:nm) = dat(pxin(6),1:nm)   !Nabla_ad
     yy(2,1:nm) = dat(pxin(202),1:nm) !Nabla_rad
     yy(3,1:nm) = dat(pxin(7),1:nm)   !True Nabla
     print*,pxin(6),pxin(7),pxin(202)
     ny = 3
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
  
  if(ab.eq.1.and.(log.eq.'y'.or.log.eq.'b').and.ymin.lt.-6.) ymin = -6.
  
  
  
  
  
  
  
  
  
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
  
501 if(plot.eq.8) then
     call pgbegin(1,'plot_mdl_000.eps/cps',1,1)
     call pgscf(2)
  else
     call pgbegin(1,'2/xserve',1,1)
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
  
  call pgsvp(0.06,0.96,0.07,0.96)
  call pgswin(xmin,xmax,ymin,ymax)
  if(log.eq.'n') call pgbox('BCNTS',0.0,0,'BCNTS',0.0,0)  !Use logarithmic axes rather than logarithmic variables
  if(log.eq.'x') call pgbox('BCLNTS',0.0,0,'BCNTS',0.0,0)
  if(log.eq.'y') call pgbox('BCNTS',0.0,0,'BCLNTS',0.0,0)
  if(log.eq.'b') call pgbox('BCLNTS',0.0,0,'BCLNTS',0.0,0)
  call pgmtxt('T',0.7,0.5,0.5,trim(title(14:)))  !13 to remove /home/user/
  call pgmtxt('B',2.4,0.5,0.5,lx)
  call pgmtxt('L',2.0,0.5,0.5,ly)
  
  if(plot.eq.8) call pgslw(2)
  
  if(vx.ne.201.and.vy.ne.201) then
     do i=1,ny
        call pgsci(colours(mod(i-1,ncolours)+1))
        yy1(1:nm) = yy(i,1:nm)
        call pgline(nm,xx(1:nm),yy1(1:nm))
        if(ab.eq.1) call pgmtext('RV',0.5,real(ny+1-i)/20.,0.,abds(i))
        if(nab.eq.1) call pgmtext('RV',0.5,real(ny+1-i)/20.,0.,nabs(i))
     end do
  else
     do i=1,ny 
        call pgsci(mod(i-1,6)+1)
        call pgpoint(nm,xx(1:nm),yy(i,1:nm),1)
        if(ab.eq.1) call pgmtext('RV',0.5,real(ny+1-i)/20.,0.,abds(i))
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
     ex = .true.
     i = 1
     do while(ex)
        write(psname,'(A9,I3.3,A4)')'plot_mdl_',i,'.eps'
        inquire(file=trim(psname), exist=ex) !Check whether the file already exists; ex is True or False
        if(.not.ex) j = system('mv -f plot_mdl_000.eps '//trim(psname))
        i = i+1
     end do
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
     !call identify_closest_model(nq,nn,ny,dat,xx,yy,xmin,xmax,ymin,ymax)
     call identify_closest_model(nn,ny,xx,yy,xmin,xmax,ymin,ymax)
     goto 900
  end if
  
9999 continue
  write(6,'(A,/)')' Program finished'
end program plotmdl
!***********************************************************************************************************************************




!***********************************************************************************************************************************
!subroutine identify_closest_model(nq,nn,ny,dat,xx,yy,xmin,xmax,ymin,ymax)
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
