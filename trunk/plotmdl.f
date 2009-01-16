  ! Plots the data contained in a mdl* file
  ! Lines are longer than 72 chars, so add --wide (lf) or -132 (ifort) to compile
  ! Uses PGPLOT window 2 to plot to
  ! AF, 19-05-2005

program plotmdl  
  use constants
  implicit none
  integer, parameter :: nn=2001,nq=300
  integer :: nm,nc,nr,mdl,ny,nsel,pxnr(nq),pxin(nq)
  real :: dat(nq,nn),age,ver,x
  real :: xmin,xmax,ymin,ymax,xmin0,xmax0,ymin0,ymax0,system
  real :: xx(nn),yy(10,nn),xsel(4),ysel(4)
  real :: mm,rr,pp,rrh,tt,kk,nnad,nnrad,hh,hhe,ccc,nnn,oo,nne,mmg
  real :: ll,eeth,eenc,eenu,ss,uuint
  real :: m1,r1,l1,ts,tc,mhe,mco,rhoc
  real :: hc,hec,cc,oc,zc,hs,hes,cs,os,zs

  integer i,ii,j,blk,nblk,vx,vy,hmp,plot,ab,nab,ttlen
  character findfile*99,fname*99,rng,log,abds(7)*2,nabs(3)*3,mdlnr*5,bla*3
  character :: labels(nq)*60,lx*60,ly*60,title*100,pxns(0:nq)*99,psname*99
  logical :: ex

  plot = 0
  log = 'n'
  pxnr = 0
  pxin = 0
  do i=200,nq
     !pxnr(i) = i
     pxin(i) = i
  end do
  
  call setconstants()
  abds = (/'H ','He','C ','N ','O ','Ne','Mg'/)   !Line labels in abundances plot
  nabs = (/'ad ','rad','tru'/)   !Line labels in nablas plot

  !Names of the variables in px
  pxns(0) = ''
  pxns(1:10)  = (/'Psi','P','Rho','T','k','Nad','Ntrue','Nrad-Nad','M','H'/)
  pxns(11:20) = (/'He','C','N','O','Ne','Mg','R','L','Eth','Enuc'/)
  pxns(21:30) = (/'Enu','dM','...','Thom','Uhom','Vhom','Uint','S','L/Ledd','wxl'/)
  pxns(31:40) = (/'mu','wt?','Nel','NeO','w?','MI','phi','Fm','DGOS','...'/)
  pxns(41:50) = (/'...','LDRK','Enth','V^2','FAC','','','','','Rpp'/)
  pxns(51:60) = (/'Rpc','Rpng','Rpn','Rpo','Ran','','','','','N^2'/)

  pxns(201:210) = (/'Mesh pt','Nrad','m/M*','r/R*','C/O','Ne/O','Ugr-Uint','M.f.p.','n.dens','g'/)
  pxns(251:252) = (/'Abundances','Nablas'/)
  
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
  labels(208) = 'm.f.p. (cm)'
  labels(209) = 'n (cm\u-3\d)'
  labels(210) = 'g (cm s\u-2\d)'

  labels(251) = 'Abundances'
  labels(252) = "\(2266)'s"
  
  
  !Read currend path and use it as plot title
3 i = system('pwd > tmppwd.txt')
  open(unit=10,form='formatted',status='old',file='tmppwd.txt')
  rewind(10)
  read(10,'(a100)')title
  close(10)
  i = system('rm -f tmppwd.txt')
  do i=1,100
     if(title(i:i).ne.' ') ttlen = i
  enddo

  !Search for input file in current dir
  fname=findfile('*.mdl*',6)



  !***   READ ALL STRUCTURE MODELS IN THE FILE AND DISPLAY MAIN PROPERTIES

  write(6,*)''
4 write(6,'(A)')' Reading file '//trim(fname)
  open (unit=10,form='formatted',status='old',file=fname)
  rewind 10
  
  read(10,5,err=11,end=11) nm,nc,ver !Ver used to be overshoot parameter, now file version number (if>1)
  write(6,'(A,I4,A,I3,A)')' Reading',nm,' meshpoints,',nc,' columns of data.'
  if(ver.gt.1.) then
     read(10,*)bla 
     !read(10,'(60I4)')pxnr(1:nc)
  else
     pxnr(1:21)=(/9,17,2,3,4,5,6,8,10,11,12,13,14,15,16,18,19,20,21,28,27/)!,50,51,52,53,54,55,31,7,24,25,26,60
  end if
5 format (2x,I4,4x,I2,F7.3)
  write(6,*)''
  write(6,'(A)')'  Nr  Model Nmsh          Age        M1   Mhe   Mco     Menv         R        L     Teff       Tc     Rhoc      Xc     Yc     Cc     Oc     Xs    Ys    Zs'
  do ii=1,999
     if(mod(ii,25).eq.0) then
        write(6,*)''
        write(6,'(A)')'  Nr  Model Nmsh          Age        M1   Mhe   Mco     Menv         R        L     Teff       Tc     Rhoc      Xc     Yc     Cc     Oc     Xs    Ys    Zs'
     endif
     read(10,6,err=12,end=15) mdl,age
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
        endif
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
        endif
        if(mhe.eq.0.0.and.hh.gt.0.1) mhe = mm
        if(mco.eq.0.0.and.hhe.gt.0.1) mco = mm

     enddo !do j=1,nm

     write(6,9)ii,mdl,nm,age,m1,mhe,mco,m1-mhe,r1,l1,ts,tc,rhoc,hc,hec,cc,oc,hs,hes,zs!,bms,p,p1

7    format (1P,E13.6,4E11.4,16E11.3)

  enddo !ii



9 format (I4,I7,I5,ES13.5,f10.4,2f6.3,ES9.2,1x,4ES9.2,ES9.2,1x,4f7.4,1x,3f6.3)

11 write(6,'(A)')'  Error reading first line of file, aborting...'
  close(10)
  goto 9999
12 write(6,'(A)')'  Error reading first line of block, aborting...'
  close(10)
  goto 9999
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
  endif
  
  
  
  
  !***   CHOOSE STRUCTURE MODEL
20 write(6,'(A47,I3,A3,$)')' Which structure model do you want to plot (1-',nblk,'): '
  read*,blk
  if(blk.eq.0) goto 9999
  if(blk.lt.1.or.blk.gt.nblk) goto 20

  !Read file, upto chosen model (blk-1)
25 open (unit=10,form='formatted',status='old',file=fname)
  rewind 10
  read(10,5,err=11,end=11) nm,nc,ver !Ver used to be overshoot parameter, now file version number (if>1)
  if(ver.gt.1.) read(10,'(60I4)')pxnr(1:nc)
  do i=1,blk-1
     read(10,6,err=12,end=12) mdl,age
     do j=1,nm
        !read(10,7,err=13,end=30) (x, ii=1,21) 
        read(10,*,err=13,end=30) (x, ii=1,nc) 
     enddo !j
  enddo !i
  
  
  !***   READ CHOSEN STRUCTURE MODEL
  read(10,6,err=12,end=12) mdl,age
  do j=1,nm
     !read(10,7,err=13,end=30) (dat(i,j),i=1,21)
     read(10,*,err=13,end=30) (dat(i,j),i=1,nc)
  enddo
30 close(10)
  
  !Add model number to plot title
  write(mdlnr,'(I5)')mdl
  write(title,*)title(1:ttlen)//' '//mdlnr
  ttlen = ttlen + 8
  
  
  
  
  !***   COMPUTE ADDITIONAL PLOT VARIABLES
  if(plot.eq.0) then
     !Create inverse pxnr index, pxin:  if pxin(i) = 0, then the variable px(i) is not in the file
     do i=1,nc
        if(pxnr(i).gt.0) pxin(pxnr(i)) = i
     end do
     !do i=1,60
     !   print*,i,pxnr(i),pxin(i)
     !end do
     
     do i=1,nm
        dat(201,i) = real(i)
     enddo
     !dat(8,1:nm) = dat(8,1:nm)/abs(dat(8,1:nm)) !Difference between Nabla_rad and Nabla_ad, +1 or -1, +1: convection
     !dat(202,1:nm) = dat(7,1:nm) + dat(8,1:nm)
     !dat(203,1:nm) = dat(1,1:nm)/dat(1,nm)
     !dat(204,1:nm) = dat(2,1:nm)/dat(2,nm)
     !dat(205,1:nm) = dat(11,1:nm)/dat(13,1:nm)
     !dat(206,1:nm) = dat(14,1:nm)/dat(13,1:nm)
     !dat(207,1:nm) = g*dat(1,1:nm)*m0/(dat(2,1:nm)*r0)-dat(21,1:nm)
     !dat(208,1:nm) = 1.0/(dat(4,1:nm)*dat(6,1:nm))					   !Mean free path = 1/(rho * kappa)
     dat(202,1:nm) = dat(pxin(6),1:nm) + dat(pxin(8),1:nm)                      	   !Nabla_rad
     dat(8,1:nm)   = dat(pxin(8),1:nm)/abs(dat(pxin(8),1:nm))                              !Difference between Nabla_rad and Nabla_ad, +1 or -1, +1: convection, calculate after Nabla_rad
     dat(203,1:nm) = dat(pxin(9),1:nm)/dat(pxin(9),nm)                          	   !M/M*
     dat(204,1:nm) = dat(pxin(17),1:nm)/dat(pxin(17),nm)                        	   !R/R*
     dat(205,1:nm) = dat(pxin(12),1:nm)/dat(pxin(14),1:nm)                      	   !C/O
     dat(206,1:nm) = dat(pxin(13),1:nm)/dat(pxin(14),1:nm)                      	   !Ne/O
     dat(207,1:nm) = g*dat(pxin(9),1:nm)*m0/(dat(pxin(17),1:nm)*r0)-dat(pxin(27),1:nm)     !Ugr - Uint
     dat(208,1:nm) = 1.0/(dat(pxin(3),1:nm)*dat(pxin(5),1:nm))				   !Mean free path = 1/(rho * kappa)
     pxnr(201:208) = (/201,202,203,204,205,206,207,208/)
     if(pxin(31).ne.0) then
        dat(209,1:nm) = dat(pxin(2),1:nm)/(dat(pxin(31),1:nm)*amu)                !n = rho / (mu * amu)
        pxnr(209) = 209
     end if
     pxnr(251:252) = (/251,252/) !Abundances, Nablas
     if(pxin(60).ne.0) then !Brint-Vailasakatralala frequency
        !dat(pxin(60),1:nm) = 1./sqrt(max(dat(pxin(60),1:nm),1.e-10))/60. !Period in minutes
        !dat(pxin(60),1:nm) = max(dat(pxin(60),1:nm),1.e-10)
        !dat(pxin(60),1:nm) = max(-dat(pxin(60),1:nm),1.e-10)
        dat(pxin(60),1:nm) = abs(dat(pxin(60),1:nm))
     end if
     dat(210,1:nm) = real(g*dble(dat(pxin(9),1:nm))*m0/(dble(dat(pxin(17),1:nm))**2*r0**2))                !n = rho / (mu * amu)
     pxnr(210) = 210
     
  endif !if(plot.eq.0) then
  
  
  
  !***   CHOOSE PLOT VARIABLES
32 continue   
  write(6,*)''
  !write(6,'(A)')' Variables:                         0: Quit                   ' 
  !write(6,'(A)')'                                                              '
  !write(6,'(A)')'    1: M            9:  H          16: L                      '
  !write(6,'(A)')'    2: R            10: He         17: Eth                    '
  !write(6,'(A)')'    3: P            11: C          18: Enuc                   '
  !write(6,'(A)')'    4: Rho          12: N          19: Enu                    '
  !write(6,'(A)')'    5: T            13: O          20: S                      '
  !write(6,'(A)')'    6: k            14: Ne         21: Uint                   '
  !write(6,'(A)')'    7: Nad          15: Mg                                    '
  !write(6,'(A)')'    8: Nrad-Nad                                               '
  !print*,nc,mod(nc,10),nc-mod(nc,10),nc/nr
  
  
  nr = 4 !Number of variable columns
  ii = ceiling(real(nc)/real(nr)) !Number of rows
  write(6,'(A)')' Variables:                         0: Quit                   ' 
  do i=1,ii
     !write(6,'(9(I4,A10,5x))')(i+j*ii,': '//pxns(pxnr(i+j*ii)),j=0,nr-1)
     do j=0,nr-1
        if(pxnr(i+j*ii).eq.0) then
           write(6,'(A19,$)')''
        else
           write(6,'(I4,A10,5x,$)')i+j*ii,': '//pxns(pxnr(i+j*ii))
        end if
     end do
     write(6,*)''
  end do
  
  !Print derived variables, from number 201 on:
  write(6,'(A)')'                                                              '
  write(6,'(A)')'  Derived variables:                                          '
  nr = 3 !Number of variable columns
  j = 0
  do i=201,nq
     if(j.eq.nr) then
        write(6,*)''
        j = 0
     end if
     if(pxnr(i).gt.0) then
        write(6,'(I5,A15,5x,$)')i,': '//pxns(i)
        j = j+1
     end if
  end do
  write(6,*)''
  write(6,*)''
  
  !write(6,'(A)')'  201: Mesh pt     205: C/O                                   '
  !write(6,'(A)')'  202: Nrad        206: Ne/O                                  '
  !write(6,'(A)')'  203: M/M*        207: Ugr-Uint                              '
  !write(6,'(A)')'  204: R/R*        208: M.f.p.                                '
  !write(6,'(A)')'                                                              '
  !write(6,'(A)')'  251: Abundances                                             '
  !write(6,'(A)')'                                                              '
  
  
  
  
  
35 write(6,'(A,$)')' Choose the X-axis variable: '
  ab = 0
  read*,vx
  if(vx.eq.0) goto 9999
  !if(vx.ge.1.and.vx.le.nc) goto 36
  !if(vx.ge.201.and.vx.le.208) goto 36
  if(pxnr(vx).eq.0) goto 35
  !goto 35

36 write(6,'(A,$)')' Choose the Y-axis variable: '
  read*,vy
  if(vy.eq.0) goto 9999
  !if(vy.ge.1.and.vy.le.nc) goto 37
  !if(vy.ge.201.and.vy.le.208) goto 37
  !if(vy.eq.251) goto 37
  if(pxnr(vy).eq.0) goto 36
  !goto 36
  
  
37 continue 
  !if(vx.lt.200) vx = pxnr(vx)
  !if(vy.lt.200) vy = pxnr(vx)
  !print*,pxnr(vx),pxnr(vy)
  lx = labels(pxnr(vx))
  ly = labels(pxnr(vy))
  
  ny = 1
  if(vy.eq.251.or.ab.eq.1) then
     ab = 1
     vy = pxin(10)
     yy(1:7,1:nm) = dat(pxin(10):pxin(16),1:nm)
     ny = 7
  endif
  
  if(vy.eq.252.or.nab.eq.1) then
     nab = 1
     vy = pxin(6)
     yy(1,1:nm) = dat(pxin(6),1:nm)   !Nabla_ad
     yy(2,1:nm) = dat(pxin(202),1:nm) !Nabla_rad
     yy(3,1:nm) = dat(pxin(7),1:nm)   !True Nabla
     print*,pxin(6),pxin(7),pxin(202)
     ny = 3
  endif
  
  xx(1:nm) = dat(vx,1:nm)
  yy(1,1:nm) = dat(vy,1:nm)
  
  
  
  
  
  
  !***   LIN/LOG AXES
  
41 write(6,'(A,$)')' Do you want a logarithmic scale: (N)o, (X)-axis, (Y)-axis, (B)oth: '
  read*,log
  if(log.eq.'X') log='x'
  if(log.eq.'Y') log='y'
  if(log.eq.'B') log='b'
  if(log.eq.'N') log='n'
  
  if(log.eq.'x'.or.log.eq.'b') then
     if(xx(1).eq.0.) xx(1) = xx(2)
     xx(1:nm) = log10(abs(xx(1:nm))+1.e-20)
     !lx = 'log '//lx  !Use logarithmic axes rather than logarithmic variables
  endif
  if(log.eq.'y'.or.log.eq.'b') then
     do i=1,ny
        if(yy(i,1).eq.0.) yy(i,1) = yy(i,2)
     enddo
     yy(1:ny,1:nm) = log10(abs(yy(1:ny,1:nm))+1.e-20)
     !ly = 'log '//ly  !Use logarithmic axes rather than logarithmic variables
  endif
  
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
  write(6,'(A,$)')' Do you want to change a plot range ?  (N)o, (X)-axis, (Y)-axis, (B)oth:  '
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
     endif !if(xmin.gt.xmax)
     if(xmin.lt.xmin0) xmin = xmin0
     if(xmax.gt.xmax0) xmax = xmax0
  endif
  
  if(rng.eq.'y'.or.rng.eq.'b') then
     write(6,'(A)')' Give the new range for the Y-axis (Ymin, Ymax):'
     read*,ymin,ymax
     if(ymin.gt.ymax) then
        x = ymin
        ymin = ymax
        ymax = x
        write(6,'(A)')'  Swapped Ymin and Ymax'
     endif !if(ymin.gt.ymax)
     if(ymin.lt.ymin0) ymin = ymin0
     if(ymax.gt.ymax0) ymax = ymax0
  endif
  
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
111  write(6,'(A28,I4,A3,$)')' Highlight a mesh point (1 -',nm,'): '
     read*,hmp
     if(hmp.gt.nm) goto 111
     if(hmp.lt.1) hmp=0
  endif
  
  
  
  
  
  
  
  
  
  !***   PLOT TO SCREEN OR FILE
  
501 if(plot.eq.8) then
     call pgbegin(1,'plot_mdl_000.eps/cps',1,1)
     call pgscf(2)
  else
     call pgbegin(1,'2/xserve',1,1)
     call pgpap(scrsz,scrrat)
     call pgscf(1)
  endif
  
  call pgsvp(0.06,0.96,0.07,0.96)
  call pgswin(xmin,xmax,ymin,ymax)
  !call pgbox('BCNTS',0.0,0,'BCNTS',0.0,0)
  if(log.eq.'n') call pgbox('BCNTS',0.0,0,'BCNTS',0.0,0)  !Use logarithmic axes rather than logarithmic variables
  if(log.eq.'x') call pgbox('BCLNTS',0.0,0,'BCNTS',0.0,0)
  if(log.eq.'y') call pgbox('BCNTS',0.0,0,'BCLNTS',0.0,0)
  if(log.eq.'b') call pgbox('BCLNTS',0.0,0,'BCLNTS',0.0,0)
  call pgmtxt('T',0.7,0.5,0.5,title(14:ttlen))  !13 to remove /home/user/
  call pgmtxt('B',2.4,0.5,0.5,lx)
  call pgmtxt('L',2.0,0.5,0.5,ly)

  if(plot.eq.8) call pgslw(2)

  if(vx.ne.201.and.vy.ne.201) then
     do i=1,ny
        call pgsci(mod(i-1,6)+1)
        call pgline(nm,xx(1:nm),yy(i,1:nm))
        if(ab.eq.1) call pgmtext('RV',0.5,real(ny+1-i)/20.,0.,abds(i))
        if(nab.eq.1) call pgmtext('RV',0.5,real(ny+1-i)/20.,0.,nabs(i))
     enddo
  else
     do i=1,ny 
        call pgsci(mod(i-1,6)+1)
        call pgpoint(nm,xx(1:nm),yy(i,1:nm),1)
        if(ab.eq.1) call pgmtext('RV',0.5,real(ny+1-i)/20.,0.,abds(i))
     enddo
  endif

  call pgsch(1.5)
  call pgsci(8)
  if(hmp.ne.0) then
     do i=1,ny
        call pgpoint(1,xx(hmp),yy(i,hmp),2)
     enddo
  endif
  call pgsci(1)
  call pgsch(1.)
  call pgsls(2)
  if(vy.eq.21) call pgline(2,(/xmin,xmax/),(/0.,0./))

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
     !write(6,'(A)')' Plot saved to plot_mdl.eps'
  endif













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
  endif !if(plot.ne.9) then
  write(6,*)''
  write(6,'(A27,$)')' What do you want to do ?  '
  read*,plot
  if(plot.lt.0.or.plot.gt.8) goto 900

  if(plot.ne.4) call pgend
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
     endif
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
  endif

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
  endif

9999 write(6,'(A)')' Program finished'
  write(6,*)''
end program plotmdl



