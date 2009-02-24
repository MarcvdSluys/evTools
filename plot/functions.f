!functions.f: Shared modules, functions and subroutines for the Eggleton plot package
!For functions and routines that need pgplot, see plotfunctions.f

!************************************************************************
module ubvdata
  implicit none
  save
  integer, parameter :: nzgr=9,ntgr=61,nggr=11
  real*8 :: zgr(nzgr),tgr(ntgr),ggr(nggr),ubv(8,nggr,ntgr,nzgr)

  data zgr /-1.d0,-0.5d0,-0.3d0,-0.2d0,-0.1d0,0.d0,0.1d0,0.2d0,0.3d0/
  data ggr /0.d0,0.5d0,1.d0,1.5d0,2.d0,2.5d0,3.d0,3.5d0,4.d0,4.5d0,5.d0/
  data tgr /35.d0,37.5d0,40.d0,42.5d0,45.d0,47.5d0,50.d0,52.5d0,55.d0,57.5d0,60.d0,62.5d0,  &
       65.d0,67.5d0,70.d0,72.5d0,75.d0,77.5d0,80.d0,82.5d0,85.d0,87.5d0,90.d0,92.5d0,95.d0,97.5d0,  &
       100.d0,105.d0,110.d0,115.d0,120.d0,125.d0,130.d0,140.d0,150.d0,160.d0,170.d0,180.d0,190.d0,  &
       200.d0,210.d0,220.d0,230.d0,240.d0,250.d0,260.d0,270.d0,280.d0,290.d0,300.d0,  &
       310.d0,320.d0,330.d0,340.d0,350.d0,375.d0,400.d0,425.d0,450.d0,475.d0,500.d0/
end module ubvdata
!************************************************************************


!************************************************************************
module constants
  implicit none
  save
  real :: scrsz,scrrat
  real*8 :: pi,sigma,l0,r0,m0,g,c,day,yr,amu
  character :: homedir*99
  character :: cursorup*4,cursordown*4,cursorright*4,cursorleft*4 !Cursor movement
end module constants
!************************************************************************


!************************************************************************
subroutine setconstants
  use constants
  implicit none
  !scrsz=11.5  !Screen dimensions: MacBook, MacOS
  !scrrat=0.75
  scrsz=10.8  !Screen dimensions: MacBook, Gentoo
  scrrat=0.57
  pi	   =  4*datan(1.d0)
  sigma    =  5.67051d-5
  l0	   =  3.83d33
  r0	   =  6.9599d10
  m0	   =  1.9891d33
  g	   =  6.67259d-8
  c        =  2.99792458d10
  day      =  8.64d4
  yr	   =  3.15569d7
  amu      =  1.6605402d-24
  
  homedir = '/home/user'
  !homedir = '/Network/Servers/taku.astro.northwestern.edu/Users/ajl501'
  
  cursorup = char(27)//'[2A' !Print this to go up one line (on screen) (actually 2 lines, for some reason that's needed)
  cursordown = char(27)//'[1B' !Print this to go down one line (on screen)
  cursorright = char(27)//'[1C' !Makes the cursor move right one space
  cursorleft = char(27)//'[1D' !Makes the cursor move left one space
  
end subroutine setconstants
!************************************************************************








!************************************************************************      
subroutine lt2ubv(logl,logt,mass,logz,mv,uminb,bminv,vminr,rmini)
  !Computes values of Mv, U-B, B-V and V-I for given log L, log T, mass and log(Z/0.02)

  use ubvdata
  implicit none
  real*8, parameter :: gconst=-10.6071d0
  integer :: k,ig,it,iz,indx
  real*8 :: cm(5),ltgr(ntgr)
  real*8 :: logl,logt,mass,logz,mv,uminb,bminv,vminr,rmini
  real*8 :: logm,logg,dg1,dg2,dt1,dt2,dz1,dz2,mbol,bolc
  external indx

  logm = dlog10(mass)
  logg = logm + 4*logt - logl + gconst
  ltgr = dlog10(tgr*100)

  !Find indices of log Z, log g and log T to interpolate between.
  !don't allow extrapolation outside log Z and log g grid.
  ig = indx(logg,ggr,nggr)
  it = indx(logt,ltgr,ntgr)
  iz = indx(logz,zgr,nzgr)

  dg1 = (logg - ggr(ig-1))/(ggr(ig) - ggr(ig-1))
  dg1 = max(0.0d0, min(1.0d0, dg1))
  dg2 = 1.0d0 - dg1
  dt1 = (logt - ltgr(it-1))/(ltgr(it) - ltgr(it-1))
  dt2 = 1.0d0 - dt1
  dz1 = (logz - zgr(iz-1))/(zgr(iz) - zgr(iz-1))
  dz1 = max(0.0d0, min(1.0d0, dz1))
  dz2 = 1.0d0 - dz1

  do k = 4, 8
     cm(k-3) = ((ubv(k,ig,it,iz)*dg1 + ubv(k,ig-1,it,iz)*dg2)*dt1  &
          + (ubv(k,ig,it-1,iz)*dg1 +  &
          ubv(k,ig-1,it-1,iz)*dg2)*dt2)*dz1 +  &
          ((ubv(k,ig,it,iz-1)*dg1 +  &
          ubv(k,ig-1,it,iz-1)*dg2)*dt1 +  &
          (ubv(k,ig,it-1,iz-1)*dg1 +  &
          ubv(k,ig-1,it-1,iz-1)*dg2)*dt2)*dz2
  end do
  
  ! mbol = 4.75 - 2.5*logl
  mbol = 4.741 - 2.5*logl  !AF: 4.74 = -2.5*dlog10(l0) + 2.5*dlog10(4*pi*(10*pc)**2) - 11.49  !(Verbunt, p.36 -> cgs)
  bolc = cm(1)
  mv = mbol - bolc
  uminb = cm(2)
  bminv = cm(3)
  vminr = cm(4)
  rmini = cm(5)

  return
end subroutine lt2ubv
!************************************************************************      




!***********************************************************************
function getos() !Determine the operating system type: 1-Linux, 2-MacOSX
  use constants
  implicit none
  integer :: i,system,getos
  character :: ostype*25
  i=system('uname > '//trim(homedir)//'/uname.tmp') !This gives Linux or Darwin
  open(16,file=trim(homedir)//'/uname.tmp', status='old', form='formatted')
  read(16,'(A)')ostype
  close(16, status = 'delete')
  getos = 1 !Linux
  if(ostype(1:5).eq.'Darwi') getos = 2 !MacOSX
  return
end function getos
!***********************************************************************


!***********************************************************************
function findfile(match,len)
  use constants
  implicit none
  integer, parameter :: maxfile=1000
  integer :: i,k,len,fnum,system
  character :: match*99,names(maxfile)*99,findfile*99,fname*99,tempfile*99
  
  if(len_trim(homedir).eq.99) then
     write(0,'(/,A,/)')'  Findfile:  ERROR:  variable homedir not defined (forgot to call setconstants?), quitting.'
     stop
  end if
  
  tempfile = trim(homedir)//'/.findfile.tmp'
  i = system('ls '//match(1:len)//' > '//trim(tempfile))  !Shell command to list all the files with the search string and pipe them to a temporary file
  
  k=0
  names = ''
  open(10,file=trim(tempfile), status='old', form='formatted') !Read the temp file and delete it when closing
  rewind(10)
  do i=1,maxfile 
     read(10,'(A99)',end=100) names(i)
     k=k+1
  end do
  
100 close(10, status='delete')
  
  fname=names(1)
  
  fnum = 1
  if(k.gt.1) then
     write(6,'(A)')'  Files found:'
     do i=1,k
        write(6,'(I5,A)')i,':  '//trim(names(i))
     end do
     write(6,'(/,A,$)')'  Enter the number of the file you want to view: '
     read*,fnum
     if(fnum.le.0.or.fnum.gt.k) then
        write(*,'(/,A,/)')'  No file selected, quitting...'
        stop
     end if
     fname = names(fnum)
  end if
  
  if(k.eq.0.or.fnum.eq.0) then
     fname = ''
     if(k.eq.0) write(6,'(A)')'  No file found in this directory'
  end if
  
  findfile=fname
  
  return
end function findfile
!***********************************************************************



!***********************************************************************
subroutine findfiles(match,len,nff,all,fnames,nf)  
  !Input:
  !  match:   search string to match
  !  len:     length of match
  !  nff:     maximum number of files to return
  !  all:     0-select manually from list, 1-always return all files in list
  !Output:
  !  fnames:  array that contains the files found; make sure it has the same length as the array in the calling programme
  !  nf:      the actual number of files returned in fnames ( = min(number found, nff))
  
  use constants
  implicit none
  integer :: i,j,k,len,fnum,nf,nff,system,all
  character :: match*99,names(nff)*99,fnames(nff)*99,tempfile*99
  
  if(len_trim(homedir).eq.99) then
     write(0,'(/,A,/)')'  Findfiles:  ERROR:  variable homedir not defined (forgot to call setconstants?), quitting.'
     stop
  end if
  
  tempfile = trim(homedir)//'/.findfile.tmp'
  i = system('ls '//match(1:len)//' > '//trim(tempfile))  !Shell command to list all the files with the search string and pipe them to a temporary file
  
  do i=1,nff
     names(i)='                                                                                                   '
  end do
  
  k=0
  open(10,file=trim(tempfile), status='old', form='formatted') !Read the temp file and delete it when closing
  rewind(10)
  do i=1,nff
     read(10,'(A99)',end=100) names(i)
     k=k+1
  end do
100 continue
  close(10, status='delete')
  fnames(1) = names(1)
  nf = 1
  j = 0
  
  if(k.gt.1) then
     if(all.eq.0) then !Select files manually
        write(6,'(A)')'  Files found:'
        do i=1,k
           write(6,'(I5,A3,A)')i,':  ',trim(names(i))
        end do
        write(6,*)''
        write(6,'(A,I3)')'  Enter the number of the file you want to select: 1 -',k
        write(6,'(A,I3,A1)')'    (max',nff,')'
        write(6,'(A)')'      or:   0 - to select all files in the list'
        write(6,'(A)')'           -1 - when done'
        do j=1,nff
           read*,fnum
           if(fnum.lt.0) then
              nf = j-1
              goto 200
           end if
           if(fnum.eq.0) then
              nf = min(k,nff)
              fnames(1:nf) = names(1:nf)
              goto 200
           end if !if(fnum.eq.0)
           fnames(j) = names(fnum)
           nf = j
        end do !j 
     else  !Select all files (all=1)
        nf = min(k,nff)
        fnames(1:nf) = names(1:nf)
        goto 200
     end if
  end if
  
  if(k.eq.0) then
     fnames(1)='                                                                                                   '
     write(6,'(A)')'  No file found in this directory'
     nf = 0
  end if
  
200 continue
  return
end subroutine findfiles
!***********************************************************************










!***********************************************************************
!***  ROUTINES FOR *.PLT? FILES   **************************************
!***********************************************************************










!***********************************************************************
!Provides the labels for the plot axes of a *.plt? file
subroutine getpltlabels(nvar,labels)
  implicit none
  integer :: nvar
  character :: labels(nvar)*99
  
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
  labels(15) = 'U\dbind,env\u (10\u40\d erg)'
  labels(16) = 'L\dH\u (L\d\(2281)\u)'
  labels(17) = 'L\dHe\u (L\d\(2281)\u)'
  labels(18) = 'L\dC\u (L\d\(2281)\u)'
  labels(19) = 'L\d\gn\u (L\d\(2281)\u)'
  labels(20) = 'L\dth\u (L\d\(2281)\u)'
  labels(21) = 'P\drot\u (d)'
  labels(22) = 'K\u2\d'
  labels(23) = 'R\dcz\u'
  labels(24) = '\gDR\dcz\u'
  labels(25) = 't\det\u (d)'
  labels(26) = 'R\dalfven\u'
  labels(27) = 'B\dp\u'
  labels(28) = 'P\dorb\u (d)'
  labels(29) = 'FLR'
  labels(30) = 'F1'
  labels(31) = 'dM/dt (M\d\(2281)\u/yr)'
  labels(32) = 'dM\dwind\u/dt (M\d\(2281)\u/yr)'
  labels(33) = 'dM\dmt\u/dt (M\d\(2281)\u/yr)'
  labels(34) = 'H\dorb\u (10\u50\d g cm\u2\d s\u-1\d)'
  labels(35) = 'dH\dorb\u/dt'
  labels(36) = 'dH\dgw\u/dt'
  labels(37) = 'dH\dwml\u/dt'
  labels(38) = 'dH\ds-o\u/dt'
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
  
  labels(63) = 'M\denv\u (M\d\(2281)\u)'
  labels(64) = 'X\df\u'
  
  labels(75) = '\gt (yr)'
  labels(81) = 'Q\dconv\u'
  labels(82) = 'M\dHe\u-M\dCO\u (M\d\(2281)\u)'
  labels(83) = 'M\denv\u (M\d\(2281)\u)'
  labels(84) = 'M\dconv\u (M\d\(2281)\u)'
  labels(85) = 'R/(dR/dt) (yr)'
  labels(86) = 'Rossby number'
  labels(87) = 'P\drot,crit\u (d)'
  labels(88) = 'MB\dSills\u'
  labels(89) = 't\det,int\u/t\det,anal.\u'
  labels(90) = 't-t\d0\u (yr)'
  labels(91) = '(Ne/O)\dc\u/(Ne/O)\ds\u'
  labels(92) = 'P\dGW,max\u (d)'
  labels(93) = 'R\drl\u (R\d\(2281)\u)'
  labels(94) = 'X\df\u'
  labels(95) = 'M.I. (M\d\(2281)\u R\d\(2281)\u\u2\d)'
  labels(96) = 'J\dspin\u (10\u50\d g cm\u2\d s\u-1\d)'
  labels(97) = '\gr\davg\u (g cm\u-3\d)'
  labels(98) = 'Z\dsurf\u'
  labels(99) = '(t\df\u - t)  (yr)'
  labels(100) = 'P\drot\u/P\dcrit\u'
  
  labels(101) = 'V'
  labels(102) = 'U-B'
  labels(103) = 'B-V'
  labels(104) = 'V-R'
  labels(105) = 'R-I'
  labels(106) = 'U-V'
  labels(107) = 'V-I'
  
  labels(111) = '\(2137)\denv\u'  !lambda_env
  
  labels(202) = 'dH\dorb\u/dt'
  labels(204) = 'dM/dt (M\d\(2281)\u/yr)'
  labels(205) = '\gt (yr)'
  labels(206) = 'L (L\d\(2281)\u)'
  labels(207) = 'Surface abundances'
  labels(208) = 'T\dmax\u abundances'
  labels(209) = 'Core abundances'
  
end subroutine getpltlabels
!***********************************************************************


!***********************************************************************
!Print the list of variables in a *.plt? file to screen, for input menu
subroutine printpltvarlist
  implicit none
  
  write(6,*)''
  write(6,'(A)'),'  Primary variables:                                  0: Quit                           '
  write(6,'(A)'),'                                                                                        '
  write(6,'(A)'),'    1: model        16: Lh           28: Porb        34: Horb                           '
  write(6,'(A)'),'    2: t            17: Lhe          29: FLR         35: dHorb/dt                       '
  write(6,'(A)'),'    3: dt           18: Lc           30: F1          36: dHgw/dt                        '
  write(6,'(A)'),'    4: M            19: Lnu          31: dM          37: dHwml/dt                       '
  write(6,'(A)'),'    5: Mhe          20: Lth          32: dMwind      38: dHmb/dt                        '
  write(6,'(A)'),'    6: Mco          21: Prot         33: dMmt        39: dHmtr/dt                       '
  write(6,'(A)'),'    7: Mone         22: VK2                          40: Mcomp                          '
  write(6,'(A)'),'    8: R            23: Rcz                          41: e                              '
  write(6,'(A)'),'    9: L            24: dRcz                                                            '
  write(6,'(A)'),'   10: Teff         25: Tet                                                             '
  write(6,'(A)'),'   11: Tc           26: Ralv       Abundances:                                          '
  write(6,'(A)'),'   12: Tmax         27: Bp                 H  He   C   N   O  Ne  Mg   All              '
  write(6,'(A)'),'   13: Rhoc                        Surf:  42  43  44  45  46  47  48   207              '
  write(6,'(A)'),'   14: RhoTm                       Tmax:  49  50  51  52  53  54  55   208              '
  write(6,'(A)'),'   15: Ub,env                      Core:  56  57  58  59  60  61  62   209              '
  write(6,'(A)'),'                                                                                        ' 
  write(6,'(A)'),'  Derived variables:                                                                    '
  write(6,'(A)'),'   81: Qconv                91: Ne/O change     101: V        111: lambda_env           '  
  write(6,'(A)'),'   82: Mhe-Mco              92: Pgw,max         102: U-B                                '  
  write(6,'(A)'),'   83: Menv                 93: Rrl             103: B-V                                '
  write(6,'(A)'),'   84: Mconv                94: Xf              104: V-R                                '
  write(6,'(A)'),'   85: R/(dR/dt)            95: M.I.            105: R-I                                '
  write(6,'(A)'),'   86: Rossby nr            96: Jspin           106: U-V                                '
  write(6,'(A)'),'   87: Pcr (MB)             97: Rho_avg         107: V-I                                '
  write(6,'(A)'),'   88: Sills MB             98: Zsurf                                                   '
  write(6,'(A)'),'   89: Tet: int/anal        99: t_f-t                                                   '
  write(6,'(A)'),'   90: t-to                100: P_rot/crit                                              '
  write(6,'(A)'),'                                                                                        '
  write(6,'(A)'),'  Special plots:                                                                        '
  write(6,'(A)'),'   201: HR Diagram         204: Mdots           207: Surface abundances                 '
  write(6,'(A)'),"   202: dH/dt's            205: Timescales      208: Tmax abundances                    "
  write(6,'(A)'),'   203: Convection plot    206: Luminosities    209: Core abundances                    '
  write(6,'(A)'),'                                                                                        '
  write(6,'(A)'),'                                                                                        '
  
end subroutine printpltvarlist
!***********************************************************************



!***********************************************************************
!Read the *.plt? file fname from unit u and return it's length and the contents
subroutine readplt(u,fname,nn,nvar,nc,verbose,dat,n,ver)
  use constants
  implicit none
  real*8 :: dat(nvar,nn)
  integer :: nvar,nn,ncols,nc,nc1,verbose,i,j,n,ver,u
  character :: fname*99
  
  nc1 = nc !Get rid of 'unused' message
  
  !*** Old output format (2003)
  dat = 0.d0
  ver = 2003
  open(unit=u,form='formatted',status='old',file=trim(fname))
  rewind u
  read(u,*)ncols
  if(verbose.eq.1) write(6,'(A,I4,A)')'  Reading',ncols,' columns of data'
  !if(verbose.eq.1.and.ncols.ne.nc) write(6,'(A,I4)')'  WARNING: Number of colums in this file does not match that of the program: ',nc
  do j=1,nn
     read(u,10,err=12,end=11) (dat(i,j),i=1,ncols)
  end do
10   format(F6.0,E17.9,E14.6,11F9.5,7E12.4,3F9.5,16E12.4,F8.4,21E13.5,12F9.5,6F9.5,E14.6,E12.5) !Can read upto 82 columns
  write(6,'(A)')'  End of file reached, arrays too small!'
  close(u)
  goto 15
  
11 if(verbose.eq.1) write(6,'(A,I6,A)')'  End of the file reached,',j-1,' lines read.'
  close(u)
  goto 15
  
12 if(verbose.eq.1.or.j.ge.3) write(6,'(A,I6)')'  Error reading file, line',j
  close(u)
  if(j.lt.3) goto 19
  if(verbose.eq.1) write(6,'(A)')"  I'll skip the rest of the file and use the first part."
15 continue
  if(verbose.eq.1) write(6,*)''
  
  n = j-1   !Number of models in the file
  goto 29
  
  
  
  !*** New output format (2005)
19 continue
  !Erase the output from trying the first format
  if(verbose.eq.1) then
     do i=1,2
        write(6,'(A)')cursorup
        write(6,'(A150)')''
        write(6,'(A)')cursorup
     end do
     write(6,'(A)')'  I will try the new output format...'
  end if
  dat = 0.d0
  ver = 2005
  open (unit=u,form='formatted',status='old',file=trim(fname))
  rewind u
  read(u,*)ncols
  if(verbose.eq.1) write(6,'(A,I4,A)')'  Reading',ncols,' columns of data'
  !if(verbose.eq.1.and.ncols.ne.nc) write(6,'(A,I4)')'  WARNING: Number of colums in this file does not match that of the program: ',nc
  if(ncols.eq.81) then
     do j=1,nn
        read(u,'(F6.0,E17.9,E14.6,12E13.5,7E12.4,3E13.5,16E12.4,39E13.5,E14.6)',err=22,end=21) (dat(i,j),i=1,81)  !81 Columns
     end do
  end if
  if(ncols.gt.81) then
     do j=1,nn
        read(u,'(F6.0,E17.9,E14.6,12E13.5,7E12.4,3E13.5,16E12.4,39E13.5,E14.6,ES13.5,F5.1)',err=22,end=21) (dat(i,j),i=1,83)  !83 Columns, Evert(?) added 82, 83=strmdl flag
     end do
  end if
  write(6,'(A)')'  End of file reached, arrays too small!'
  close(u)
  goto 25
  
21 if(verbose.eq.1) write(6,'(A,I6,A)')'  End of the file reached,',j-1,' lines read.'
  close(u)
  goto 25
  
22 write(6,'(A,I6)')'  Error reading file, aborting at line',j
  if(j.lt.3) then
     write(6,'(/,A,/)')' Program finished.'
     stop
  end if
  write(6,'(A)')"  I'll skip the rest of the file and use the first part."
  close(u)
25 continue
  if(verbose.eq.1) write(6,*)''
  
  n = j-1   !Number of models in the file
  
29 continue
  
end subroutine readplt
!***********************************************************************



!***********************************************************************
!Change (e.g. de-log) and add plot variables for a *.plt? file
subroutine changepltvars(nn,nvar,n,dat,labels,dpdt)
  use constants
  implicit none
  integer :: nn,nvar,n,dpdt, i,j,j0,ib
  real*8 :: dat(nvar,nn),var(nn),dpdj(nn)
  real*8 :: c92(nn),c85a,c85b
  character :: labels(nvar)*99
  
  !de-log some variables
  do i=4,nvar
     if(dat(i,1).eq.0.) dat(i,1) = dat(i,2)  !In case you want to log them. Skip t,dt
  end do
  do i=36,39
     if(dat(i,1).le.0.) dat(i,1) = dat(i,2)
  end do
  
  do i=8,14  !De-log them
     dat(i,1:n) = 10.d0**dat(i,1:n)
  end do
  
  !'Clean' the convection data
  do j0 = 63,69,6
     do i=1,n
        ib = j0+5
        do j=j0+5,j0,-1
           if(dat(j,i).gt.0.d0) dat(j,i) = 0.d0  !Last number should be <=0, remove it if >0
           ib = j
           if(dat(j,i).lt.0.d0) exit
        end do !j
        do j=ib,j0+1,-2
           if(dabs((dat(j-1,i)+dat(j,i))/dat(4,i)).lt.1.d-4) dat(j-1:j,i) = (/0.d0,0.d0/)  !If upper and lower boundary are close enough, remove them
        end do !j
        do while(dabs(dat(j0,i))/dat(4,i).lt.1.d-4.and.sum(dabs(dat(j0:j0+5,i))).gt.1.e-7)  !Remove all the leading zeroes
           do j=j0,j0+4
              dat(j,i) = dat(j+1,i)
           end do
           dat(j0+5,i) = 0.d0
        end do
     end do !i
  end do !j0
  
  !************************************************************************      
  !***   CREATE EXTRA PLOT VARIABLES
  !************************************************************************      
  
  dat(5,1:n) = dat(5,1:n) + 1.d-30                              !Still necessary?
  dat(15,:) = dat(15,:)*m0*1.d-40                               !Ubind in 10^40 ergs
  do i=1,n
     dat(82,i) = dat(5,i)-dat(6,i)                              !Intershell mass
  end do
  dat(83,1:n) = dat(4,1:n) - dat(5,1:n)                         !H-envelope mass
  do i=1,n
     dat(84,i) = 0.d0
     if(dat(64,i).lt.0.d0) dat(84,i) = dabs(dat(64,i))          !Convective core boundary
  end do
  
  dat(85,1) = 0.d0
  do i=2,n
     c85a = dabs(dat(8,i)-dat(8,i-1))+1.d-30                    !dR
     c85b = dabs(dat(2,i)-dat(2,i-1))+1.d-30                    !dt
     dat(85,i) = dat(8,i)/(c85a/c85b)                           !R/(dR/dt)
  end do
  
  dat(86,1:n) = dat(21,1:n)/(dat(25,1:n)+1.d-30)                !Rossby number = Prot/Tet
  dat(87,1:n) = 2.5d0/13.8d0 * dat(25,1:n)                      !Critical Prot (Pc) for saturated MB (=2pi/omega_c): Pc_sun~2.5d, Tet_sun~13.8d
  
  do i=1,n                                                      !Saturated MB - Sills et al, 2000ApJ.534.335
     dat(88,i) = 2.7e-3*(2*pi/dat(21,i))*(2*pi/dat(87,i))**2* dat(8,i)**0.5d0*dat(4,i)**(-0.5d0)
     if(dat(21,i).gt.dat(87,i)) dat(88,i) = 2.7e-3* (2*pi/dat(21,i))**3*dat(8,i)**0.5d0*dat(4,i)**(-0.5d0)
     if(dat(81,i).lt.0.02)  dat(88,i) = dat(88,i)*exp(1.d0-2.d-2/dat(81,i)) !Exponential decrease for thin convective envelopes
     if(dat(81,i).lt.1.d-9)  dat(88,i) = 0.d0
     if(dat(81,i).gt.1.d0-1.d-9)  dat(88,i) = 0.d0  !No MB for fully convective star
  end do !i
  dat(88,1:n) = dat(88,1:n)/day**3
  
  
  dat(38,1:n)  = 3.8e-30*dat(4,1:n)*m0*(dat(8,1:n)*r0)**4* (2*pi/(dat(21,1:n)*day))**3/1.d50  !Calculate actual magnetic braking, according to Rappaport, Joss, Verbunt 1983
  do i=1,n
     if(dat(81,i).lt.0.02)  dat(38,i) = dat(38,i)*exp(1.d0-2.d-2/dat(81,i))
     if(dat(81,i).lt.1.d-9)  dat(38,i) = 0.d0
     if(dat(81,i).gt.1.d0-1.d-9)  dat(38,i) = 0.d0  !No MB for fully convective star
  end do !i
  
  dat(37,1:n) = dat(88,1:n)                                     !Take Sills MB in stead of Wind AML
  !dat(38,1:n) = dat(88,1:n)                                    !Take Sills in stead of Rappaport MB
  
  dpdj(1:n) = 3.d0/(dat(4,1:n)*dat(40,1:n)*m0*m0)*(2.d0*pi* (dat(28,1:n)*day)**2*(dat(4,1:n)+dat(40,1:n))*m0/(g*g)) **(1.d0/3.d0)                      !dP/dJ = 3/(m1m2)(2piP^2(m1+m2)/G^2)^1/3
  
  !Replace AML due to non-conservative MT by 'negative AML' due to MT
  !dat(39,1:n) = (dat(4,1:n)-dat(40,1:n))*m0*dat(31,1:n)*m0/yr* **(1.d0/3.d0)/1.d50                     !dJ/dt needed to obtain the same effect on Porb as from (conservative) mass transfer, in case of no wind: use dat(31) instead of dat(33)
  
  
  var(1:n) = (1.1487d0*dat(9,1:n)**0.47d0 +  0.1186d0*dat(9,1:n)**0.8d0)/dat(4,1:n)**0.31d0  !~Hyashi track radius
  dat(89,1:n) = 28.437d0*(dat(8,1:n)**2*dat(4,1:n)/ dat(9,1:n))**(1.d0/3.d0) * (dat(8,1:n)/var(1:n))**2.7  !Analytic convective turnover timescale (days), adapted from Eggleton (CFUNCS.F)
  dat(89,1:n) = dat(25,1:n)/dat(89,1:n)  !Actual Tet / analitic Tet
  
  dat(90,1:n) = dat(2,1:n) - dat(2,1) !t-t0
  dat(91,1:n) = (dat(61,1:n)/dat(60,1:n))/(dat(47,1:n)/dat(46,1:n))   !(Ne/O)cen/(Ne/O)surf
  
  c92(1:n)    = 2*pi*(256.d0/5.d0)**(3.d0/8.d0)* g**(5.d0/8.d0)/c**(15.d0/8.d0) *(dat(5,1:n)*1.4)**(3.d0/8.d0)*m0**(5.d0/8.d0)/ (dat(5,1:n)+1.4)**(1.d0/8.d0)
  dat(92,1:n) = ((13.6d9-dat(2,1:n))*yr)**(3.d0/8.d0)*c92(1:n)/day  !Pmax that can still be converged for a WD with the mass of the He core and a NS of 1.4Mo in a time t-t_H due to GWs
  
  dat(93,1:n) = dat(8,1:n)/dexp(dat(29,1:n))     
  dat(94,1:n) = 2*dat(56,1:n) + dat(57,1:n) + 1.                            !Xf := 2Xc + Yc + 1
  dat(95,1:n) = 10.d0**dat(22,1:n)*dat(4,1:n)*dat(8,1:n)**2                 !M.I. = k^2*M*R^2 in MoRo^2  (in some models, log(VK2) is listed
  dat(96,1:n) = dat(95,1:n)*2*pi/(dat(21,1:n)+1.e-30)*(1.d-50*m0*r0*r0/day) !Jspin = I*w in 10^50 g cm^2 s^-1
  dat(97,1:n) = dat(4,1:n)*m0/(4/3.d0*pi*(dat(8,1:n)*r0)**3)                !Average Rho
  dat(98,1:n) = 1.d0 - dat(42,1:n)-dat(43,1:n)                              !Z_surf = 1 - X - Y
  dat(99,1:n) = dat(2,n) - min(dat(2,1:n), dat(2,n)-1.d4)                          !t - t_final, avoid by setting dat(,1) = dat(,2)
  
  !dat(100,1:n) = sqrt(2*g*dat(4,1:n)*m0/(dat(8,1:n)*r0)**3)/day             !Critical (Keplerian) omega
  dat(100,1:n) = 2*pi*sqrt((dat(8,1:n)*r0)**3/(g*dat(4,1:n)*m0))/day         !Critical (Keplerian) rotation period
  dat(100,1:n) = dat(21,1:n)/(dat(100,1:n)+1.e-30)                           !Prot/Pcrit
  
  !Colours
  do i=1,n
     call lt2ubv(dlog10(dat(9,i)),dlog10(dat(10,i)),dat(4,i),dlog10(dat(98,i)/2.d-2),dat(101,i),dat(102,i), dat(103,i),dat(104,i),dat(105,i))
     dat(106,i) = dat(102,i)+dat(103,i)                                     !(U-V) = (U-B) + (B-V)
     dat(107,i) = dat(104,i)+dat(105,i)                                     !(V-I) = (V-R) + (R-I)
  end do
  
  dat(111,1:n) = g*dat(4,1:n)*dat(83,1:n)*m0**2 / (dat(15,1:n)*dat(8,1:n)*r0*1.d40+1.d-30)  !lambda_env = G*M*M_env/(Ubind*R)
  !dat(111,1:n) = dabs(dat(111,1:n))    !This 'hides' the fact that Ubind changes sign
  dat(111,1:n) = max(dat(111,1:n),0.d0)
  do i=1,n
     if(dabs(dat(5,i)).lt.1.d-29) dat(111,i) = 0.d0  !If there's no He core mass, there's no lambda
     !write(*,'(I6,9ES20.5)')i,dat(4:5,i),dat(83,i),dat(15,i),dat(8,i),dat(111,i)
  end do
  
  !Timescales
  dat(201,1:n) = g*dat(4,1:n)**2*m0*m0 / (dat(8,1:n)*r0*dat(9,1:n)*l0)/yr   !KH timescale
  dat(202,1:n) = dat(4,1:n)*m0/1.9891/(dat(9,1:n)*l0)*4.d10                 !Nuclear evolution timescale
  dat(203,1:n) = dat(4,1:n)/max(abs(dat(33,1:n)),1.e-30)                    !Mass transfer
  dat(204,1:n) = dat(34,1:n)/max(dat(36,1:n)*yr,1.e-30)                     !Gravitational waves
  !dat(205,1:n) = dat(34,1:n)/max(abs(dat(38,1:n))*yr,1.e-30)               !Magnetic braking (Actually SO-coupling!)
  dat(205,1:n) = dsqrt(dat(8,1:n)**3/(g*dat(4,1:n)))                        !Dynamical: t ~ sqrt(R^3/(G*M))
  dpdt  = 0
  
  !Replace dH/dt by dP/dt
  dpdj(1:n) = 3.d0/(dat(4,1:n)*dat(40,1:n)*m0*m0)*(2.d0*pi* (dat(28,1:n)*day)**2*(dat(4,1:n)+dat(40,1:n))*m0/(g*g)) **(1.d0/3.d0)                      !dP/dJ = 3/(m1m2)(2piP^2(m1+m2)/G^2)^1/3
  do i=35,39
     dat(i,1:n) = dat(i,1:n)*dpdj(1:n)*1.d50+1.d-30
  end do
  labels(35) = 'dP\dorb\u/dt'
  labels(202) = 'dP\dorb\u/dt'
  dpdt = 1
  
  !Replace dP/dt by timescales 
  do i=35,39
     dat(i,1:n) = dat(28,1:n)*day/dat(i,1:n)/yr
  end do
  labels(35) = '\gt\dP\dorb\u\u (yr)'
  labels(202) = '\gt\dP\dorb\u\u (yr)'
  dpdt = 2
  
end subroutine changepltvars
!***********************************************************************


!***********************************************************************
subroutine rswap(x,y) !Swap two real numbers
  implicit none
  real :: x,y,z
  z = x
  x = y
  y = z
end subroutine rswap
!***********************************************************************





!************************************************************************      
function indx(ax,xx,nx)  !Double precision
  !Finds index of ax in monotonously increasing or decreasing array xx
  implicit none
  integer :: indx,nx,j,jl,jh
  real*8 :: ax,xx(nx),sx
  
  sx = xx(nx) - xx(1)
  jl = 1
  jh = nx
1 if (jh-jl.gt.1) then
     j = (jh + jl)/2
     if ((ax-xx(j))*sx .gt. 0.0) then
        jl = j
     else
        jh = j
     end if
     goto 1
  end if
  indx = jh
  
  return
end function indx
!************************************************************************      




!***********************************************************************
subroutine locate(xx,n,x,j)  !Double precision
  !Input: 
  !  xx: monotonic array
  !  n:  length of xx
  !  x:  value to look for
  !  Output:
  !  j:  returned value, such that x is between xx(j) and xx(j+1).  If j=0 or jn, x is out of range
  
  implicit none
  integer :: j,n,jl,jm,ju
  real*8 :: x,xx(n)
  jl=0
  ju=n+1
10 if(ju-jl.gt.1)then
     jm=(ju+jl)/2
     if((xx(n).ge.xx(1)).eqv.(x.ge.xx(jm)))then
        jl=jm
     else
        ju=jm
     end if
     goto 10
  end if
  if(x.eq.xx(1))then
     j=1
  else if(x.eq.xx(n))then
     j=n-1
  else
     j=jl
  end if
end subroutine locate
!***********************************************************************


!***********************************************************************
subroutine locater(xxr,n,xr,j)  !Single precision
  !Input: 
  !  xx: monotonic array
  !  n:  length of xx
  !  x:  value to look for
  !  Output:
  !  j:  returned value, such that x is between xx(j) and xx(j+1).  If j=0 or jn, x is out of range
  
  implicit none
  integer :: j,n
  real :: xr,xxr(n)
  real*8 :: xd,xxd(n)
  xxd = dble(xxr)
  xd  = dble(xr)
  call locate(xxd,n,xd,j)  !j will be returned to the calling routine
end subroutine locater
!************************************************************************      


!***********************************************************************
subroutine polint(xa,ya,n,x,y,dy)
  implicit none
  integer, parameter :: nmax=10
  integer :: n
  real :: dy,x,y,xa(n),ya(n)
  integer :: i,m,ns
  real :: den,dif,dift,ho,hp,w,c(nmax),d(nmax)
  
  ns=1
  dif=abs(x-xa(1))
  do i=1,n
     dift=abs(x-xa(i))
     if (dift.lt.dif) then
        ns=i
        dif=dift
     end if
     c(i)=ya(i)
     d(i)=ya(i)
  end do
  y=ya(ns)
  ns=ns-1
  do m=1,n-1
     do i=1,n-m
        ho=xa(i)-x
        hp=xa(i+m)-x
        w=c(i+1)-d(i)
        den=ho-hp
        !          if(den.eq.0.) pause 'failure in polint'
        den=w/den
        d(i)=hp*den
        c(i)=ho*den
     end do
     if (2*ns.lt.n-m)then
        dy=c(ns+1)
     else
        dy=d(ns)
        ns=ns-1
     end if
     y=y+dy
  end do
  return
end subroutine polint
!***********************************************************************




!***********************************************************************
function ran1(idum)
  implicit none
  integer :: idum,IA,IM,IQ,IR,NTAB,NDIV
  real :: ran1,AM,EPS,RNMX
  PARAMETER (IA=16807,IM=2147483647,AM=1./IM,IQ=127773,IR=2836,NTAB=32,NDIV=1+(IM-1)/NTAB,EPS=1.2e-7,RNMX=1.-EPS)
  INTEGER j,k,iv(NTAB),iy
  SAVE iv,iy
  DATA iv /NTAB*0/, iy /0/
  
  if (idum.le.0.or.iy.eq.0) then
     idum=max(-idum,1)
     do j=NTAB+8,1,-1
        k=idum/IQ
        idum=IA*(idum-k*IQ)-IR*k
        if (idum.lt.0) idum=idum+IM
        if (j.le.NTAB) iv(j)=idum
     end do
     iy=iv(1)
  end if
  k=idum/IQ
  idum=IA*(idum-k*IQ)-IR*k
  if (idum.lt.0) idum=idum+IM
  j=1+iy/NDIV
  iy=iv(j)
  iv(j)=idum
  ran1=min(AM*iy,RNMX)
  return
end function ran1
!***********************************************************************










