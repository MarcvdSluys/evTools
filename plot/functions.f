!functions.f: Shared modules, functions and subroutines for the Eggleton plot package
!For functions and routines that need pgplot, see plotfunctions.f
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
  real*8 :: pi,tpi,pi2,c3rd
  real*8 :: l0,r0,m0,g,c,day,yr,km
  real*8 :: amu,m_h,k_b,h_p,h_bar,a_rad,sigma
  character :: homedir*99,workdir*99,username*99,userID*9,hostname*99
  character :: cursorup*4,cursordown*4,cursorright*4,cursorleft*4 !Cursor movement
  logical :: student_mode
end module constants
!************************************************************************


!************************************************************************
subroutine setconstants
  use constants
  implicit none
  scrsz=10.8  !Screen dimensions: MacBook, Gentoo
  scrrat=0.57
  
  pi       =  4*datan(1.d0)                         !Pi, area of circle/r^2
  tpi      =  2*pi
  pi2      =  0.5d0*pi
  c3rd     =  1.d0/3.d0
  
  l0       =  3.83d33                               !Solar luminosity, erg s^-1
  r0       =  6.9599d10                             !Solar radius, cm
  m0       =  1.9891d33                             !Solar mass, g
  
  g        =  6.67259d-8                            !Newton's constant, cm^3 g^-1 s^-2
  c        =  2.99792458d10                         !Speed of light in vacuo, cm s^-1
  
  day      =  8.64d4                                !Day, s
  yr       =  3.15569d7                             !Year, s
  km       =  1.d5                                  !Kilometer, cm
  
  amu      =  1.6605402d-24                         !Atomic mass unit; (mass of C12 atom)/12, g
  m_h      =  1.007825*amu                          !Mass of a hydrogen atom
  k_b      =  1.380658d-16                          !Boltzmann constant, erg/K
  h_p      =  6.6260755d-27                         !Planck's constant, erg s
  h_bar    =  h_p/tpi                               !Reduced Planck constant, erg s
  a_rad    =  k_b**4/((c*h_p)**3) * 8*pi**5/15.d0   !Radiation (density) constant, 7.56591d-15 erg cm^-3 K^-4
  sigma    =  a_rad*c*0.25d0                        !Stefan-Boltzmann constant, 5.67051d-5 erg cm^-2 K^-4 s^-1
  
  call getenv('HOME',homedir)       !Set homedir  = $HOME (the environment variable)
  call getenv('PWD',workdir)        !Set workdir  = $PWD
  call getenv('HOSTNAME',hostname)  !Set hostname = $HOSTNAME  !Apparently not always exported
  call getenv('USER',username)      !Set username = $USER
  call getenv('UID',userid)         !Set userid   = $UID
  
  cursorup = char(27)//'[2A' !Print this to go up one line (on screen) (actually 2 lines, for some reason that's needed)
  cursordown = char(27)//'[1B' !Print this to go down one line (on screen)
  cursorright = char(27)//'[1C' !Makes the cursor move right one space
  cursorleft = char(27)//'[1D' !Makes the cursor move left one space
  
end subroutine setconstants
!************************************************************************








!************************************************************************      
subroutine lt2ubv(logl,logt,mass,logz,mbol,bolc,mv,uminb,bminv,vminr,rmini)
  !Computes values of Mv, U-B, B-V and V-I for given log L, log T, mass and log(Z/0.02)
  !  See: http://vizier.cfa.harvard.edu/viz-bin/ftp-index?/ftp/cats/J/MNRAS/298/525/evgrid
  !  UBVRI.Kur:  table of synthetic BC and UBVRI colours, from Kurucz model atmospheres (1992, IAU Symp 149, p.225)
  !  UBVRI.LBC:  empirically corrected version of the above, from Lejeune, Cuisinier & Buser (1997, A&AS 125, 229)
  
  use ubvdata
  implicit none
  real*8, parameter :: gconst=-10.6071d0
  integer :: k,ig,it,iz,find_index
  real*8 :: cm(5),ltgr(ntgr)
  real*8 :: logl,logt,mass,logz,mv,uminb,bminv,vminr,rmini
  real*8 :: logm,logg,dg1,dg2,dt1,dt2,dz1,dz2,mbol,bolc
  external find_index
  
  logm = log10(mass)
  logg = logm + 4*logt - logl + gconst
  ltgr = log10(tgr*100)
  
  !Find indices of log Z, log g and log T to interpolate between.
  !don't allow extrapolation outside log Z and log g grid.
  ig = find_index(logg,ggr,nggr)
  it = find_index(logt,ltgr,ntgr)
  iz = find_index(logz,zgr,nzgr)
  
  dg1 = (logg - ggr(ig-1))/(ggr(ig) - ggr(ig-1))
  dg1 = max(0.0d0, min(1.0d0, dg1))
  dg2 = 1.0d0 - dg1
  dt1 = (logt - ltgr(it-1))/(ltgr(it) - ltgr(it-1))
  dt2 = 1.0d0 - dt1
  dz1 = (logz - zgr(iz-1))/(zgr(iz) - zgr(iz-1))
  dz1 = max(0.0d0, min(1.0d0, dz1))
  dz2 = 1.0d0 - dz1
  
  do k = 4,8
     cm(k-3) = ((ubv(k,ig,it,iz)*dg1 + ubv(k,ig-1,it,iz)*dg2)*dt1  &
          + (ubv(k,ig,it-1,iz)*dg1 +  &
          ubv(k,ig-1,it-1,iz)*dg2)*dt2)*dz1 +  &
          ((ubv(k,ig,it,iz-1)*dg1 +  &
          ubv(k,ig-1,it,iz-1)*dg2)*dt1 +  &
          (ubv(k,ig,it-1,iz-1)*dg1 +  &
          ubv(k,ig-1,it-1,iz-1)*dg2)*dt2)*dz2
  end do
  
  !mbol = 4.75 - 2.5*logl
  mbol = 4.741 - 2.5*logl  !AF: 4.74 = -2.5*log10(l0) + 2.5*log10(4*pi*(10*pc)**2) - 11.49  !(Verbunt, p.36 -> cgs)
  bolc = cm(1)
  mv = mbol - bolc
  uminb = cm(2)
  bminv = cm(3)
  vminr = cm(4)
  rmini = cm(5)
  
end subroutine lt2ubv
!************************************************************************      




!***********************************************************************
function getos() !Determine the operating system type: 1-Linux, 2-MacOSX
  use constants
  implicit none
  integer :: i,system,getos
  character :: ostype*25
  i = system('uname > '//trim(homedir)//'/uname.tmp') !This gives Linux or Darwin
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
  character :: match*(len),names(maxfile)*99,findfile*99,fname*99,tempfile*99
  
  if(len_trim(homedir).le.0.or.len_trim(homedir).ge.99) then
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
  character :: match*(len),names(nff)*99,fnames(nff)*99,tempfile*99
  
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
  labels(112) = 'q\dcrit\u'       !q_crit; q_1 > q_crit gives dynamical MT (Hurley et al., 2002, Eq.57)
  labels(113) = 'M\dcomp,crit\u'  !M2 < M2,crit gives dynamical MT (Hurley et al., 2002, Eq.57)
  labels(114) = 'v\drot\u (km/s)' !Rotational velocity
  
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
subroutine set_plotpltn_labels(pglabels,asclabels,maxi)
  implicit none
  integer :: maxi
  character :: pglabels(maxi)*(*),asclabels(maxi)*(*)
  
  pglabels(1) = 'Model'
  pglabels(2) = 't (yr)'
  pglabels(3) = '\gDt (yr)'
  pglabels(4) = 'M (M\d\(2281)\u)'
  pglabels(5) = 'M\dHe\u (M\d\(2281)\u)'
  pglabels(6) = 'M\dCO\u (M\d\(2281)\u)'
  pglabels(7) = 'M\dONe\u (M\d\(2281)\u)'
  pglabels(8) = 'R (R\d\(2281)\u)'
  pglabels(9) = 'L (L\d\(2281)\u)'
  pglabels(10) = 'T\deff\u (K)'
  pglabels(11) = 'T\dc\u (K)'
  pglabels(12) = 'T\dmax\u (K)'
  pglabels(13) = '\gr\dc\u (g cm\u-3\d)'
  pglabels(14) = '\gr\dTmax\u (g cm\u-3\d)'
  pglabels(15) = 'U\dbind,env\u (erg)'
  pglabels(16) = 'L\dH\u (L\d\(2281)\u)'
  pglabels(17) = 'L\dHe\u (L\d\(2281)\u)'
  pglabels(18) = 'L\dC\u (L\d\(2281)\u)'
  pglabels(19) = 'L\d\gn\u (L\d\(2281)\u)'
  pglabels(20) = 'L\dth\u (L\d\(2281)\u)'
  pglabels(21) = 'P\drot\u (d)'
  pglabels(22) = 'K\u2\d'
  pglabels(23) = 'R\dcz\u'
  pglabels(24) = 'dR\dcz\u'
  pglabels(25) = 'T\det\u'
  pglabels(26) = 'R\dalfven\u'
  pglabels(27) = 'B\dp\u'
  pglabels(28) = 'P\dorb\u (d)'
  pglabels(29) = 'FLR'
  pglabels(30) = 'F1'
  pglabels(31) = 'dM (M\d\(2281)\u/yr)'
  pglabels(32) = 'dM\dwind\u (M\d\(2281)\u/yr)'
  pglabels(33) = 'dM\dmt\u (M\d\(2281)\u/yr)'
  pglabels(34) = 'H\dorb\u'
  pglabels(35) = 'H\dorb\u/dt'
  pglabels(36) = 'dH\dgw\u/dt'
  pglabels(37) = 'dH\dwml\u/dt'
  pglabels(38) = 'dH\ds-o\u/dt'
  pglabels(39) = 'dH\dmtr\u/dt'
  pglabels(40) = 'M\dcomp\u'
  pglabels(41) = 'e'
  pglabels(42) = 'H\dsurf\u'
  pglabels(43) = 'He\dsurf\u'
  pglabels(44) = 'C\dsurf\u'
  pglabels(45) = 'N\dsurf\u'
  pglabels(46) = 'O\dsurf\u'
  pglabels(47) = 'Ne\dsurf\u'
  pglabels(48) = 'Mg\dsurf\u'
  pglabels(49) = 'H\dTmax\u'
  pglabels(50) = 'He\dTmax\u'
  pglabels(51) = 'C\dTmax\u'
  pglabels(52) = 'N\dTmax\u'
  pglabels(53) = 'O\dTmax\u'
  pglabels(54) = 'Ne\dTmax\u'
  pglabels(55) = 'Mg\dTmax\u'
  pglabels(56) = 'H\dcentr\u'
  pglabels(57) = 'He\dcentr\u'
  pglabels(58) = 'C\dcentr\u'
  pglabels(59) = 'N\dcentr\u'
  pglabels(60) = 'O\dcentr\u'
  pglabels(61) = 'Ne\dcentr\u'
  pglabels(62) = 'Mg\dcentr\u'
  pglabels(71) = 'M\dHe\u-M\dCO\u (M\d\(2281)\u)'
  pglabels(72) = 'M.I. (M\d\(2281)\u R\d\(2281)\u\u2\d)'
  pglabels(73) = '\gr\davg\u (g cm\u-3\d)'
  pglabels(74) = 'Q\dconv\u'
  pglabels(75) = 'Z\dsurf\u'
  pglabels(76) = '(t\df\u - t)  (yr)'
  pglabels(77) = 't/t\df\u'
  pglabels(78) = 'L\dHe\u/L\dH\u'
  
  pglabels(81) = 'V'
  pglabels(82) = 'U-B'
  pglabels(83) = 'B-V'
  pglabels(84) = 'V-R'
  pglabels(85) = 'R-I'
  pglabels(86) = 'U-V'
  pglabels(87) = 'V-I'
  
  pglabels(88) = 'k\u2\dR\u2\d'
  pglabels(89) = 'M\denv\u'        !M_env
  pglabels(90) = '\(2137)\denv\u'  !lambda_env
  pglabels(91) = 'Reimers ratio'   !Ratio of Reimers-like wind terms in Eggleton code - which dominates? - Politano et al. 2010, Eq.1
  
  
  
  
  
  asclabels(1) = 'model'
  asclabels(2) = 'time'
  asclabels(3) = 'dtime'
  asclabels(4) = 'mass'
  asclabels(5) = 'Mhe'
  asclabels(6) = 'Mco'
  asclabels(7) = 'Mone'
  asclabels(8) = 'radius'
  asclabels(9) = 'luminosity'
  asclabels(10) = 'Teff'
  asclabels(11) = 'Tc'
  asclabels(12) = 'Tmax'
  asclabels(13) = 'cendens'
  asclabels(14) = 'Tmaxdens'
  asclabels(15) = 'Ebind'
  asclabels(16) = 'LH'
  asclabels(17) = 'LHe'
  asclabels(18) = 'LC'
  asclabels(19) = 'L\gn'
  asclabels(20) = 'Lth'
  asclabels(21) = 'Prot'
  asclabels(22) = 'K2'
  asclabels(23) = 'Rcz'
  asclabels(24) = 'dRcz'
  asclabels(25) = 'Tet'
  asclabels(26) = 'Ralfven'
  asclabels(27) = 'Bp'
  asclabels(28) = 'Porb'
  asclabels(29) = 'FLR'
  asclabels(30) = 'F1'
  asclabels(31) = 'dM'
  asclabels(32) = 'dMwind'
  asclabels(33) = 'dMmt'
  asclabels(34) = 'Horb'
  asclabels(35) = 'Horbdt'
  asclabels(36) = 'dHgwdt'
  asclabels(37) = 'dHwmldt'
  asclabels(38) = 'dHsodt'
  asclabels(39) = 'dHmtrdt'
  asclabels(40) = 'Mcomp'
  asclabels(41) = 'e'
  asclabels(42) = 'Hsurf'
  asclabels(43) = 'Hesurf'
  asclabels(44) = 'Csurf'
  asclabels(45) = 'Nsurf'
  asclabels(46) = 'Osurf'
  asclabels(47) = 'Nesurf'
  asclabels(48) = 'Mgsurf'
  asclabels(49) = 'HTmax'
  asclabels(50) = 'HeTmax'
  asclabels(51) = 'CTmax'
  asclabels(52) = 'NTmax'
  asclabels(53) = 'OTmax'
  asclabels(54) = 'NeTmax'
  asclabels(55) = 'MgTmax'
  asclabels(56) = 'Hcentr'
  asclabels(57) = 'Hecentr'
  asclabels(58) = 'Ccentr'
  asclabels(59) = 'Ncentr'
  asclabels(60) = 'Ocentr'
  asclabels(61) = 'Necentr'
  asclabels(62) = 'Mgcentr'
  asclabels(71) = 'MHe-MCO'
  asclabels(72) = 'M.I.'
  asclabels(73) = 'avgdens'
  asclabels(74) = 'Qconv'
  asclabels(75) = 'Zsurf'
  asclabels(76) = 'tf-t'
  asclabels(77) = 'ttf'
  asclabels(78) = 'LHeLH'
  
  asclabels(81) = 'V'
  asclabels(82) = 'U-B'
  asclabels(83) = 'B-V'
  asclabels(84) = 'V-R'
  asclabels(85) = 'R-I'
  asclabels(86) = 'U-V'
  asclabels(87) = 'V-I'
  
  asclabels(88) = 'k2R2'
  asclabels(89) = 'Menv'        !M_env
  asclabels(90) = 'lambda'  !lambda_env
  asclabels(91) = 'Reimersrat'   !Ratio of Reimers-like wind terms in Eggleton code - which dominates? - Politano et al. 2010, Eq.1
  
  
  
  
  
end subroutine set_plotpltn_labels
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
  write(6,'(A)'),'   82: Mhe-Mco              92: Pgw,max         102: U-B      112: q_crit               '  
  write(6,'(A)'),'   83: Menv                 93: Rrl             103: B-V      113: M2,crit              '
  write(6,'(A)'),'   84: Mconv                94: Xf              104: V-R      114: Vrot                 '
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
     !read(u,10,err=12,end=11) (dat(i,j),i=1,ncols)
     read(u,*,err=12,end=11) (dat(i,j),i=1,ncols)
  end do
!10 format(F6.0,E17.9,E14.6,11F9.5,7E12.4,3F9.5,16E12.4,F8.4,21E13.5,12F9.5,6F9.5,E14.6,E12.5) !Can read upto 82 columns
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
  open(unit=u,form='formatted',status='old',file=trim(fname))
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
  real*8 :: c92(nn),c85a,c85b,x,z,mbol,bc
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
           if(abs((dat(j-1,i)+dat(j,i))/dat(4,i)).lt.1.d-4) dat(j-1:j,i) = (/0.d0,0.d0/)  !If upper and lower boundary are close enough, remove them
        end do !j
        do while(abs(dat(j0,i))/dat(4,i).lt.1.d-4.and.sum(abs(dat(j0:j0+5,i))).gt.1.e-7)  !Remove all the leading zeroes
           do j=j0,j0+4
              dat(j,i) = dat(j+1,i)
           end do
           dat(j0+5,i) = 0.d0
        end do
     end do !i
  end do !j0
  
  
  
  !************************************************************************      
  !***   CHANGE EXISTING PLOT VARIABLES
  !************************************************************************      
  
  dat(5,1:n) = dat(5,1:n) + 1.d-30                              !Still necessary?
  dat(15,:) = dat(15,:)*m0*1.d-40                               !Ubind in 10^40 ergs
  
  dat(42:62,:) = max(dat(42:62,:),1.e-10)                       !Abundances: limit them to >10^-10  -  isn't it weird that the compiler actually understands this...?  You'd need at least two for-loops in freakin' C!
  
  
  
  !************************************************************************      
  !***   CREATE EXTRA PLOT VARIABLES
  !************************************************************************      
  
  do i=1,n
     dat(82,i) = dat(5,i)-dat(6,i)                              !Intershell mass
  end do
  dat(83,1:n) = dat(4,1:n) - dat(5,1:n)                         !H-envelope mass
  do i=1,n
     dat(84,i) = 0.d0
     if(dat(64,i).lt.0.d0) dat(84,i) = abs(dat(64,i))          !Convective core boundary
  end do
  
  dat(85,1) = 0.d0
  do i=2,n
     c85a = abs(dat(8,i)-dat(8,i-1))+1.d-30                    !dR
     c85b = abs(dat(2,i)-dat(2,i-1))+1.d-30                    !dt
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
  dat(98,1:n) = 1.d0 - dat(42,1:n)-dat(43,1:n)                              !Z_surf = 1 - X - Y:  surface metallicity
  dat(99,1:n) = dat(2,n) - min(dat(2,1:n), dat(2,n)-1.d4)                   !t - t_final, avoid by setting dat(,1) = dat(,2)
  
  !dat(100,1:n) = sqrt(2*g*dat(4,1:n)*m0/(dat(8,1:n)*r0)**3)/day             !Critical (Keplerian) omega
  dat(100,1:n) = 2*pi*sqrt((dat(8,1:n)*r0)**3/(g*dat(4,1:n)*m0))/day         !Critical (Keplerian) rotation period
  dat(100,1:n) = dat(21,1:n)/(dat(100,1:n)+1.e-30)                           !Prot/Pcrit
  
  !Colours
  do i=1,n
     call lt2ubv(log10(dat(9,i)),log10(dat(10,i)),dat(4,i),log10(dat(98,i)/2.d-2),mbol,bc,dat(101,i),dat(102,i), dat(103,i),dat(104,i),dat(105,i))
     dat(106,i) = dat(102,i)+dat(103,i)                                     !(U-V) = (U-B) + (B-V)
     dat(107,i) = dat(104,i)+dat(105,i)                                     !(V-I) = (V-R) + (R-I)
  end do
  
  dat(111,1:n) = g*dat(4,1:n)*dat(83,1:n)*m0**2 / (dat(15,1:n)*dat(8,1:n)*r0*1.d40+1.d-30)  !lambda_env = G*M*M_env/(Ubind*R)
  !dat(111,1:n) = abs(dat(111,1:n))    !This 'hides' the fact that Ubind changes sign
  dat(111,1:n) = max(dat(111,1:n),0.d0)
  do i=1,n
     if(abs(dat(5,i)).lt.1.d-29) dat(111,i) = 0.d0  !If there's no He core mass, there's no lambda
     !write(*,'(I6,9ES20.5)')i,dat(4:5,i),dat(83,i),dat(15,i),dat(8,i),dat(111,i)
  end do
  
  z = log10(dat(98,1)/0.02)  !Use the surface Z of the first model as 'the' metallicity
  x = 0.30406 + 0.0805*z + 0.0897*z*z + 0.0878*z**3 + 0.0222*z**4
  
  dat(112,1:n) = (1.67 - x + 2*(dat(5,1:n)/(dat(4,1:n)+1.d-30))**5)/2.13
  dat(113,1:n) = dat(4,1:n)/(dat(112,1:n)+1.d-30)
  do i=1,n
     if(dat(5,i).lt.1.e-6) then
        dat(112,i) = 0.
        dat(113,i) = 0.
     end if
     !write(6,'(I6,9ES12.3)')i,dat(4,i),dat(5,i),dat(112,i),dat(113,i)
  end do
  
  dat(114,1:n) = tpi*dat(8,1:n)*r0/(dat(21,1:n)*day)/km  !Vrot = 2piR/P -> km/s
  
  
  !Timescales
  dat(201,1:n) = g*dat(4,1:n)**2*m0*m0 / (dat(8,1:n)*r0*dat(9,1:n)*l0)/yr   !KH timescale
  dat(202,1:n) = dat(4,1:n)*m0/1.9891/(dat(9,1:n)*l0)*4.d10                 !Nuclear evolution timescale
  dat(203,1:n) = dat(4,1:n)/max(abs(dat(33,1:n)),1.e-30)                    !Mass transfer
  dat(204,1:n) = dat(34,1:n)/max(dat(36,1:n)*yr,1.e-30)                     !Gravitational waves
  !dat(205,1:n) = dat(34,1:n)/max(abs(dat(38,1:n))*yr,1.e-30)               !Magnetic braking (Actually SO-coupling!)
  dat(205,1:n) = dsqrt(dat(8,1:n)**3/(g*dat(4,1:n)))                        !Dynamical: t ~ sqrt(R^3/(G*M))
  dpdt  = 0
  
  !Replace dH/dt by dP/dt
  !35 = H_orb,  36 = H_gw, 37 = H_wml, 38 = H_s-o, 39 = H_mtr
  if(1.eq.2) then
     dpdj(1:n) = 3.d0/(dat(4,1:n)*dat(40,1:n)*m0*m0)*(2.d0*pi* (dat(28,1:n)*day)**2*(dat(4,1:n)+dat(40,1:n))*m0/(g*g)) **(1.d0/3.d0)                      !dP/dJ = 3/(m1m2)(2piP^2(m1+m2)/G^2)^1/3
     do i=35,39
        dat(i,1:n) = dat(i,1:n)*dpdj(1:n)*1.d50+1.d-30
     end do
     labels(35) = 'dP\dorb\u/dt'
     labels(36) = 'dP\dgw\u/dt'
     labels(37) = 'dP\dwml\u/dt'
     labels(38) = 'dP\ds-o\u/dt'
     labels(39) = 'dP\dmtr\u/dt'
     labels(202) = 'dP\dorb\u/dt'
     dpdt = 1
  end if
  
  !Replace dP/dt by timescales 
  if(1.eq.1) then
     do i=35,39
        dat(i,1:n) = dat(28,1:n)*day/dat(i,1:n)/yr
     end do
     labels(35) = '\gt\dP\dorb\u\u (yr)'
     labels(36) = '\gt\dP\dgw\u\u (yr)'
     labels(37) = '\gt\dP\dwml\u\u (yr)'
     labels(38) = '\gt\dP\ds-o\u\u (yr)'
     labels(39) = '\gt\dP\dmtr\u\u (yr)'
     labels(202) = '\gt\dP\dorb\u\u (yr)'
     dpdt = 2
  end if
  
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
function find_index(v,arr,narr)  !Double precision
  !Finds index of value v in monotonously increasing or decreasing array arr of length narr
  implicit none
  integer :: find_index,narr,i,iLow,iHigh
  real*8 :: v,arr(narr),range
  
  range = arr(narr) - arr(1)
  iLow = 1
  iHigh = narr
  
  do while(iHigh-iLow.gt.1)
     i = (iHigh + iLow)/2
     if ((v-arr(i))*range .gt. 0.0) then
        iLow = i
     else
        iHigh = i
     end if
  end do
  
  find_index = iHigh
end function find_index
!************************************************************************      




!***********************************************************************
subroutine locate(arr,narr,v,i)  !Double precision
  !Input: 
  !  arr: monotonic array
  !  narr:  length of arr
  !  v:  value to look for
  !  Output:
  !  i:  returned value, such that v is between arr(i) and arr(i+1).  If i=0 or narr, v is out of range
  
  implicit none
  integer :: i,narr,iLow,iMid,iHigh
  real*8 :: v,arr(narr)
  
  iLow = 0
  iHigh = narr+1
  do while(iHigh-iLow.gt.1)
     iMid = (iHigh+iLow)/2
     if((arr(narr).ge.arr(1)) .eqv. (v.ge.arr(iMid))) then
        iLow = iMid
     else
        iHigh = iMid
     end if
  end do
  
  if(v.eq.arr(1)) then
     i = 1
  else if(v.eq.arr(narr)) then
     i = narr-1
  else
     i = iLow
  end if
  
end subroutine locate
!***********************************************************************


!***********************************************************************
subroutine locater(rarr,narr,rv,i)  !Single precision
  !Input: 
  !  rarr:  monotonic array
  !  narr:  length of rarr
  !  rv:    value to look for
  !  Output:
  !  i:     returned value, such that v is between arr(i) and arr(i+1).  If i=0 or narr, v is out of range
  
  implicit none
  integer :: i,narr
  real :: rv,rarr(narr)
  real*8 :: dv,darr(narr)
  
  darr = dble(rarr)
  dv  = dble(rv)
  call locate(darr,narr,dv,i)  !i will be returned to the calling routine
  
end subroutine locater
!************************************************************************      


!************************************************************************************************************************************
subroutine rindex(narr,rarr,indx)  !Return a sorted index indx of array rarr of length n - single precision
  implicit none
  integer :: narr,indx(narr)
  real :: rarr(narr)
  real*8 :: darr(narr)
  
  darr = dble(rarr)
  call dindex(narr,darr,indx)  !Call the double-precision routine
  
end subroutine rindex
!************************************************************************************************************************************

!************************************************************************************************************************************
subroutine dindex(narr,arr,ind)  !Return a sorted index ind of array arr of length narr - double precision
  implicit none
  integer, parameter :: m=7,nstack=50
  integer :: narr,ind(narr)
  real*8 :: arr(narr),a
  integer :: i,indxt,ir,itemp,j,jstack,k,l,istack(nstack)
  
  do j=1,narr
     ind(j) = j
  end do
  
  jstack = 0
  l = 1
  ir = narr
  
1 if(ir-l.lt.m) then
     do j = l+1,ir
        indxt = ind(j)
        a = arr(indxt)
        do i = j-1,l,-1
           if(arr(ind(i)).le.a) goto 2
           ind(i+1) = ind(i)
        end do
        i = l-1
2       ind(i+1) = indxt
     end do
     if(jstack.eq.0) return
     ir = istack(jstack)
     l = istack(jstack-1)
     jstack = jstack-2
     
  else
     
     k = (l+ir)/2
     itemp = ind(k)
     ind(k) = ind(l+1)
     ind(l+1) = itemp
     if(arr(ind(l)) .gt. arr(ind(ir))) then
        itemp = ind(l)
        ind(l) = ind(ir)
        ind(ir) = itemp
     end if
     if(arr(ind(l+1)) .gt. arr(ind(ir))) then
        itemp = ind(l+1)
        ind(l+1) = ind(ir)
        ind(ir) = itemp
     end if
     if(arr(ind(l)) .gt. arr(ind(l+1))) then
        itemp = ind(l)
        ind(l) = ind(l+1)
        ind(l+1) = itemp
     end if
     i = l+1
     j = ir
     indxt = ind(l+1)
     a = arr(indxt)
     
3    continue
     i = i+1
     if(arr(ind(i)).lt.a) goto 3
     
4    continue
     j = j-1
     if(arr(ind(j)).gt.a) goto 4
     if(j.lt.i) goto 5
     itemp = ind(i)
     ind(i) = ind(j)
     ind(j) = itemp
     goto 3
     
5    ind(l+1) = ind(j)
     ind(j) = indxt
     jstack = jstack+2
     if(jstack.gt.nstack) write(0,'(A)')'  *** Warning:  nstack too small in dindex  ***'
     if(ir-i+1.ge.j-l) then
        istack(jstack) = ir
        istack(jstack-1) = i
        ir = j-1
     else
        istack(jstack) = j-1
        istack(jstack-1) = l
        l = i
     end if
  end if
  
  goto 1
end subroutine dindex
!************************************************************************************************************************************



!***********************************************************************
subroutine polint(xa,ya,n,x,y,dy)  !Single precision
  implicit none
  integer, parameter :: nmax=10
  integer :: n
  real :: dy,x,y,xa(n),ya(n)
  integer :: i,m,ns
  real :: den,dif,dift,ho,hp,w,c(nmax),d(nmax)
  
  ns = 1
  dif = abs(x-xa(1))
  do i=1,n
     dift = abs(x-xa(i))
     if (dift.lt.dif) then
        ns = i
        dif = dift
     end if
     c(i) = ya(i)
     d(i) = ya(i)
  end do
  y = ya(ns)
  ns = ns-1
  do m=1,n-1
     do i=1,n-m
        ho = xa(i)-x
        hp = xa(i+m)-x
        w = c(i+1)-d(i)
        den = ho-hp
        if(den.eq.0.) write(0,'(A)')'  *** Warning:  Failure in polint  ***'
        den = w/den
        d(i) = hp*den
        c(i) = ho*den
     end do
     if (2*ns.lt.n-m) then
        dy = c(ns+1)
     else
        dy = d(ns)
        ns = ns-1
     end if
     y = y+dy
  end do
  return
end subroutine polint
!***********************************************************************




!***********************************************************************
subroutine polintd(xa,ya,n,x,y,dy)  !Double precision
  implicit none
  integer, parameter :: nmax=10
  integer :: n
  real*8 :: dy,x,y,xa(n),ya(n)
  integer :: i,m,ns
  real*8 :: den,dif,dift,ho,hp,w,c(nmax),d(nmax)
  
  ns = 1
  dif = abs(x-xa(1))
  do i=1,n
     dift = abs(x-xa(i))
     if (dift.lt.dif) then
        ns = i
        dif = dift
     end if
     c(i) = ya(i)
     d(i) = ya(i)
  end do
  y = ya(ns)
  ns = ns-1
  do m=1,n-1
     do i=1,n-m
        ho = xa(i)-x
        hp = xa(i+m)-x
        w = c(i+1)-d(i)
        den = ho-hp
        if(den.eq.0.d0) write(0,'(A)')'  *** Warning:  Failure in polint  ***'
        den = w/den
        d(i) = hp*den
        c(i) = ho*den
     end do
     if (2*ns.lt.n-m) then
        dy = c(ns+1)
     else
        dy = d(ns)
        ns = ns-1
     end if
     y = y+dy
  end do
  return
end subroutine polintd
!***********************************************************************




!***********************************************************************
function ran1(seed)
  implicit none
  integer, parameter :: ia=16807,im=2147483647,iq=127773,ir=2836,ntab=32,ndiv=1+(im-1)/ntab
  real, parameter :: am=1./im, eps=1.2e-7, rnmx=1.-eps
  integer :: j,k,iv(ntab),iy,seed
  real :: ran1
  
  save iv,iy
  iv = 0
  iy = 0
  
  if (seed.le.0.or.iy.eq.0) then
     seed = max(-seed,1)
     do j=ntab+8,1,-1
        k = seed/iq
        seed = ia*(seed-k*iq)-ir*k
        if (seed.lt.0) seed = seed+im
        if (j.le.ntab) iv(j) = seed
     end do
     iy = iv(1)
  end if
  
  k = seed/iq
  seed = ia*(seed-k*iq)-ir*k
  if (seed.lt.0) seed = seed+im
  j = 1+iy/ndiv
  iy = iv(j)
  iv(j) = seed
  ran1 = min(am*iy,rnmx)
  
end function ran1
!***********************************************************************



!***********************************************************************
subroutine spline(x,y,n,yp1,ypn,y2)
  implicit none
  !integer, parameter :: nmax=1000
  integer :: n,i,k
  real*8 :: yp1,ypn,x(n),y(n),y2(n)
  real*8 :: p,qn,sig,un,u(n)
  
  !if(n.gt.nmax) then
  !   write(0,'(/,A)')'  ERROR: spline():  n > nmax'
  !   write(0,'(A,/)')'  Aborting...'
  !   stop
  !end if
  
  if(yp1.gt.0.99d30) then
     y2(1) = 0.
     u(1) = 0.
  else
     y2(1) = -0.5
     u(1) = (3./(x(2)-x(1)))*((y(2)-y(1))/(x(2)-x(1))-yp1)
  end if
  
  do i=2,n-1
     sig = (x(i)-x(i-1))/(x(i+1)-x(i-1))
     p = sig*y2(i-1)+2.
     y2(i) = (sig-1.)/p
     u(i) = (6 * ( (y(i+1)-y(i)) / (x(i+1)-x(i)) - (y(i)-y(i-1)) / (x(i)-x(i-1)) ) / (x(i+1)-x(i-1)) - sig*u(i-1)) / p
  end do
  
  if(ypn.gt..99e30) then
     qn = 0.
     un = 0.
  else
     qn = 0.5
     un = (3./(x(n)-x(n-1)))*(ypn-(y(n)-y(n-1))/(x(n)-x(n-1)))
  end if
  
  y2(n) = (un-qn*u(n-1))/(qn*y2(n-1)+1.)
  do k=n-1,1,-1
     y2(k) = y2(k)*y2(k+1)+u(k)
  end do
  
end subroutine spline
!***********************************************************************


!***********************************************************************
subroutine splint(xa,ya,y2a,n,x,y)
  implicit none
  integer :: n,k,khi,klo
  real*8 :: x,y,xa(n),y2a(n),ya(n),a,b,h
  
  klo = 1
  khi = n
1 if(khi-klo.gt.1) then
     k = (khi+klo)/2
     if(xa(k).gt.x) then
        khi = k
     else
        klo = k
     end if
     goto 1
  end if
  
  h = xa(khi)-xa(klo)
  if(h.eq.0.) write(0,'(A)')'  *** Warning:  Bad xa input in splint  ***'
  a = (xa(khi)-x)/h
  b = (x-xa(klo))/h
  y = a*ya(klo)+b*ya(khi)+((a**3-a)*y2a(klo)+(b**3-b)*y2a(khi))*(h**2)/6.
  
end subroutine splint
!***********************************************************************






!********************************************************************************      
function a2j(m1,m2,a)  !a to Orbital angular momentum
  use constants
  implicit none
  real*8 :: a2j,m1,m2,a
  a2j = m1*m2*dsqrt(g*a*r0/(m1+m2)*m0**3)!*m0**1.5d0
end function a2j
!***********************************************************************

!********************************************************************************      
function j2a(m1,m2,j)  !Orbital angular momentum to a
  use constants
  implicit none
  real*8 :: j2a,m1,m2,j
  j2a = (j/(m1*m2))**2 * (m1+m2)/(g*m0**3)/r0
end function j2a
!***********************************************************************



!********************************************************************************      
function p2j(m1,m2,p)  !P to Orbital angular momentum, all in cgs units
  use constants
  implicit none
  real*8 :: p2j,m1,m2,p,a,p2a
  a = p2a(m1+m2,p)
  p2j = m1*m2*sqrt(g*a/(m1+m2))
end function p2j
!***********************************************************************

!********************************************************************************      
function j2p(m1,m2,j)  !Orbital angular momentum to P, all in cgs units
  use constants
  implicit none
  real*8 :: j2p,m1,m2,j,a,a2p
  a = (j/(m1*m2))**2 * (m1+m2)/g
  j2p = a2p(m1+m2,a)
end function j2p
!***********************************************************************



!********************************************************************************      
function p2a(mtot,p)  !P, a, mtot in cgs units
  use constants
  implicit none
  real*8 :: p2a,mtot,p
  p2a = (g*mtot/(4*pi**2))**c3rd * p**(2*c3rd)
end function p2a
!***********************************************************************

!********************************************************************************      
function a2p(mtot,a)  !P, a, mtot in cgs units
  use constants
  implicit none
  real*8 :: a2p,mtot,a
  a2p = (4*pi**2/(g*mtot))**0.5d0*a**1.5d0
end function a2p
!***********************************************************************


!************************************************************************
function a2rl(m1,m2,a)  !m1 and m2 in the same units, Rl and a in the same units
  use constants
  implicit none
  real*8 :: a2rl,q,m1,m2,a
  q = m1/m2
  a2rl = a / (0.6d0*q**(2*c3rd) + log(1.d0 + q**c3rd)) * (0.49d0*q**(2*c3rd))
end function a2rl
!***********************************************************************



!************************************************************************
function rl2a(m1,m2,rl1)  !m1 and m2 in the same units, Rl and a in the same units
  use constants
  implicit none
  real*8 :: rl2a,q,m1,m2,rl1
  q = m1/m2
  rl2a = rl1/(0.49d0*q**(2*c3rd)/(0.6d0*q**(2*c3rd) + log(1.d0+q**c3rd)))
end function rl2a
!***********************************************************************


!************************************************************************
function p2rl(m1,m2,p)  !All in cgs units
  implicit none
  real*8 :: p2rl,m1,m2,p,a,p2a,a2rl
  a = p2a(m1+m2,p)
  p2rl = a2rl(m1,m2,a)
end function p2rl
!************************************************************************


!************************************************************************
function rl2p(m1,m2,rl1)  !All in cgs units
  implicit none
  real*8 :: rl2p,m1,m2,rl1,a,a2p,rl2a
  a = rl2a(m1,m2,rl1)
  rl2p = a2p(m1+m2,a)
end function rl2p
!************************************************************************




!************************************************************************
subroutine quit_program(message,len)  !Print a message and quit
  implicit none
  character :: message*(len)
  integer :: len
  
  if(len.ge.1.and.len.le.199) write(0,'(/,A)')'  '//trim(message(1:len))
  write(0,'(A,/)')'  Aborting...'
  stop
end subroutine quit_program
!************************************************************************



!************************************************************************************************************************************
subroutine bin_data_1d(n,x,norm,nbin,xmin1,xmax1,xbin,ybin)  !Count the number of points in each bin (1D), taken from analyse_mcmc
  ! x - input: data, n points
  ! norm - input: normalise (1) or not (0)
  ! nbin - input: number of bins
  ! xmin, xmax - in/output: set xmin=xmax to auto-determine
  ! xbin, ybin - output: binned data (x, y).  The x values are the left side of the bin!
  
  implicit none
  integer :: i,k,n,nbin,norm
  real :: x(n),xbin(nbin+1),ybin(nbin+1),xmin,xmax,dx,xmin1,xmax1
  
  xmin = xmin1
  xmax = xmax1
  
  if(abs((xmin-xmax)/(xmax+1.e-30)).lt.1.e-20) then  !Autodetermine
     xmin = minval(x(1:n))
     xmax = maxval(x(1:n))
     xmin1 = xmin                                    !And return new values
     xmax1 = xmax
  end if
  dx = abs(xmax - xmin)/real(nbin)
  
  do k=1,nbin+1
     !xbin(k) = xmin + (real(k)-0.5)*dx  !x is the centre of the bin
     xbin(k) = xmin + (k-1)*dx          !x is the left of the bin
  end do
  !ybintot=0.
  ybin = 0.
  do i=1,n
     do k=1,nbin
        if(x(i).ge.xbin(k)) then
           if(x(i).lt.xbin(k+1)) then
              ybin(k) = ybin(k) + 1.
              exit !If point i fits in this bin, don't try the others
           end if
        end if
     end do !k (bin)
     !ybintot = ybintot + ybin(k)
  end do
  !if(norm.eq.1) ybin = ybin/(ybintot+1.e-30)
  if(norm.eq.1) ybin = ybin/(sum(ybin)+1.e-30)
  
end subroutine bin_data_1d
!************************************************************************************************************************************


!************************************************************************
function time_stamp(os)  !Get time stamp in seconds since 1970-01-01 00:00:00 UTC
  implicit none
  real*8 :: time_stamp
  integer :: os,i,system
  character :: fname*99
  
  fname = './.analysemcmc_time_stamp'  !gfortran doesn't want to read from ~ for some reason
  if(os.eq.2) then !MacOS
     i = system('date +%s >& '//trim(fname)) !%N for fractional seconds doesn't work on MacOS!!! (But it does with GNU date)
  else !GNU/Linux, default
     i = system('date +%s.%N >& '//trim(fname))
  end if
  open(unit=9,status='old',file=trim(fname))
  read(9,*)time_stamp
  close(9)
  i = system('rm -f '//trim(fname))
end function time_stamp
!************************************************************************



!************************************************************************************************************************************
subroutine set_PGPS_title(PSfile,PStitle)  !Set the title in a Postscript file generated by PGPlot
  implicit none
  integer :: status,system
  character :: PSfile*(*),PStitle*(*),tempfile*99
  
  tempfile = 'temp_PGPS_file.eps'
  
  status = system("sed -e 's/Title: PGPLOT PostScript plot/Title: "//trim(PStitle)//"/' "//trim(PSfile)//" > "//trim(tempfile))
  if(status.eq.0) then
     status = system('mv -f '//trim(tempfile)//' '//trim(PSfile))
  else
     status = system('rm -f '//trim(tempfile))
  end if
  
  status = system("sed -e 's/For: AstroFloyd/For: AstroFloyd - astrofloyd.org/' "//trim(PSfile)//" > "//trim(tempfile))
  if(status.eq.0) then
     status = system('mv -f '//trim(tempfile)//' '//trim(PSfile))
  else
     status = system('rm -f '//trim(tempfile))
  end if
  
end subroutine set_PGPS_title
!************************************************************************************************************************************


