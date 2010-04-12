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
  !scrsz=10.8  !Screen dimensions: MacBook, Gentoo
  !scrrat=0.57 
  
  scrsz=14.406   !Screen dimensions: ThinkPad
  scrrat=0.5875 
  
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
  
  call get_environment_variable('HOME',homedir)       !Set homedir  = $HOME (the environment variable)
  call get_environment_variable('PWD',workdir)        !Set workdir  = $PWD
  call get_environment_variable('HOSTNAME',hostname)  !Set hostname = $HOSTNAME  !Apparently not always exported
  call get_environment_variable('USER',username)      !Set username = $USER
  call get_environment_variable('UID',userid)         !Set userid   = $UID
  
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
end function getos
!***********************************************************************


!***********************************************************************************************************************************
function findfile(match)
  use constants
  implicit none
  integer, parameter :: maxfile=1000
  integer :: i,k,fnum,system
  character :: match*(*),names(maxfile)*99,findfile*99,fname*99,tempfile*99
  
  if(len_trim(homedir).le.0.or.len_trim(homedir).ge.99) then
     write(0,'(/,A,/)')'  Findfile:  ERROR:  variable homedir not defined (forgot to call setconstants?), quitting.'
     stop
  end if
  
  tempfile = trim(homedir)//'/.findfile.tmp'
  i = system('ls '//trim(match)//' 1> '//trim(tempfile)//' 2> /dev/null')  !Shell command to list all the files with the search string and pipe them to a temporary file
  
  k=0
  names = ''
  open(10,file=trim(tempfile), status='old', form='formatted') !Read the temp file and delete it when closing
  rewind(10)
  do i=1,maxfile 
     read(10,'(A99)',end=100) names(i)
     k=k+1
  end do
  
100 close(10, status='delete')
  
  fname = names(1)
  
  fnum = 1
  if(k.gt.1) then
     write(6,'(A)')'  Files found:'
     do i=1,k
        write(6,'(I5,A)')i,':  '//trim(names(i))
     end do
     write(6,'(/,A)',advance='no')'  Enter the number of the file you want to view: '
     read*,fnum
     if(fnum.le.0.or.fnum.gt.k) then
        write(*,'(/,A,/)')'  No file selected, quitting...'
        stop
     end if
     fname = names(fnum)
  end if
  
  if(k.eq.0.or.fnum.eq.0) then
     fname = ''
     !if(k.eq.0) write(6,'(A)')'  No file found in this directory'
  end if
  
  findfile = fname
  
end function findfile
!***********************************************************************



!***********************************************************************
subroutine findfiles(match,nff,all,fnames,nf)  
  !Input:
  !  match:   search string to match
  !  nff:     maximum number of files to return
  !  all:     0-select manually from list, 1-always return all files in list
  !Output:
  !  fnames:  array that contains the files found; make sure it has the same length as the array in the calling programme
  !  nf:      the actual number of files returned in fnames ( = min(number found, nff))
  
  use constants
  implicit none
  integer :: i,j,k,fnum,nf,nff,system,all
  character :: match*(*),names(nff)*99,fnames(nff)*99,tempfile*99
  
  if(len_trim(homedir).eq.99) then
     write(0,'(/,A,/)')'  Findfiles:  ERROR:  variable homedir not defined (forgot to call setconstants?), quitting.'
     stop
  end if
  
  tempfile = trim(homedir)//'/.findfile.tmp'
  i = system('ls '//trim(match)//' > '//trim(tempfile))  !Shell command to list all the files with the search string and pipe them to a temporary file
  
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
     !write(6,'(A)')'  No file found in this directory'
     nf = 0
  end if
  
200 continue
  
end subroutine findfiles
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
subroutine quit_program(message)  !Print a message and quit
  implicit none
  character :: message*(*)
  integer :: len
  
  len = len_trim(message)
  if(len.ge.1.and.len.le.199) write(0,'(/,A)')'  '//trim(message)
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


