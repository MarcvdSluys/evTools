!> \file functions.f90  Shared modules, functions and subroutines for the evTools package
!!
!! For general functions and routines that need PGPlot, see plotfunctions.f90


! Copyright 2002-2012 AstroFloyd - astrofloyd.org
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
!> \brief  Contains the integers double and dbl, which shall be used in (almost) all routines
!! to provide the kind of a (currently double-precision) real variable type.
!! 
!! Variables can be declared using "real(double) :: "; constants can be defined as 
!! e.g. "x = 3.0_dbl".

module kinds
   implicit none
   
   ! Integer, double precision:
   integer, parameter :: long = selected_int_kind(18)
   
   ! Real, double precision:
   integer, parameter :: double = selected_real_kind(15,307)  !Precision = 15, range = 307
   integer, parameter :: dbl = selected_real_kind(15,307)     !Precision = 15, range = 307
   
end module kinds
!***********************************************************************************************************************************


!***********************************************************************************************************************************
!> \brief  Contains data to compute magnitudes and colours from L,T,g

module ubvdata
  use kinds, only: double
  implicit none
  save
  private :: double
  
  integer, parameter :: nzgr=9,ntgr=61,nggr=11
  real(double) :: zgr(nzgr),tgr(ntgr),ggr(nggr),ubv(8,nggr,ntgr,nzgr)
  
  data zgr /-1.d0,-0.5d0,-0.3d0,-0.2d0,-0.1d0,0.d0,0.1d0,0.2d0,0.3d0/
  data ggr /0.d0,0.5d0,1.d0,1.5d0,2.d0,2.5d0,3.d0,3.5d0,4.d0,4.5d0,5.d0/
  data tgr /35.d0,37.5d0,40.d0,42.5d0,45.d0,47.5d0,50.d0,52.5d0,55.d0,57.5d0,60.d0,62.5d0,  &
       65.d0,67.5d0,70.d0,72.5d0,75.d0,77.5d0,80.d0,82.5d0,85.d0,87.5d0,90.d0,92.5d0,95.d0,97.5d0,  &
       100.d0,105.d0,110.d0,115.d0,120.d0,125.d0,130.d0,140.d0,150.d0,160.d0,170.d0,180.d0,190.d0,  &
       200.d0,210.d0,220.d0,230.d0,240.d0,250.d0,260.d0,270.d0,280.d0,290.d0,300.d0,  &
       310.d0,320.d0,330.d0,340.d0,350.d0,375.d0,400.d0,425.d0,450.d0,475.d0,500.d0/
  
end module ubvdata
!***********************************************************************************************************************************


!***********************************************************************************************************************************
!> \brief  Contains the 'constants' used in the evTools package

module constants
  use kinds, only: double
  implicit none
  save
  private :: double
  
  integer :: screen_dpi,screen_size_h,screen_size_v, colours(29),ncolours
  real :: scrsz,scrrat
  character :: libdir*(99)
  logical :: student_mode, white_bg
  
end module constants
!***********************************************************************************************************************************


!***********************************************************************************************************************************
!> \brief  Define the 'constants' in the evTools package

subroutine setconstants()
  use SUFR_constants, only: set_SUFR_constants
  
  use SUFR_constants, only: homedir
  use constants, only: screen_dpi,screen_size_h,screen_size_v,white_bg, colours,ncolours, libdir
  
  implicit none
  
  call set_SUFR_constants()  ! Set libSUFR constants
  
  ! Default:
  screen_size_h = 1000   ! Horizontal screen size (pixels)
  screen_size_v = 700    ! Vertical screen size (pixels)
  
  screen_dpi = 96        ! Screen resolution:  96 is common on PCs, 72 on Macs (still?)
  white_bg = .true.      ! F: black background on screen, T: white
  
  write(libdir,'(A)') trim(homedir)//'/usr/lib'       ! Default lib dir, may be overwritten by settings file
  
  
  ! Line colours:
  ncolours = 13                                            ! Number of (line) colours used to distinguish tracks.  Default: 13
  colours(1:ncolours) = (/2,4,5,6,3,8,9,10,11,12,13,7,1/)  ! Use green late (avoid comparison with red) and black as last resort
  
end subroutine setconstants
!***********************************************************************************************************************************








!***********************************************************************************************************************************
!> \brief  Computes values of Mv, U-B, B-V and V-I for a star with given log L, log T, mass and log(Z/0.02)
!!
!! \param  logl   10-log of the stellar luminosity/Lo
!! \param  logt   10-log of the effective temperature/K
!! \param  mass   stellar mass (Mo)
!! \param  logz   10-log of Z/0.02
!! \retval mbol   Bolometric magnitude
!! \retval bolc   Bolometric correction
!! \retval mv     Visual absolute magnitude
!! \retval uminb  Colour U-B
!! \retval bminv  Colour B-V
!! \retval vminr  Colour V-I
!! \retval rmini  Colour R-I
!!
!! \see http://vizier.cfa.harvard.edu/viz-bin/ftp-index?/ftp/cats/J/MNRAS/298/525/evgrid
!! 
!! Needs one of:
!! - UBVRI.Kur:  table of synthetic BC and UBVRI colours, from Kurucz model atmospheres (1992, IAU Symp 149, p.225)
!! - UBVRI.LBC:  empirically corrected version of the above, from Lejeune, Cuisinier & Buser (1997, A&AS 125, 229)

subroutine lt2ubv(logl,logt,mass,logz,  mbol,bolc,mv,uminb,bminv,vminr,rmini)
  use kinds, only: double
  use ubvdata, only: ntgr,nggr,nzgr, tgr,ggr,zgr, ubv
  
  implicit none
  real(double), intent(in) :: logl,logt,mass,logz
  real(double), intent(out) :: mbol,bolc,mv,uminb,bminv,vminr,rmini
  
  real(double), parameter :: gconst=-10.6071d0
  integer :: k,ig,it,iz,find_index
  real(double) :: cm(5),ltgr(ntgr)
  real(double) :: logm,logg,dg1,dg2,dt1,dt2,dz1,dz2
  external find_index
  
  logm = log10(mass)
  logg = logm + 4*logt - logl + gconst
  ltgr = log10(tgr*100)
  
  ! Find indices of log Z, log g and log T to interpolate between.
  ! don't allow extrapolation outside log Z and log g grid.
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
  mbol = 4.741 - 2.5*logl  ! AF: 4.74 = -2.5*log10(lsun) + 2.5*log10(4*pi*(10*pc)**2) - 11.49  !(Verbunt, p.36 -> cgs)
  bolc = cm(1)
  mv = mbol - bolc
  uminb = cm(2)
  bminv = cm(3)
  vminr = cm(4)
  rmini = cm(5)
  
end subroutine lt2ubv
!***********************************************************************************************************************************


!***********************************************************************************************************************************
!> Convert spectral type and luminosity class to L, Teff using De Jager & Nieuwenhuijzen 1987
!!
!! \param  sptyp  Spectral type (0.0-8.5 for O0-M10)
!! \param  lumcl  Lumonosity class (0.0-5.0 for Ia+-V)
!! \retval lum    Luminosity (Lo)
!! \retval teff   Effective temperature (K)
!!
!! \see 1987A&A...177..217D

subroutine num_sp_type_2_lt(sptyp,lumcl, lum,teff)
  use kinds, only: double
  implicit none
  real(double), intent(in) :: sptyp, lumcl
  real(double), intent(out) :: lum, teff
  integer :: i,j ,n
  real(double) :: st,lc, acst,aclc, logl,logt, fac
  real(double) :: cij(0:8,0:8),dij(0:8,0:8)
  
  cij = 0.d0
  dij = 0.d0
  
  ! 20 coefficients: - up to 50% difference in logL,T compared to the 40-coefficients version!
  if(1.eq.2) then
     ! L:                0           1           2           3          4
     cij(0,0:5) = (/ 3.82573d0, -2.13868d0, -0.46357d0,  0.02076d0, -0.11937d0,  0.d0/)  ! c(0,5) = 0?
     cij(1,0:4) = (/-1.55607d0, -1.89216d0, -0.96916d0, -0.08869d0, -0.20423d0/)
     cij(2,0:3) = (/ 1.05165d0,  0.42330d0, -0.94379d0, -0.07438d0/)
     cij(3,0:2) = (/-0.01663d0, -0.20024d0, -0.18552d0/)
     cij(4,0:1) = (/-0.07576d0, -0.10934d0/)
     cij(5,0)   =    0.11008d0
     
     ! Teff:             0           1           2           3          4
     dij(0,0:5) = (/ 3.96105d0,  0.03165d0, -0.02963d0,  0.01307d0, -0.01172d0,  0.d0/)  ! d(0,5) = 0?
     dij(1,0:4) = (/-0.62945d0,  0.02596d0, -0.06009d0,  0.01881d0, -0.01121d0/)
     dij(2,0:3) = (/ 0.14370d0, -0.00977d0, -0.03265d0,  0.01649d0/)
     dij(3,0:2) = (/ 0.00791d0,  0.00076d0, -0.03006d0/)
     dij(4,0:1) = (/ 0.00723d0, -0.02621d0/)
     dij(5,0)   =    0.02755d0
  end if
  
  
  ! 40 coefficients:
  if(2.eq.2) then
     ! L:
     cij(0,0) =  3.948671d0
     cij(1,0) = -1.433483d0
     cij(0,1) = -2.393305d0
     cij(2,0) =  1.179913d0
     cij(1,1) = -2.438970d0
     cij(0,2) = -2.475556d-1
     cij(3,0) = -8.768061d-3
     cij(2,1) =  1.169188d-1
     cij(1,2) = -5.098354d-1
     cij(0,3) = -6.587491d-2
     cij(4,0) = -5.824052d-2
     cij(3,1) = -3.984403d-1
     cij(2,2) = -7.131113d-1
     cij(1,3) = -2.360943d-1
     cij(0,4) = -1.119168d-1
     cij(5,0) =  6.307021d-2
     cij(4,1) = -1.353292d-1
     cij(3,2) = -5.802147d-2
     cij(2,3) = -9.808638d-2
     cij(1,4) = -1.691771d-1
     
     cij(0,5) =  4.918448d-2             
     cij(6,0) =  8.230225d-2
     cij(5,1) = -3.726583d-2
     cij(4,2) = -3.464800d-2
     cij(3,3) =  2.901596d-2
     cij(2,4) =  2.335186d-2
     cij(1,5) =  6.984993d-2
     cij(0,6) =  1.153887d-2
     cij(7,0) = -1.847738d-2
     cij(6,1) =  6.434137d-2
     cij(5,2) = -6.704941d-2
     cij(4,3) =  1.873843d-1
     cij(3,4) = -1.422689d-2
     cij(2,5) =  9.555925d-2
     cij(1,6) = -4.736092d-2
     cij(0,7) =  1.569149d-2
     cij(8,0) =  4.419809d-2
     cij(7,1) = -6.080426d-2
     cij(6,2) = -1.009405d-1
     cij(5,3) =  9.523569d-2
     
     ! Teff:
     dij(0,0) =  3.952326d0
     dij(1,0) = -6.556686d-1
     dij(0,1) =  3.923369d-2
     dij(2,0) =  1.286346d-1
     dij(1,1) =  3.028204d-2
     dij(0,2) = -2.981806d-2
     dij(3,0) = -1.610569d-2
     dij(2,1) =  1.983002d-2
     dij(1,2) = -4.934013d-2
     dij(0,3) =  1.668647d-2
     dij(4,0) =  2.798024d-3
     dij(3,1) =  5.321887d-3
     dij(2,2) = -3.555299d-2
     dij(1,3) =  1.238407d-2
     dij(0,4) = -4.454756d-3
     dij(5,0) =  1.523164d-2
     dij(4,1) = -1.072416d-2
     dij(3,2) = -2.706478d-2
     dij(2,3) =  2.142100d-2
     dij(1,4) = -1.192406d-2
     
     dij(0,5) =  4.519005d-3
     dij(6,0) =  4.123494d-3
     dij(5,1) = -1.875065d-2
     dij(4,2) = -3.459662d-3
     dij(3,3) = -1.057953d-2
     dij(2,4) =  4.964606d-3
     dij(1,5) = -5.835454d-3
     dij(0,6) = -5.407326d-3
     dij(7,0) = -4.601968d-3
     dij(6,1) = -4.604300d-5
     dij(5,2) =  4.669335d-3
     dij(4,3) =  9.053883d-3
     dij(3,4) = -7.707991d-3
     dij(2,5) = -2.590151d-3
     dij(1,6) = -1.934067d-2
     dij(0,7) =  2.335943d-3
     dij(8,0) =  1.603422d-2
     dij(7,1) = -2.611312d-2
     dij(6,2) =  4.514187d-3
     dij(5,3) = -5.585875d-3
  end if
  
  
  st = sptyp/4.25d0 - 1.d0  ! s: 0.0 - 8.5  ->  -1 - +1
  lc = lumcl/2.5d0  - 1.d0  ! b: 0.0 - 5.0  ->  -1 - +1
  
  acst = acos(st)  ! s
  aclc = acos(lc)  ! b
  
  logl = 0.d0
  logt = 0.d0
  do n=0,8 !5
     do i=0,n
        j = n-i
        fac  = cos(dble(i)*acst) * cos(dble(j)*aclc)  ! Eq.2a/b
        logl = logl + cij(i,j) * fac  ! Eq.2a
        logt = logt + dij(i,j) * fac  ! Eq.2b
     end do
  end do
  
  lum  = 10.d0**logl
  teff = 10.d0**logt
  
end subroutine num_sp_type_2_lt
!***********************************************************************************************************************************










!***********************************************************************************************************************************
!> \brief  Determine the operating system type: 1-Linux, 2-MacOSX

function getos()
  use SUFR_constants, only: homedir
  implicit none
  integer :: getos
  character :: ostype*(25)
  
  call system('uname > '//trim(homedir)//'/uname.tmp')  ! This gives Linux or Darwin
  open(16,file=trim(homedir)//'/uname.tmp', status='old', form='formatted')
  read(16,'(A)')ostype
  close(16, status = 'delete')
  getos = 1 !Linux
  if(ostype(1:5).eq.'Darwi') getos = 2 !MacOSX
  
end function getos
!***********************************************************************************************************************************


!***********************************************************************************************************************************
!> \brief  Find a file that matches match in the current directory
!!
!! \param match      Match string
!! \retval findfile  File name (if found)

function findfile(match)
  use SUFR_constants, only: homedir
  implicit none
  character, intent(in) :: match*(*)
  integer, parameter :: maxfile=1000
  integer :: i,k,fnum
  character :: names(maxfile)*(99),findfile*(99),fname*(99),tempfile*(99)
  
  if(len_trim(homedir).le.0.or.len_trim(homedir).ge.99) then
     write(0,'(/,A,/)')'  Findfile:  ERROR:  variable homedir not defined (forgot to call setconstants?), quitting.'
     stop
  end if
  
  tempfile = trim(homedir)//'/.findfile.tmp'
  ! Shell command to list all the files with the search string and pipe them to a temporary file:
  call system('ls '//trim(match)//' 1> '//trim(tempfile)//' 2> /dev/null')  
  
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
!***********************************************************************************************************************************



!***********************************************************************************************************************************
!> \brief  Find files that match match in the current directory
!!
!! \param match    Search string to match
!! \param nff      Maximum number of files to return
!! \param all      0-select manually from list, 1-always return all files in list
!! \retval fnames  Array that contains the files found; make sure it has the same length as the array in the calling programme
!! \retval nf      The actual number of files returned in fnames ( = min(number found, nff))

subroutine findfiles(match,nff,all, fnames,nf)  
  use SUFR_constants, only: homedir
  implicit none
  character, intent(in) :: match*(*)
  integer, intent(in) :: nff,all
  character, intent(out) :: fnames(nff)*(99)
  integer, intent(out) :: nf
  integer :: i,j,k,fnum
  character :: names(nff)*(99),tempfile*(99)
  
  if(len_trim(homedir).eq.99) then
     write(0,'(/,A,/)')'  Findfiles:  ERROR:  variable homedir not defined (forgot to call setconstants?), quitting.'
     stop
  end if
  
  tempfile = trim(homedir)//'/.findfile.tmp'
  ! Shell command to list all the files with the search string and pipe them to a temporary file:
  call system('ls '//trim(match)//' > '//trim(tempfile))
  
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
!***********************************************************************************************************************************






!***********************************************************************************************************************************
!> \brief  Swap two real numbers
!!
!! \param x  Real number 1
!! \param y  Real number 2

subroutine rswap(x,y)
  implicit none
  real, intent(inout) :: x,y
  real :: z
  
  z = x
  x = y
  y = z
  
end subroutine rswap
!***********************************************************************************************************************************





!***********************************************************************************************************************************
!> \brief  Finds index of value v in monotonously increasing or decreasing array arr of length narr
!!
!! \param v     Value to match to arr (double)
!! \param arr   Array to match v to (double)
!! \param narr  Size of arr (integer)
!! \retval find_index  Index for value v in array arr (integer)

function find_index(v,arr,narr)
  use kinds
  implicit none
  integer, intent(in) :: narr
  real(double), intent(in) :: v,arr(narr)
  integer :: find_index,i,iLow,iHigh
  real(double) :: range
  
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
!***********************************************************************************************************************************




!***********************************************************************************************************************************
!> \brief  Locate the index of array arr such that v lies between arr(i) and arr(i+1)
!!
!! \param arr   monotonic array (double)
!! \param narr  length of arr (integer)
!! \param v     value to look for (double)
!! \retval i    returned index, such that v is between arr(i) and arr(i+1).  If i=0 or narr, v is out of range

subroutine locate(arr,narr,v, i)
  use kinds, only: double
  
  implicit none
  integer, intent(in) :: narr
  real(double), intent(in) :: v,arr(narr)
  integer, intent(out) :: i
  integer :: iLow,iMid,iHigh
  
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
!***********************************************************************************************************************************


!***********************************************************************************************************************************
!> \brief  Single-precision wrapper for of locate()
!!
!! \param rarr  monotonic array
!! \param narr  length of rarr
!! \param rv    value to look for
!! \retval i    returned index, such that v is between arr(i) and arr(i+1).  If i=0 or narr, v is out of range

subroutine locater(rarr,narr,rv,i)
  use kinds, only: double
  
  implicit none
  integer, intent(in) :: narr
  real, intent(in) :: rarr(narr), rv
  integer, intent(out) :: i
  real(double) :: dv,darr(narr)
  
  darr = dble(rarr)
  dv  = dble(rv)
  call locate(darr,narr,dv,i)  ! i will be returned to the calling routine
  
end subroutine locater
!***********************************************************************************************************************************










!***********************************************************************************************************************************
!> \brief  Convert orbital separation to orbital angular momentum
!!
!! \param  m1    Mass 1 (Mo)
!! \param  m2    Mass 2 (Mo)
!! \param  a     Orbital separation (Ro)
!! \retval a2j   Orbital angular momentum (cgs)

function a2j(m1,m2,a)
  use kinds, only: double
  use SUFR_constants, only: pc_g,msun,rsun
  
  implicit none
  real(double), intent(in) :: m1,m2,a
  real(double) :: a2j
  
  a2j = m1*m2*sqrt(pc_g*a*rsun/(m1+m2)*msun**3)

end function a2j
!***********************************************************************************************************************************


!***********************************************************************************************************************************
!> \brief  Convert orbital angular momentum to orbital separation
!!
!! \param  m1    Mass 1 (Mo)
!! \param  m2    Mass 2 (Mo)
!! \param  j     Orbital angular momentum (cgs)
!! \retval j2a   Orbital separation (Ro)

function j2a(m1,m2,j)
  use kinds, only: double
  use SUFR_constants, only: pc_g,msun,rsun
  
  implicit none
  real(double), intent(in) :: m1,m2,j
  real(double) :: j2a
  
  j2a = (j/(m1*m2))**2 * (m1+m2)/(pc_g*msun**3)/rsun

end function j2a
!***********************************************************************************************************************************



!***********************************************************************************************************************************
!> \brief  Convert orbital period to orbital angular momentum, all in cgs units
!!
!! \param  m1    Mass 1 (g)
!! \param  m2    Mass 2 (g)
!! \param  p     Orbital period (s)
!! \retval p2j   Orbital angular momentum (cgs)

function p2j(m1,m2,p)
  use kinds, only: double
  use SUFR_constants, only: pc_g
  
  implicit none
  real(double), intent(in) :: m1,m2,p
  real(double) :: p2j,a
  
  call p2a(m1+m2,p,a)
  p2j = m1*m2*sqrt(pc_g*a/(m1+m2))
  
end function p2j
!***********************************************************************************************************************************


!***********************************************************************************************************************************
!> \brief  Convert orbital angular momentum to orbital period, all in cgs units
!!
!! \param  m1   Mass 1 (g)
!! \param  m2   Mass 2 (g)
!! \param  j    Orbital angular momentum (cgs)
!! \retval j2p  Orbital period (s)

function j2p(m1,m2,j)
  use kinds, only: double
  use SUFR_constants, only: pc_g
  
  implicit none
  real(double), intent(in) :: m1,m2,j
  real(double) :: j2p,a
  
  a = (j/(m1*m2))**2 * (m1+m2)/pc_g
  call a2p(m1+m2,a,j2p)
  
end function j2p
!***********************************************************************************************************************************



!***********************************************************************************************************************************
!> \brief  Convert orbital period to orbital separation, in cgs units
!!
!! \param mtot  Total mass of the binary (g)
!! \param p     Binary period (s)
!! \retval a    Binary orbital separation (cm)

subroutine p2a(mtot,p,a)
  use kinds, only: double
  use SUFR_constants, only: pc_g,pi,c3rd
  
  implicit none
  real(double), intent(in) :: mtot,p
  real(double), intent(out) :: a
  
  a = (pc_g*mtot/(4*pi**2))**c3rd * p**(2*c3rd)
  
end subroutine p2a
!***********************************************************************************************************************************


!***********************************************************************************************************************************
!> \brief  Convert orbital separation to orbital period, in cgs units
!!
!! \param  mtot  Total mass of the binary (g)
!! \param  a     Binary orbital separation (cm)
!! \retval p     Binary period (s)

subroutine a2p(mtot,a,p)
  use kinds, only: double
  use SUFR_constants, only: pc_g,pi
  
  implicit none
  real(double), intent(in) :: mtot,a
  real(double), intent(out) :: p
  
  p = (4*pi**2/(pc_g*mtot))**0.5d0*a**1.5d0
  
end subroutine a2p
!***********************************************************************************************************************************


!***********************************************************************************************************************************
!> \brief  Compute Roche-lobe radius, using Eggleton (1983)
!!
!! \param  m1    Mass 1 (arbitrary unit)
!! \param  m2    Mass 2 (same unit as m1)
!! \param  a     Orbital separation (arbitrary unit)
!! \retval a2rl  Roche-lobe radius of star 1 (same unit as a)

function a2rl(m1,m2,a)
  use kinds, only: double
  use SUFR_constants, only: c3rd
  
  implicit none
  real(double), intent(in) :: m1,m2,a
  real(double) :: a2rl,q
  
  q = m1/m2
  a2rl = a / (0.6d0*q**(2*c3rd) + log(1.d0 + q**c3rd)) * (0.49d0*q**(2*c3rd))
  
end function a2rl
!***********************************************************************************************************************************



!***********************************************************************************************************************************
!> \brief  Convert Roche-lobe radius to orbital separation, using Eggleton (1983)
!!
!! \param  m1    Mass 1 (arbitrary unit)
!! \param  m2    Mass 2 (same unit as m1)
!! \param  rl1   Roche-lobe radius of star 1 (arbitrary unit)
!! \retval rl2a  Orbital separation (same unit as rl1)

function rl2a(m1,m2,rl1)
  use kinds, only: double
  use SUFR_constants, only: c3rd
  
  implicit none
  real(double), intent(in) :: m1,m2,rl1
  real(double) :: rl2a,q
  
  q = m1/m2
  rl2a = rl1/(0.49d0*q**(2*c3rd)/(0.6d0*q**(2*c3rd) + log(1.d0+q**c3rd)))
  
end function rl2a
!***********************************************************************************************************************************


!***********************************************************************************************************************************
!> \brief  Convert orbital period to Roche-lobe radius, all in cgs units
!!
!! \param  m1    Mass 1 (g)
!! \param  m2    Mass 2 (g)
!! \param  p     Orbital period (s)
!! \retval p2rl  Roche-lobe radius of star 1 (cm)

function p2rl(m1,m2,p)
  use kinds, only: double
  
  implicit none
  real(double), intent(in) :: m1,m2,p
  real(double) :: p2rl,a,a2rl
  
  call p2a(m1+m2,p,a)
  p2rl = a2rl(m1,m2,a)
  
end function p2rl
!***********************************************************************************************************************************


!***********************************************************************************************************************************
!> \brief  Convert Roche-lobe radius to orbital period, all in cgs units
!!
!! \param  m1    Mass 1 (g)
!! \param  m2    Mass 2 (g)
!! \param  rl1   Roche-lobe radius of star 1 (cm)
!! \retval rl2p  Orbital period (s)

function rl2p(m1,m2,rl1)
  use kinds, only: double
  
  implicit none
  real(double), intent(in) :: m1,m2,rl1
  real(double) :: rl2p,a,rl2a
  
  a = rl2a(m1,m2,rl1)
  call a2p(m1+m2,a,rl2p)
  
end function rl2p
!***********************************************************************************************************************************




!***********************************************************************************************************************************
!> \brief  Print an exit  message and stop the program
!!
!! \param message  Exit message

subroutine quit_program(message)
  implicit none
  character, intent(in) :: message*(*)
  integer :: len
  
  len = len_trim(message)
  if(len.ge.1) write(0,'(/,A)')'  '//trim(message)
  write(0,'(A,/)')'  Aborting...'
  stop
  
end subroutine quit_program
!***********************************************************************************************************************************



!***********************************************************************************************************************************
!> \brief  Bin data in one dimension, by counting data points in each bin
!!
!! \param  n      Size of data array
!! \param  x      Input data array
!! \param  norm   Normalise (1) or not (0)
!! \param  nbin   Number of bins
!! \param  xmin1  Minimum x value (left side of first bin (I/O: set xmin=xmax to auto-determine)
!! \param  xmax1  Maximum x value (right side of last bin (I/O: set xmin=xmax to auto-determine)
!! \retval xbin   Binned data.  The x values are the left side of the bin!
!! \retval ybin   Binned data.
 
subroutine bin_data_1d(n,x,norm,nbin, xmin1,xmax1, xbin,ybin)
  implicit none
  integer, intent(in) :: n,nbin,norm
  real, intent(in) :: x(n)
  real, intent(inout) :: xmin1,xmax1
  real, intent(out) :: xbin(nbin+1),ybin(nbin+1)
  integer :: i,k
  real :: xmin,xmax,dx
  
  xmin = xmin1
  xmax = xmax1
  
  if(abs((xmin-xmax)/(xmax+1.e-30)).lt.1.e-20) then  ! Autodetermine
     xmin = minval(x(1:n))
     xmax = maxval(x(1:n))
     xmin1 = xmin                                    ! And return new values
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
              exit  ! If point i fits in this bin, don't try the others
           end if
        end if
     end do !k (bin)
     !ybintot = ybintot + ybin(k)
  end do
  !if(norm.eq.1) ybin = ybin/(ybintot+1.e-30)
  if(norm.eq.1) ybin = ybin/(sum(ybin)+1.e-30)
  
end subroutine bin_data_1d
!***********************************************************************************************************************************


!***********************************************************************************************************************************
!> \brief  Get time stamp in seconds since 1970-01-01 00:00:00 UTC
!!
!! \param os  Operating system: 1-Linux, 2-BSD/MacOS

function time_stamp(os)
  use kinds, only: double
  
  implicit none
  integer, intent(in) :: os
  real(double) :: time_stamp
  character :: fname*(99)
  
  fname = './.analysemcmc_time_stamp'  !gfortran doesn't want to read from ~ for some reason
  if(os.eq.2) then  ! MacOS
     call system('date +%s >& '//trim(fname)) !%N for fractional seconds doesn't work on MacOS!!! (But it does with GNU date)
  else  ! GNU/Linux, default
     call system('date +%s.%N >& '//trim(fname))
  end if
  
  open(unit=9,status='old',file=trim(fname))
  read(9,*)time_stamp
  close(9)
  call system('rm -f '//trim(fname))
  
end function time_stamp
!***********************************************************************************************************************************



!***********************************************************************************************************************************
!> \brief  Set the title in a Postscript file generated by PGPlot
!!
!! \param PSfile   Name of the PS file
!! \param PStitle  Title for the PS file

subroutine set_PGPS_title(PSfile,PStitle)
  use SUFR_constants, only: username
  implicit none
  character, intent(in) :: PSfile*(*),PStitle*(*)
  
  call system( "sed -i -e 's/Title: PGPLOT PostScript plot/Title: "//trim(PStitle)//"/' "//trim(PSfile) )
  
  call system( "sed -i -e 's/For: "//trim(username)//"/For: AstroFloyd - astrofloyd.org/' "//trim(PSfile))
  
end subroutine set_PGPS_title
!***********************************************************************************************************************************


!***********************************************************************************************************************************
!> \brief  Read/create evTools settings file ~/.evTools

subroutine evTools_settings()
  use SUFR_constants, only: homedir
  use constants, only: libdir,screen_dpi,screen_size_h,screen_size_v,scrrat,scrsz,white_bg
  
  implicit none
  integer :: io,u
  logical :: ex
  character :: filename*(99)
  
  ! Define namelist, file name:
  namelist /screen_settings/ screen_size_h,screen_size_v,screen_dpi,white_bg
  namelist /local_settings/ libdir
  filename = trim(homedir)//'/.evTools'
  inquire(file=trim(filename), exist=ex)
  
  u = 10
  
  if(ex) then
     
     ! Read the settings file:
     open(unit=u,form='formatted',status='old',action='read',position='rewind',file=trim(filename),iostat=io)
     if(io.ne.0) then
        write(0,'(A,/)')'  Error opening settings file '//trim(filename)//' for reading.'
        return
     end if
     read(u, nml=screen_settings, iostat=io)
     close(u)
     
     if(io.eq.0) then
        call pgxy2szrat_screen(screen_size_h,screen_size_v, screen_dpi, scrsz,scrrat)
     else
        write(6,*)
        write(0,'(A)')'  An error occured when reading the settings file '//trim(filename)// &
             ', using default settings.'
        write(6,'(A)')'  The format of your settings file may be outdated.'
        write(6,'(A)')'  Consider renaming the existing file and rerunning this program to generate a new settings file.'
        write(6,*)
     end if
     
  else
     
     write(6,*)
     write(6,'(A)')'############################################################'
     write(6,'(A)')'#                                                          #'
     write(6,'(A)')'#   No evTools settings file found.                        #'
     write(6,'(A)')'#   Creating '//trim(filename)//' with default settings,   #'
     write(6,'(A)')'#   please edit it to set your preferences.                #'
     write(6,'(A)')'#                                                          #'
     write(6,'(A)')'############################################################'
     write(6,*)
     
  end if
  
  ! Write settings file (do this always, to update in case variables are added):
  open(unit=u,form='formatted',status='unknown',action='write',position='rewind',file=trim(filename),iostat=io)
  if(io.ne.0) then
     write(0,'(A,/)')'  Error opening settings file '//trim(filename)//' for writing.'
     return
  end if
  write(u, nml=local_settings, iostat=io)
  write(u, nml=screen_settings, iostat=io)
  close(u)
  if(io.ne.0) write(0,'(A)')'  An error occured when writing the settings file '//trim(filename)
  
  call pgxy2szrat_screen(screen_size_h,screen_size_v, screen_dpi, scrsz,scrrat)
  
end subroutine evTools_settings
!***********************************************************************************************************************************




!***********************************************************************************************************************************
!> \brief  Convert PGPlot x,y dimensions to paper size and ratio for bitmap
!!
!! \param  x      Horizontal plot size in pixels
!! \param  y      Vertical plot size in pixels
!! \retval size   PGPlot plot size
!! \retval ratio  PGPlot plot ratio

subroutine pgxy2szrat_bitmap(x,y, size,ratio)
  implicit none
  integer, intent(in) :: x,y
  real, intent(out) :: size,ratio
  
  size = real(x-1)/85.
  ratio = real(y-1)/real(x-1)
  
end subroutine pgxy2szrat_bitmap
!***********************************************************************************************************************************


!***********************************************************************************************************************************
!> \brief  Convert PGPlot x,y dimensions to paper size and ratio for bitmap
!!
!! \param  size   PGPlot plot size
!! \param  ratio  PGPlot plot ratio
!! \retval x      Horizontal plot size in pixels
!! \retval y      Vertical plot size in pixels

subroutine pgszrat2xy_bitmap(size,ratio,x,y)
  implicit none
  real, intent(in) :: size,ratio
  integer, intent(out) :: x,y
  
  x = nint(size*85) + 1
  y = nint(size*ratio*85) + 1
  
end subroutine pgszrat2xy_bitmap
!***********************************************************************************************************************************



!***********************************************************************************************************************************
!> \brief  Convert x,y screen dimensions to PGPlot paper size and ratio for a screen
!!
!! \param horiz   Horizontal screen size (pixels)
!! \param vert    Vertical screen size (pixels)
!! \param dpi     Screen resolution in dots per inch
!! \retval size   PGPlot screen size
!! \retval ratio  PGPlot screen ratio

subroutine pgxy2szrat_screen(horiz,vert, dpi, size,ratio)
  implicit none
  integer, intent(in) :: horiz,vert,dpi
  real, intent(out) :: size,ratio
  
  size  = real(horiz-48) / real(dpi)
  ratio = real(vert -48) / real(horiz-48)
  
end subroutine pgxy2szrat_screen
!***********************************************************************************************************************************


!***********************************************************************************************************************************
!> \brief  Convert PGPlot paper size and ratio to screen dimensions
!!
!! \param size    PGPlot screen size
!! \param ratio   PGPlot screen ratio
!! \param dpi     Screen resolution in dots per inch
!! \retval horiz  Horizontal screen size (pixels)
!! \retval vert   Vertical screen size (pixels)

subroutine pgszrat2xy_screen(size,ratio, dpi, horiz,vert)
  implicit none
  real, intent(in) :: size,ratio
  integer, intent(in) :: dpi
  integer, intent(out) :: horiz,vert
  
  horiz = nint(dpi*size)       + 48
  vert  = nint(dpi*size*ratio) + 48
  
end subroutine pgszrat2xy_screen
!***********************************************************************************************************************************


