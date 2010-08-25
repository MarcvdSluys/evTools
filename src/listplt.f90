!> \file listplt.f90  Reads the data contained in star.plt1,2 and prints it to screen, taken from plotplt
!

! Copyright 2002-2010 AstroFloyd - astrofloyd.org
! 
! 
! This file is part of the evTools package.
! 
! This is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by
! the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
! 
! This software is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details.
! 
! You should have received a copy of the GNU General Public License along with this code.  
! If not, see <http://www.gnu.org/licenses/>.


program listplt
  use kinds
  use constants
  use ubvdata
  
  implicit none
  integer, parameter :: nn=30000,nnn=200,nc=81
  real(double), parameter :: c8th=0.125_dbl
  real(double) :: dat(nnn,nn),var(nn),dpdj(nn),d(nn),a(nnn)
  real(double) :: c82(nn),c85a,c85b,c92(nn),mbol,bc
  
  integer :: i,io,j,n,ans,ncols,narg,command_argument_count
  character :: findfile*99, fname*99,labels(nnn)*99,tmpstr*10
  
  call setconstants()
  
  !Read atmosphere-model data
  open(unit=10, file=trim(libdir)//'/UBVRI.Kur',status='old',action='read',iostat=io)
  if(io.eq.0) then
     read(10,*)tmpstr
     read(10,*)tmpstr
     read(10,*)ubv
     close(10)
  else
     write(6,'(A)')" Warning:  I can't find the file "//trim(libdir)//"/UBVRI.Kur, so I can't calculate colours and magnitudes..."
  end if
  
  labels = ''
  labels(1)  = 'Model'
  labels(2)  = 't (yr)'
  labels(3)  = 'dt (yr)'
  labels(4)  = 'M (Mo)'
  labels(5)  = 'MHe (Mo)'
  labels(6)  = 'MCO (Mo)'
  labels(7)  = 'MONe (Mo)'
  labels(8)  = 'R (Ro)'
  labels(9)  = 'L (Lo)'
  labels(10) = 'Teff (K)'
  labels(11) = 'Tc (K)'
  labels(12) = 'Tmax (K)'
  labels(13) = 'Rhoc'
  labels(14) = 'Rho_Tmx'
  labels(15) = 'Ubind'
  labels(16) = 'LH (Lo)'
  labels(17) = 'LHe (Lo)'
  labels(18) = 'LC (Lo)'
  labels(19) = 'Lnu (Lo)'
  labels(20) = 'Lth (Lo)'
  labels(21) = 'Prot (d)'
  labels(22) = 'K2'
  labels(23) = 'Rcz'
  labels(24) = 'dRcz'
  labels(25) = 'tet (d)'
  labels(26) = 'Ralfven'
  labels(27) = 'Bp'
  labels(28) = 'Porb (d)'
  labels(29) = 'FLR'
  labels(30) = 'F1'
  labels(31) = 'dM/dt'
  labels(32) = 'dMwind/dt'
  labels(33) = 'dMmt/dt'
  labels(34) = 'Horb'
  labels(35) = 'dHorb/dt'
  labels(36) = 'dHgw/dt'
  labels(37) = 'dHwml/dt'
  labels(38) = 'dHs-o/dt'
  labels(39) = 'dHmtr/dt'
  labels(40) = 'Mcomp'
  labels(41) = 'e'
  labels(42) = 'H,surf'
  labels(43) = 'He,surf'
  labels(44) = 'C,surf'
  labels(45) = 'N,surf'
  labels(46) = 'O,surf'
  labels(47) = 'Ne,surf'
  labels(48) = 'Mg,surf'
  labels(49) = 'H,Tmax'
  labels(50) = 'He,Tmax'
  labels(51) = 'C,Tmax'
  labels(52) = 'N,Tmax'
  labels(53) = 'O,Tmax'
  labels(54) = 'Ne,Tmax'
  labels(55) = 'Mg,Tmax'
  labels(56) = 'H,centr'
  labels(57) = 'He,centr'
  labels(58) = 'C,centr'
  labels(59) = 'N,centr'
  labels(60) = 'O,centr'
  labels(61) = 'Ne,centr'
  labels(62) = 'Mg,centr'
  labels(63) = 'Menv (Mo)'
  labels(64) = 'Xf'
  
  labels(81) = 'Qconv'
  labels(82) = 'PGW,max (d)'
  labels(83) = 'Menv (Mo)'
  labels(84) = 'Xf'
  labels(85) = 'R/(dR/dt)(yr)'
  labels(86) = 'Rossby number'
  labels(87) = 'Prot,crit (d)'
  labels(88) = 'MB_Sills'
  labels(89) = 't_et,int/t_et,anal.'
  labels(90) = 't-t_0 (yr)'
  labels(91) = '(Ne/O)_c/(Ne/O)_s'
  labels(92) = 'P_GW,max (d)'
  labels(93) = 'R_rl (R_\(2281))'
  labels(94) = 'X_f'
  labels(95) = 'M.I. (M_\(2281) R_\(2281)^2)'
  labels(96) = 'J_spin (10^50 g cm^2 s^-1)'
  labels(97) = '\gr_avg (g cm^-3)'
  labels(98) = 'Z_surf'
  
  labels(101) = 'V'
  labels(102) = 'U-B'
  labels(103) = 'B-V'
  labels(104) = 'V-R'
  labels(105) = 'R-I'
  labels(106) = 'U-V'
  labels(107) = 'V-I'
  
  
  
  
  !************************************************************************
  !***   READ COMMAND LINE VARIABLES
  !************************************************************************
  
  narg = command_argument_count()
  if(narg.eq.1) then
     call get_command_argument(1,fname)
  else
     write(6,'(A)')'listplt: lists the contents of a plt-file to screen'
     write(6,'(A)')'    syntax:  listplt <filename>'
     write(6,*)''
     write(6,'(A)')"I'll look in the current directory for a *.plt file..."
     fname=findfile('*.plt*')
  end if
  
  
  
  
  
  write(6,*)''
  write(6,'(A)')'Reading file '//trim(fname)
  
  dat = 0.d0
  open(unit=10,form='formatted',status='old',file=trim(fname))
  rewind 10
  read(10,*)ncols
  write(6,'(A,I4,A)')'  Reading',ncols,' columns of data'
  if(ncols.ne.nc) write(6,'(A,I4)')'WARNING: Number of colums in this file does not match that of the program:',nc
  do j=1,nn
     !read(10,'(F6.0,E17.9,E14.6,11F9.5,7E12.4,3F9.5,16E12.4,F8.4,21E13.5,12F9.5,6F9.5,E14.6)',err=12,end=11) (dat(i,j),i=1,nc)
     read(10,*,err=12,end=11) (dat(i,j),i=1,nc)
  end do
  write(6,'(A)')'  End of file reached, arrays too small!'
  close(10)
  goto 15
  
11 write(6,'(A,I6,A)')'  End of the file reached,',j-1,' lines read.'
  close(10)
  goto 15
  
12 write(6,'(A,I6)')'  Error reading file, line ',j
  close(10)
  if(j.lt.3) goto 19
  print*,"  I'll skip the rest of the file and use the first part."
15 continue
  write(6,*)''
  
  n = j-1   !Number of models in the file
  
  
  
  goto 29
19 write(6,'(A)')'Trying for the new output format...'
  dat = 0.d0
  open(unit=20,form='formatted',status='old',file=trim(fname))
  rewind 20
  read(20,*)ncols
  write(6,'(A,I6,A)')'  Reading',ncols,' columns of data'
  if(ncols.ne.nc) write(6,'(A,I4)')'WARNING: Number of colums in this file does not match that of the program:',nc
  do j=1,nn
     !read(20,'(F6.0,E17.9,E14.6,12E13.5,7E12.4,3E13.5,17E12.4,39E13.5,E14.6)',err=22,end=21) (dat(i,j),i=1,nc)
     read(20,*,err=22,end=21) (dat(i,j),i=1,nc)
  end do
  write(6,'(A)')'  End of file reached, arrays too small!'
  close(20)
  goto 25
  
21 write(6,'(A,I6,A)')'  End of the file reached,',j-1,' lines read.'
  close(20)
  goto 25
  
22 write(6,'(A,I6)')'  Error reading file, aborting at line ',j
  print*,dat(1,1:10)
  if(j.lt.3) goto 9999
  write(6,'(A)')"  I'll skip the rest of the file and use the first part."
  close(20)
25 continue
  write(6,*)''
  
  n = j-1   !Number of models in the file
  
  
  
  
  
  write(6,*)''
29 continue
  !Log or de-log some variables
  do i=4,nnn !skip t,dt
     if(dat(i,1).eq.0.) dat(i,1) = dat(i,2)
  end do
  
  dat(8,1:n)  = 10**dat(8,1:n)
  dat(9,1:n)  = 10**dat(9,1:n)
  dat(10,1:n) = 10**dat(10,1:n)
  dat(11,1:n) = 10**dat(11,1:n)
  dat(12,1:n) = 10**dat(12,1:n)
  dat(13,1:n) = 10**dat(13,1:n)
  dat(14,1:n) = 10**dat(14,1:n)
  dat(15,1:n) = dat(15,1:n)*m0  !One would like to print Ubind in erg, not erg/m0
  
  
  
  
  !****** CALCULATE SOME VARIABLES THAT ARE NOT IN THE FILE ***************
  
  !Calculate actual magnetic braking, according to Rappaport, Joss, Verbunt 1983
  dat(38,1:n)  = 3.8e-30*dat(4,1:n)*m0*(dat(8,1:n)*r0)**4 * (2*pi/(dat(21,1:n)*day))**3/1.d50
  do i=1,n
     if(dat(81,i).lt.0.02) dat(38,i) = dat(38,i)*exp(1.d0-2.d-2/dat(81,i))
  end do !i
  
  
  !dat(38,1:n) = dat(88,1:n)  !Take Sills in stead of Rappaport MB
  dat(37,1:n) = dat(88,1:n)  !Take Sills MB in stead of Wind AML
  
  !dP/dJ = 3/(m1m2)(2piP^2(m1+m2)/G^2)^1/3:
  dpdj(1:n) = 3.d0/(dat(4,1:n)*dat(40,1:n)*m0*m0) * (tpi*(dat(28,1:n)*day)**2*(dat(4,1:n)+dat(40,1:n))*m0/(g*g))**c3rd
  
  !dJ/dt needed to obtain the same effect on Porb as from (conservative) mass transfer, in case of no wind: use dat(31) instead of 
  !dat(33):
  dat(39,1:n) = (dat(4,1:n)-dat(40,1:n))*m0*dat(31,1:n)*m0/yr*(g*g*dat(28,1:n)*day/(tpi*(dat(4,1:n)+dat(40,1:n))*m0))**c3rd/1.d50
  
  
  dat(63,1:n) = dat(4,1:n) - dat(5,1:n) 
  
  
  dat(75,1:n) = g*dat(4,1:n)**2*m0*m0/(dat(8,1:n)*r0*dat(9,1:n)*l0)/yr          !KH timescale
  dat(76,1:n) = dat(34,1:n)/max(dat(36,1:n)*yr,1.d-30)          !Gravitational waves
  dat(77,1:n) = dat(34,1:n)/max(abs(dat(38,1:n))*yr,1.d-30)             !Magnetic braking (Actually SO-coupling!)
  dat(78,1:n) = dat(4,1:n)/max(abs(dat(33,1:n)),1.d-30)         !Mass transfer
  dat(79,1:n) = dat(4,1:n)*m0/1.9891/(dat(9,1:n)*l0)*4.e10              !Nuclear evolution timescale
  dat(80,1:n) = dat(79,1:n)
  
  dat(5,1:n) = dat(5,1:n) + 1.d-30
  c82(1:n) = tpi*(256.d0/5.d0)**(3*c8th) * g**(5*c8th)/c**(15*c8th) * (dat(5,1:n)*1.4)**(3*c8th)*m0**(5*c8th)/(dat(5,1:n)+1.4)**c8th
  !Pmax that can still be converged for a WD with the mass of the He core and a NS of 1.4Mo in a time t-t_H due to GWs:
  dat(82,1:n) = ((13.6d9-dat(2,1:n))*yr)**(3*c8th)*c82(1:n)/day
  
  
  dat(83,1:n) = dat(4,1:n) - dat(5,1:n)                                 !H-envelope mass
  dat(84,1:n) = 2*dat(56,1:n) + dat(57,1:n) + 1.                        !Xf := 2Xc + Yc + 1
  
  dat(85,1) = 0.d0
  do i=2,n
     c85a = dabs(dat(8,i)-dat(8,i-1))+1.d-30    !dR
     c85b = dabs(dat(2,i)-dat(2,i-1))+1.d-30    !dt
     dat(85,i) = dat(8,i)/(c85a/c85b)   !R/(dR/dt)
  end do
  
  dat(86,1:n) = dat(21,1:n)/(dat(25,1:n)+1.d-30)  !Rossby number = Prot/Tet
  
  dat(87,1:n) = 2.5d0/13.8d0 * dat(25,1:n)  !Critical Prot (Pc) for saturated MB (=2pi/omega_c): Pc_sun~2.5d, Tet_sun~13.8d
  
  do i=1,n !Saturated MB - Sills et al, 2000ApJ.534.335
     dat(88,i) = 2.7e-3*(2*pi/dat(21,i))*(2*pi/dat(87,i))**2 * dat(8,i)**0.5d0*dat(4,i)**(-0.5d0)
     if(dat(21,i).gt.dat(87,i)) dat(88,i) = 2.7e-3*(2*pi/dat(21,i))**3*dat(8,i)**0.5d0*dat(4,i)**(-0.5d0)
     if(dat(81,i).lt.0.02) dat(88,i) = dat(88,i)*exp(1.d0-2.d-2/dat(81,i)) !Exponential decrease for thin convective envelopes
  end do !i
  !      dat(88,1:n) = log10(dat(88,1:n)/day**3)
  dat(88,1:n) = dat(88,1:n)/day**3
  
  var(1:n) = (1.1487d0*dat(9,1:n)**0.47d0 + 0.1186d0*dat(9,1:n)**0.8d0)/dat(4,1:n)**0.31d0  !~Hyashi track radius
  !Analytic convective turnover timescale (days), adapted from Eggleton's CFUNCS.F:
  dat(89,1:n) = 28.437d0*(dat(8,1:n)**2*dat(4,1:n)/dat(9,1:n))**c3rd * (dat(8,1:n)/var(1:n))**2.7d0
  
  
  dat(90,1:n) = dat(2,1:n) - dat(2,1) !t-t0
  
  dat(91,1:n) = (dat(61,1:n)/dat(60,1:n))/(dat(47,1:n)/dat(46,1:n))   !(Ne/O)cen/(Ne/O)surf
  
  c92(1:n) = 2*pi*(256.d0/5.d0)**(3*c8th) * g**(5*c8th)/c**(15*c8th) *(dat(5,1:n)*1.4)**(3*c8th)*m0**(5*c8th)/(dat(5,1:n)+1.4)**c8th
  !Pmax that can still be converged for a WD with the mass of the He core and a NS of 1.4Mo in a time t-t_H due to GWs:
  dat(92,1:n) = ((13.6d9-dat(2,1:n))*yr)**(3*c8th)*c92(1:n)/day
  
  dat(93,1:n) = dat(8,1:n)/dexp(dat(29,1:n))    
  dat(94,1:n) = 2*dat(56,1:n) + dat(57,1:n) + 1.                                !Xf := 2Xc + Yc + 1
  !M.I. = k^2*M*R^2 in MoRo^2  (in some models, log(VK2) is listed:
  dat(95,1:n) = 10.d0**dat(22,1:n)*dat(4,1:n)*dat(8,1:n)**2
  dat(96,1:n) = dat(95,1:n)*2*pi/(dat(21,1:n)+1.e-30)*(1.d-50*m0*r0*r0/day)     !Jspin = I*w in 10^50 g cm^2 s^-1
  dat(97,1:n) = dat(4,1:n)*m0/(4*c3rd*pi*(dat(8,1:n)*r0)**3)                !Average Rho
  dat(98,1:n) = 1.d0 - dat(42,1:n)-dat(43,1:n)                              !Z_surf = 1 - X - Y
  
  
  !Colours
  do i=1,n
     call lt2ubv(dlog10(dat(9,i)),dlog10(dat(10,i)),dat(4,i),dlog10(dat(98,i)/2.d-2),mbol,bc,dat(101,i),dat(102,i),dat(103,i), &
          dat(104,i),dat(105,i))
     dat(106,i) = dat(102,i)+dat(103,i)  ! (U-V) = (U-B) + (B-V)
     dat(107,i) = dat(104,i)+dat(105,i)  ! (V-I) = (V-R) + (R-I)
  enddo
  
  
  
  
  
  
  
  
  
  do i=1,n
     d(1:nnn) = dat(1:nnn,i)
     if(mod(i,25).eq.1) then
        write(6,*)''
        write(6,'(A)')' Line   Mdl     t (yr)   M(Mo)   Mhe   Mco   Menv    R (Ro)   L (Lo)    Te (K)   Tc (K)       V    B-V'// &
             '     Xc    Yc   Porb(d)     dM/dt  M2/Mo'
     end if
     write(6,'(I5,I6,ES11.4,F8.3,2F6.3,F7.3,2(1x,2ES9.2),1x,2F7.3,1x,2F6.3,2ES10.2,F7.3)')i,nint(d(1)),d(2),d(4),d(5),d(6),d(63), &
          d(8),d(9),d(10),d(11),d(101),d(103),d(56),d(57),d(28),dabs(d(31)),d(40)
  end do
  
  
61 write(6,*)''
  write(6,'(A)', advance='no')'  Which line would you like to see (0 = quit):  '
  read*,ans
  if(ans.gt.n.or.ans.lt.0) goto 61
  if(ans.eq.0) goto 9999
  
  
  
  
  
  
  
  
  a = dat(:,ans)
  do i=1,2
     write(6,*)''
  end do
  
  do i=1,16
     write(6,'(2x,4(I3,1x,A9,1x,ES15.7,9x))')i,trim(labels(i)),a(i),i+16,trim(labels(i+16)),a(i+16),i+32,trim(labels(i+32)), &
          a(i+32),i+48,trim(labels(i+48)),a(i+48)
  end do
  
  do i=1,2
     write(6,*)''
  end do
  
  do i=1,10
     write(6,'(4(I5,1x,A13,2x,ES15.7,10x))')80+i,trim(labels(80+i)),a(80+i), 90+i,trim(labels(90+i)),a(90+i), 100+i, &
          trim(labels(100+i)),a(100+i)
  end do
  
  do i=1,2
     write(6,*)''
  end do
  
  
  
  
  
  
  
  
  
  
  
  
  goto 9999
  
  
  !*********************************************************************************************************************************
  
  
  
  write(6,*)''
  write(6,'(A)')'Variables:                                    0: Quit    '
  write(6,*)''
  write(6,'(A)')'  1: model      16: Lh         28: Porb      34: Horb    '
  write(6,'(A)')'  2: t          17: Lhe        29: FLR       35: dHorb/dt'
  write(6,'(A)')'  3: dt         18: Lc         30: F1        36: dHgw/dt '
  write(6,'(A)')'  4: M          19: Lnu        31: dM        37: dHwml/dt'
  write(6,'(A)')'  5: Mhe        20: Lth        32: dMwind    38: dHmb/dt '
  write(6,'(A)')'  6: Mco        21: Prot       33: dMmt      39: dHmtr/dt'
  write(6,'(A)')'  7: Mone       22: VK2                      40: Mcomp   '
  write(6,'(A)')'  8: R          23: Rcz                      41: e       '
  write(6,'(A)')'  9: L          24: dRcz                                 '
  write(6,'(A)')' 10: Teff       25: Tet                                  '
  write(6,'(A)')' 11: Tc         26: Ralv                                 '
  write(6,'(A)')' 12: Tmax       27: Bp          H  He   C   N   O  Ne  Mg'
  write(6,'(A)')' 13: Rhoc                Surf  42  43  44  45  46  47  48'
  write(6,'(A)')' 14: RhoTm               Tmax  49  50  51  52  53  54  55'
  write(6,'(A)')' 15: Ubind               Core  56  57  58  59  60  61  62'
  write(6,'(A)')'                                                         ' 
  write(6,'(A)')'                                                         '
  write(6,'(A)')' 81: Qconv          86: Rossby nr                        '  
  write(6,'(A)')' 82: Pgw,max        87: Pcr (MB)                         '  
  write(6,'(A)')' 83: Menv           88: Sills MB                         '
  write(6,'(A)')' 84: Xf             89: Tet: int/anal                    '
  write(6,'(A)')' 85: R/(dR/dt)                                           '
  write(6,'(A)')'                                                         '
  
  
9999 write(6,*)''
  write(6,'(A)')'Program finished'
  write(6,*)''
  write(6,*)''
end program listplt











