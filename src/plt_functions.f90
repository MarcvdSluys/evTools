!> \file plt_functions.f90  Functions and subroutines for plotplt* in the evTools package that need pgplot

! Copyright 2002-2011 AstroFloyd - astrofloyd.org
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
!***  ROUTINES FOR *.PLT? FILES   **************************************************************************************************
!***********************************************************************************************************************************










!***********************************************************************************************************************************
!> \brief Provides the labels for the plot axes of a *.plt? file in plotplt
subroutine getpltlabels(nf,nvar,pglabels,asclabels,defvar)
  implicit none
  integer :: nf,nvar,defvar(0:nvar)
  character :: pglabels(nvar)*(99),asclabels(nvar)*(99)
  
  defvar = 0
  
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
  pglabels(15) = 'E\dbind,env\u (10\u40\d erg)'
  pglabels(16) = 'L\dH\u (L\d\(2281)\u)'
  pglabels(17) = 'L\dHe\u (L\d\(2281)\u)'
  pglabels(18) = 'L\dC\u (L\d\(2281)\u)'
  pglabels(19) = 'L\d\gn\u (L\d\(2281)\u)'
  pglabels(20) = 'L\dth\u (L\d\(2281)\u)'
  
  pglabels(21) = 'P\drot\u (d)'
  pglabels(22) = 'K\u2\d'
  pglabels(23) = 'R\dcz\u'
  pglabels(24) = '\gDR\dcz\u'
  pglabels(25) = 't\det\u (d)'
  pglabels(26) = 'R\dalfven\u'
  pglabels(27) = 'B\dp\u'
  pglabels(28) = 'P\dorb\u (d)'
  pglabels(29) = 'FLR'
  pglabels(30) = 'F1'
  
  pglabels(31) = 'dM/dt (M\d\(2281)\u/yr)'
  pglabels(32) = 'dM\dwind\u/dt (M\d\(2281)\u/yr)'
  pglabels(33) = 'dM\dmt\u/dt (M\d\(2281)\u/yr)'
  pglabels(34) = 'J\dorb\u (10\u50\d g cm\u2\d s\u-1\d)'
  pglabels(35) = 'dJ\dorb\u/dt'
  pglabels(36) = 'dJ\dgw\u/dt'
  pglabels(37) = 'dJ\dwml\u/dt'
  pglabels(38) = 'dJ\ds-o\u/dt'
  pglabels(39) = 'dJ\dmtr\u/dt'
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
  
  defvar(1:62) = 1
  
  
  pglabels(81) = 'Q\dconv\u'
  pglabels(82) = 'P\dc\u (cgs)'
  pglabels(83) = 'P\drot,c\u (s)'
  pglabels(84) = 'E\dbind,env,grav\u (10\u40\d erg)'
  pglabels(85) = 'E\dbind,env,int\u (10\u40\d erg)'
  pglabels(86) = 'E\dbind,env,recom\u (10\u40\d erg)'
  pglabels(87) = 'E\dbind,env,H2ass\u (10\u40\d erg)'
  pglabels(88) = 'S\dc\u (erg g\u-1\d K\u-1\d)'
  pglabels(89) = 'S\dT=1e5K\u (erg g\u-1\d K\u-1\d)'
  pglabels(90) = 'R\dHe\u (R\d\(2281)\u)'
  pglabels(91) = 'R\dCO\u (R\d\(2281)\u)'
  pglabels(92) = 'STRMDL'
  
  defvar(81:92) = 1
  
  
  !Derived variables:
  pglabels(101) = 'V'
  pglabels(102) = 'U-B'
  pglabels(103) = 'B-V'
  pglabels(104) = 'V-R'
  pglabels(105) = 'R-I'
  pglabels(106) = 'U-V'
  pglabels(107) = 'V-I'
  
  pglabels(111) = '\(2137)\denv\u'  !lambda_env
  pglabels(112) = 'q\dcrit\u'       !q_crit; q_1 > q_crit gives dynamical MT (Hurley et al., 2002, Eq.57)
  pglabels(113) = 'M\dcomp,crit\u'  !M2 < M2,crit gives dynamical MT (Hurley et al., 2002, Eq.57)
  pglabels(114) = 'v\drot\u (km/s)' !Rotational velocity
  pglabels(115) = 'R/R\dZAMS\u'     !Radius over ZAMS radius
  
  pglabels(116) = 'M\dHe\u-M\dCO\u (M\d\(2281)\u)'
  pglabels(117) = 'M\denv\u (M\d\(2281)\u)'
  pglabels(118) = 'M\dconv\u (M\d\(2281)\u)'
  pglabels(119) = 'R/(dR/dt) (yr)'
  pglabels(120) = 'Rossby number'
  
  pglabels(121) = 'P\drot,crit\u (d)'
  pglabels(122) = 'MB\dSills\u'
  pglabels(123) = 't\det,int\u/t\det,anal.\u'
  pglabels(124) = 't-t\d0\u (yr)'
  pglabels(125) = '(Ne/O)\dc\u/(Ne/O)\ds\u'
  pglabels(126) = 'P\dGW,max\u (d)'
  pglabels(127) = 'R\drl\u (R\d\(2281)\u)'
  pglabels(128) = 'X\df\u'
  pglabels(129) = 'M.I. (M\d\(2281)\u R\d\(2281)\u\u2\d)'
  pglabels(130) = 'w\dspin\u (s\u-1\d)'
  
  pglabels(131) = '\gr\davg\u (g cm\u-3\d)'
  pglabels(132) = 'Z\dsurf\u'
  pglabels(133) = '(t\df\u - t)  (yr)'
  pglabels(134) = 'P\drot\u/P\dcrit\u'
  pglabels(135) = 'g\dsurf\u (cm s\u-2\d)'
  pglabels(136) = 'dM/dt\dReimers\u (M\d\(2281)\u yr\u-1\d)'
  pglabels(137) = 'dM/dt\dReimers-like\u (M\d\(2281)\u yr\u-1\d)'
  pglabels(138) = 'dM/dt\dReimers-like\u / dM/dt\dReimers\u'
  pglabels(139) = 'M\dZAMS\u - M (M\d\(2281)\u)'
  pglabels(140) = '(M\dZAMS\u - M)/M\dZAMS\u'
  
  pglabels(141) = 'GMM\denv\u/R (10\u40\d erg)'
  pglabels(142) = 'M\dbin\u (M\d\(2281)\u)'
  pglabels(143) = 'a\dorb\u (R\d\(2281)\u)'
  pglabels(144) = 'J\dorb\u (G\u1/2\dM\d\(2281)\u\u3/2\dR\d\(2281)\u\u1/2\d)'
  pglabels(145) = 'J\dspin\u (G\u1/2\dM\d\(2281)\u\u3/2\dR\d\(2281)\u\u1/2\d)'
  pglabels(146) = 'J\dtot\u (G\u1/2\dM\d\(2281)\u\u3/2\dR\d\(2281)\u\u1/2\d)'
  pglabels(147) = 'E\dorb\u (GM\d\(2281)\u\u2\dR\d\(2281)\u\u-1\d)'
  pglabels(148) = 'E\dspin\u (GM\d\(2281)\u\u2\dR\d\(2281)\u\u-1\d)'
  pglabels(149) = 'E\dso\u (GM\d\(2281)\u\u2\dR\d\(2281)\u\u-1\d)'
  pglabels(150) = 'E\dbind\u (GM\d\(2281)\u\u2\dR\d\(2281)\u\u-1\d)'
  pglabels(151) = 'E\dtot\u (GM\d\(2281)\u\u2\dR\d\(2281)\u\u-1\d)'
  
  defvar(101:151) = 1
  
  
  !Special plots:
  defvar(201) = 1  !HRD
  
  
  !These plots can only be made when reading 1 file:
  if(nf.eq.1) then
     defvar(202) = 1  !Convection plot
     
     
     pglabels(211) = '\gt (yr)'
     pglabels(212) = 'L (L\d\(2281)\u)'
     pglabels(213) = 'Surface abundances'
     pglabels(214) = 'T\dmax\u abundances'
     pglabels(215) = 'Core abundances'
     defvar(211:215) = 1
     
     pglabels(221) = 'dJ\dorb\u/dt'
     pglabels(222) = 'dM/dt (M\d\(2281)\u/yr)'
     pglabels(223) = 'dM/dt (M\d\(2281)\u/yr)'
     defvar(221:223) = 1
  end if
  
  



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
  asclabels(34) = 'Jorb'
  asclabels(35) = 'Jorbdt'
  asclabels(36) = 'dJgwdt'
  asclabels(37) = 'dJwmldt'
  asclabels(38) = 'dJsodt'
  asclabels(39) = 'dJmtrdt'
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
  
  asclabels(81) = 'Qconv'
  asclabels(82) = 'Pc'
  asclabels(83) = 'Protc'
  asclabels(84) = 'Ebind_grav'
  asclabels(85) = 'Ebind_int'
  asclabels(86) = 'Ebind_recom'
  asclabels(87) = 'Ebind_H2ass'
  asclabels(88) = 'Sc'
  asclabels(89) = 'S1e5K'
  asclabels(90) = 'Rhe'
  asclabels(91) = 'Rco'
  asclabels(92) = 'STRMDL'
  
  
  asclabels(101) = 'V'
  asclabels(102) = 'U-B'
  asclabels(103) = 'B-V'
  asclabels(104) = 'V-R'
  asclabels(105) = 'R-I'
  asclabels(106) = 'U-V'
  asclabels(107) = 'V-I'
  

  asclabels(111) = 'lambda'  !lambda_env
  asclabels(112) = 'qcr'
  asclabels(113) = 'M2cr'
  asclabels(114) = 'Vrot'
  asclabels(115) = 'RRzams'
  asclabels(116) = 'MHe-MCO'
  asclabels(117) = 'Menv'        !M_env
  asclabels(118) = 'Mconv'
  asclabels(119) = 'tauR'
  asclabels(120) = 'RosbNr'
  
  asclabels(121) = 'Pcr'
  asclabels(122) = 'SillsMB'
  asclabels(123) = 'TetRat'
  asclabels(124) = 't-t0'
  asclabels(125) = 'dNeO'
  asclabels(126) = 'Pgwmax'
  asclabels(127) = 'Rrl'
  asclabels(128) = 'Xf'
  asclabels(129) = 'M.I.'
  asclabels(130) = 'wspin'
  
  asclabels(131) = 'avgdens'
  asclabels(132) = 'Zsurf'
  asclabels(133) = 'tf-t'
  asclabels(134) = 'ProtPcrit'
  asclabels(135) = 'gsurf'
  asclabels(136) = 'dM_Rmr'
  asclabels(137) = 'dM_Rmrlk'
  asclabels(138) = 'dMRmrlk_dMRmr'
  asclabels(139) = 'Mzams-M'
  asclabels(140) = 'Mzams-M_Mzams'
  
  asclabels(141) = 'GMM_R'
  asclabels(142) = 'Mbin'
  asclabels(143) = 'aorb'
  asclabels(144) = 'Jorb'
  asclabels(145) = 'Jspin'
  asclabels(146) = 'Jtot'
  asclabels(147) = 'Eorb'
  asclabels(148) = 'Espin'
  asclabels(149) = 'Eso'
  asclabels(150) = 'Ebind'
  asclabels(151) = 'Etot'
  
  
  
  asclabels(201) = 'HRD'
  asclabels(202) = 'Convection'
  
  asclabels(211) = 'taus'
  asclabels(212) = 'Lums'
  asclabels(213) = 'SurfAbd'
  asclabels(214) = 'TmaxAbd'
  asclabels(215) = 'CoreAbd'
  
  asclabels(221) = 'dJdts'
  asclabels(222) = 'Mdots'
  asclabels(223) = 'Winds'
  !asclabels(22) = ''
  
  
  
  !These were available in plotpltn
  !asclabels() = 'k2R2'
  !asclabels() = 'Reimersrat'   !Ratio of Reimers-like wind terms in ev - which dominates? - Politano et al. 2010, Eq.1
  !asclabels() = 'ttf'
  !asclabels() = 'LHeLH'
  
  
  
end subroutine getpltlabels
!***********************************************************************************************************************************



!***********************************************************************************************************************************
!> \brief Provides the labels for the plot axes of a *.plt? file in plotpltn (obsolescent)
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
  pglabels(15) = 'E\dbind,env\u (erg)'
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
  pglabels(34) = 'J\dorb\u'
  pglabels(35) = 'J\dorb\u/dt'
  pglabels(36) = 'dJ\dgw\u/dt'
  pglabels(37) = 'dJ\dwml\u/dt'
  pglabels(38) = 'dJ\ds-o\u/dt'
  pglabels(39) = 'dJ\dmtr\u/dt'
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
  pglabels(91) = 'Reimers ratio'   !Ratio of Reimers-like wind terms in ev - which dominates? - Politano et al. 2010,Eq.1
  
  
  
  
  
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
  asclabels(34) = 'Jorb'
  asclabels(35) = 'Jorbdt'
  asclabels(36) = 'dJgwdt'
  asclabels(37) = 'dJwmldt'
  asclabels(38) = 'dJsodt'
  asclabels(39) = 'dJmtrdt'
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
  asclabels(91) = 'Reimersrat'   !Ratio of Reimers-like wind terms in ev - which dominates? - Politano et al. 2010, Eq.1
  
  
  
  
  
end subroutine set_plotpltn_labels
!***********************************************************************************************************************************


!***********************************************************************************************************************************
!> \brief Print the list of variables in a *.plt? file to screen, for input menu of plotplt
subroutine printpltvarlist(nf)
  implicit none
  integer :: nf
  
  write(6,*)''
  write(6,'(A)')'  Primary variables:                                  0: Quit                           '
  write(6,'(A)')'                                                                                        '
  write(6,'(A)')'    1: model        16: Lh           28: Porb        34: Jorb                           '
  write(6,'(A)')'    2: t            17: Lhe          29: FLR         35: dJorb/dt                       '
  write(6,'(A)')'    3: dt           18: Lc           30: F1          36: dJgw/dt                        '
  write(6,'(A)')'    4: M            19: Lnu          31: dM          37: dJwml/dt                       '
  write(6,'(A)')'    5: Mhe          20: Lth          32: dMwind      38: dJmb/dt                        '
  write(6,'(A)')'    6: Mco          21: Prot         33: dMmt        39: dJmtr/dt                       '
  write(6,'(A)')'    7: Mone         22: VK2                          40: Mcomp                          '
  write(6,'(A)')'    8: R            23: Rcz                          41: e                              '
  write(6,'(A)')'    9: L            24: dRcz                                                            '
  write(6,'(A)')'   10: Teff         25: Tet                                                             '
  write(6,'(A)')'   11: Tc           26: Ralv       Abundances:                                          '
  write(6,'(A)')'   12: Tmax         27: Bp                 H  He   C   N   O  Ne  Mg   All              '
  write(6,'(A)')'   13: Rhoc                        Surf:  42  43  44  45  46  47  48   213              '
  write(6,'(A)')'   14: RhoTm                       Tmax:  49  50  51  52  53  54  55   214              '
  write(6,'(A)')'   15: Ebind,env                   Core:  56  57  58  59  60  61  62   215              '
  write(6,'(A)')'                                                                                        ' 
  write(6,'(A)')'   81: Qconv        86: Eb,recom     91: Rco                                            '
  write(6,'(A)')'   82: Pc           87: Eb,H2ass     92: STRMDL                                         ' 
  write(6,'(A)')'   83: Prot,c       88: Sc                                                              '
  write(6,'(A)')'   84: Eb,grav      89: S1e5K                                                           '
  write(6,'(A)')'   85: Eb,int       90: Rhe                                                             '
  write(6,'(A)')'                                                                                        ' 
  write(6,'(A)')'  Derived variables:                                                                    '
  write(6,'(A)')'   101: V      111: lambda_env    121: Pcr (MB)         131: Rho_avg             141: GMMenv/R    151: E_tot   '  
  write(6,'(A)')'   102: U-B    112: q_crit        122: Sills MB         132: Zsurf               142: M_bin '  
  write(6,'(A)')'   103: B-V    113: M2,crit       123: Tet: int/anal    133: t_f-t               143: a_orb      '
  write(6,'(A)')'   104: V-R    114: Vrot          124: t-to             134: P_rot/crit          144: J_orb      '
  write(6,'(A)')'   105: R-I    115: R/Rzams       125: Ne/O change      135: g_surf              145: J_spin             '
  write(6,'(A)')'   106: U-V    116: Mhe-Mco       126: Pgw,max          136: Reimers Mdot        146: J_tot       '
  write(6,'(A)')'   107: V-I    117: Menv          127: Rrl              137: Reimers-like        147: E_orb       '
  write(6,'(A)')'               118: Mconv         128: Xf               138: Rmrslike/Rmrs       148: E_spin       '
  write(6,'(A)')'               119: R/(dR/dt)     129: M.I.             139: Mzams-M             149: E_so       '
  write(6,'(A)')'               120: Rossby nr     130: w_spin           140: (Mzams-M)/Mzams     150: E_bind       '
  write(6,'(A)')'                                                                                        '
  write(6,'(A)')'  Special plots:                                                                        '
  if(nf.eq.1) then
     write(6,'(A)')"   201: HR Diagram         211: Timescales            221: dJ/dt's                      "
     write(6,'(A)')'   202: Convection plot    212: Luminosities          222: Mdots                        '
     write(6,'(A)')'                           213: Surface abundances    223: Winds                        '
     write(6,'(A)')'                           214: Tmax abundances                                         '
     write(6,'(A)')'                           215: Core abundances                                         '
  else
     write(6,'(A)')'   201: HR Diagram                                                                      '
  end if
  write(6,'(A)')'                                                                                        '
  
end subroutine printpltvarlist
!***********************************************************************************************************************************



!***********************************************************************************************************************************
!> \brief Read the *.plt[12] file fname from unit u and return its length and the contents
subroutine readplt(u,fname,nn,nvar,nc,verbose,dat,n,version)
  use kinds
  use constants
  implicit none
  integer :: nvar,nn
  real(double) :: dat(nvar,nn)
  integer :: ncols,nc,nc1,verbose,i,j,n,version,u
  character :: fname*(*)
  
  nc1 = nc !Get rid of 'unused' message
  
  !*** Unformatted:
  dat = 0.d0
  version = 2005  !Can no longer distinguish if unformatted read
  open(unit=u,form='formatted',status='old',file=trim(fname))
  rewind u
  read(u,*)ncols
  if(verbose.eq.1) write(6,'(A,I4,A)', advance='no')'  Found',ncols,' columns.'
  do j=1,nn
     read(u,*,err=12,end=11) (dat(i,j),i=1,ncols)
     if(verbose.eq.1.and.j.eq.1) write(6,'(A,F6.2,A)', advance='no')'  Mi =',dat(4,j),'Mo.'
  end do
  write(0,'(A)')'  ***  ERROR:  End of file reached, arrays too small!  ***'
  close(u)
  n = j-1   !Number of models in the file
  return
  
11 if(verbose.eq.1) write(6,'(A,I6,A)')'  File read OK,',j-1,' lines read.'
  close(u)
  n = j-1   !Number of models in the file
  return
  
12 if(verbose.eq.1.or.j.ge.3) write(6,'(A,I6)')'  Error reading file, line',j
  close(u)
  if(j.lt.3) call quit_program('Error reading input file')
  if(verbose.eq.1) write(6,'(A)')"  I'll skip the rest of the file and use the first part."
  
  n = j-1   !Number of models in the file
  
end subroutine readplt
!***********************************************************************************************************************************



!***********************************************************************************************************************************
!> \brief Change (e.g. de-log) and add plot variables for a *.plt? file
subroutine changepltvars(nn,nvar,n,dat,labels,dpdt)
  use kinds
  use constants
  implicit none
  integer :: nn,nvar,n,dpdt, i,j,j0,ib
  real(double) :: dat(nvar,nn),var(nn),dpdj(nn)
  real(double) :: c126(nn),c119a,c119b,x,z,mbol,bc,g0, Zsurf(nn),Menv(nn)
  character :: labels(nvar)*(99)
  
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
           !If upper and lower boundary are close enough, remove them:
           if(abs((dat(j-1,i)+dat(j,i))/dat(4,i)).lt.1.d-4) dat(j-1:j,i) = (/0.d0,0.d0/)
        end do !j
        do while(abs(dat(j0,i))/dat(4,i).lt.1.d-4.and.sum(abs(dat(j0:j0+5,i))).gt.1.d-7)  !Remove all the leading zeroes
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
  
  !Ebind in 10^40 ergs:
  dat(15,:) = dat(15,:)*m0*1.d-40                               ! Total envelope BE
  dat(84:87,:) = dat(84:87,:)*m0*1.d-40                         ! Envelope BE terms
  
  !Abundances: limit them to >10^-10
  !isn't it weird that the compiler actually understands this...?  You'd need at least two for-loops in freakin' C!
  dat(42:62,:) = max(dat(42:62,:),1.d-10)
  
  
  
  !************************************************************************      
  !***   CREATE EXTRA PLOT VARIABLES
  !************************************************************************      
  
  ! Z_surf = 1 - X - Y:  surface Z
  Zsurf(1:n) = 1.d0 - dat(42,1:n)-dat(43,1:n)
  
  ! 101-107: Colours:
  do i=1,n
     call lt2ubv(log10(dat(9,i)),log10(dat(10,i)),dat(4,i),log10(Zsurf(i)/2.d-2),  &
          mbol,bc,dat(101,i),dat(102,i), dat(103,i),dat(104,i),dat(105,i))
     dat(106,i) = dat(102,i)+dat(103,i)                                     !(U-V) = (U-B) + (B-V)
     dat(107,i) = dat(104,i)+dat(105,i)                                     !(V-I) = (V-R) + (R-I)
  end do
  
  !108-110 undefined
  
  
  ! H-envelope mass, premature def.:
  Menv(1:n) = dat(4,1:n) - dat(5,1:n)
  
  ! 111: lambda_env = G*M*M_env/(Ebind*R):
  dat(111,1:n) = g*dat(4,1:n)*Menv(1:n)*m0**2 / (dat(15,1:n)*dat(8,1:n)*r0*1.d40+1.d-30)
  !dat(111,1:n) = abs(dat(111,1:n))    !This 'hides' the fact that Ebind changes sign
  dat(111,1:n) = max(dat(111,1:n),0.d0)
  do i=1,n
     if(abs(dat(5,i)).lt.1.d-29) dat(111,i) = 0.d0  !If there's no He core mass, there's no lambda
     !write(*,'(I6,9ES20.5)')i,dat(4:5,i),Menv(i),dat(15,i),dat(8,i),dat(111,i)
     !write(99,'(9ES12.4)')dat((/5,8,111/),i)
  end do
  
  
  z = log10(Zsurf(1)/0.02)  !Use the surface Z of the first model as 'the' metallicity
  x = 0.30406 + 0.0805*z + 0.0897*z*z + 0.0878*z**3 + 0.0222*z**4
  
  ! 112: 
  ! 113: 
  dat(112,1:n) = (1.67 - x + 2*(dat(5,1:n)/(dat(4,1:n)+1.d-30))**5)/2.13
  dat(113,1:n) = dat(4,1:n)/(dat(112,1:n)+1.d-30)
  do i=1,n
     if(dat(5,i).lt.1.d-6) then
        dat(112,i) = 0.
        dat(113,i) = 0.
     end if
     !write(6,'(I6,9ES12.3)')i,dat(4,i),dat(5,i),dat(112,i),dat(113,i)
  end do
  
  ! 114: Vrot = 2piR/P -> km/s:
  dat(114,1:n) = tpi*dat(8,1:n)*r0/(dat(21,1:n)*day)/km
  
  ! 115: R/Rzams:
  dat(115,1:n) = dat(8,1:n)/(dat(8,1)+1.d-30)
  
  ! 116: Intershell mass:
  do i=1,n
     dat(116,i) = dat(5,i)-dat(6,i)
  end do
  
  ! 117: H-envelope mass:
  dat(117,1:n) = Menv(1:n)
  
  ! 118: Convective core boundary:
  do i=1,n
     dat(118,i) = 0.d0
     if(dat(64,i).lt.0.d0) dat(118,i) = abs(dat(64,i))
  end do
  
  ! 119: R/(dR/dt):
  dat(119,1) = 0.d0
  do i=2,n
     c119a = abs(dat(8,i)-dat(8,i-1))+1.d-30                    !dR
     c119b = abs(dat(2,i)-dat(2,i-1))+1.d-30                    !dt
     dat(119,i) = dat(8,i)/(c119a/c119b)                        !R/(dR/dt)
  end do
  
  ! 120: Rossby number = Prot/Tet:
  dat(120,1:n) = dat(21,1:n)/(dat(25,1:n)+1.d-30)
  
  ! 121: Critical Prot (Pc) for saturated MB (=2pi/omega_c): Pc_sun~2.5d, Tet_sun~13.8d:
  dat(121,1:n) = 2.5d0/13.8d0 * dat(25,1:n)
  
  ! 122: Saturated MB - Sills et al, 2000ApJ.534.335:
  do i=1,n
     dat(122,i) = 2.7e-3*(2*pi/dat(21,i))*(2*pi/dat(121,i))**2* dat(8,i)**0.5d0*dat(4,i)**(-0.5d0)
     if(dat(21,i).gt.dat(121,i)) dat(122,i) = 2.7e-3* (2*pi/dat(21,i))**3*dat(8,i)**0.5d0*dat(4,i)**(-0.5d0)
     if(dat(81,i).lt.0.02)  dat(122,i) = dat(122,i)*exp(1.d0-2.d-2/dat(81,i)) !Exponential decrease for thin convective envelopes
     if(dat(81,i).lt.1.d-9)  dat(122,i) = 0.d0
     if(dat(81,i).gt.1.d0-1.d-9)  dat(122,i) = 0.d0  !No MB for fully convective star
  end do !i
  dat(122,1:n) = dat(122,1:n)/day**3
  
  
  ! Calculate actual magnetic braking, according to Rappaport, Joss, Verbunt 1983:
  dat(38,1:n)  = 3.8e-30*dat(4,1:n)*m0*(dat(8,1:n)*r0)**4* (2*pi/(dat(21,1:n)*day))**3/1.d50
  do i=1,n
     if(dat(81,i).lt.0.02)  dat(38,i) = dat(38,i)*exp(1.d0-2.d-2/dat(81,i))
     if(dat(81,i).lt.1.d-9)  dat(38,i) = 0.d0
     if(dat(81,i).gt.1.d0-1.d-9)  dat(38,i) = 0.d0  !No MB for fully convective star
  end do !i
  
  ! 37: Take Sills MB in stead of Wind AML:
  dat(37,1:n) = dat(122,1:n)
  
  ! 38: Take Sills in stead of Rappaport MB:
  !dat(38,1:n) = dat(122,1:n)
  
  !dP/dJ = 3/(m1m2)(2piP^2(m1+m2)/G^2)^1/3:
  dpdj(1:n) = 3.d0/(dat(4,1:n)*dat(40,1:n)*m0*m0)*(2.d0*pi* (dat(28,1:n)*day)**2*(dat(4,1:n)+dat(40,1:n))*m0/(g*g)) **(1.d0/3.d0)
  
  ! 39: Replace AML due to non-conservative MT by 'negative AML' due to MT
  !     dJ/dt needed to obtain the same effect on Porb as from (conservative) mass transfer, 
  !     in case of no wind: use dat(31) instead of dat(33):
  !dat(39,1:n) = (dat(4,1:n)-dat(40,1:n))*m0*dat(31,1:n)*m0/yr* **(1.d0/3.d0)/1.d50
  
  
  
  ! ~Hyashi track radius:  
  var(1:n) = (1.1487d0*dat(9,1:n)**0.47d0 +  0.1186d0*dat(9,1:n)**0.8d0)/dat(4,1:n)**0.31d0
  
  ! 123: Analytic convective turnover timescale (days), adapted from Eggleton's CFUNCS.F:
  dat(123,1:n) = 28.437d0*(dat(8,1:n)**2*dat(4,1:n)/ dat(9,1:n))**(1.d0/3.d0) * (dat(8,1:n)/var(1:n))**2.7  
  dat(123,1:n) = dat(25,1:n)/dat(123,1:n)  !Actual Tet / analitic Tet
  
  ! 124: t-t0:
  dat(124,1:n) = dat(2,1:n) - dat(2,1)
  
  ! 125: (Ne/O)cen/(Ne/O)surf:
  dat(125,1:n) = (dat(61,1:n)/dat(60,1:n))/(dat(47,1:n)/dat(46,1:n))
  
  
  c126(1:n)    = 2*pi*(256.d0/5.d0)**(3.d0/8.d0) * g**(5.d0/8.d0)/c**(15.d0/8.d0) *  &
       (dat(5,1:n)*1.4)**(3.d0/8.d0) * m0**(5.d0/8.d0) / (dat(5,1:n)+1.4)**(1.d0/8.d0)
  
  ! 126: Pmax that can still be converged for a WD with the mass of the He core and a NS of 1.4Mo in a time t-t_H due to GWs:
  dat(126,1:n) = ((13.6d9-dat(2,1:n))*yr)**(3.d0/8.d0)*c126(1:n)/day
  
  ! 127: Roche-lobe radius:
  dat(127,1:n) = dat(8,1:n)/exp(dat(29,1:n))     
  
  ! 128: Xf := 2Xc + Yc + 1:
  dat(128,1:n) = 2*dat(56,1:n) + dat(57,1:n) + 1.
  
  ! 129: M.I. = k^2*M*R^2 in MoRo^2  (in some models, log(VK2) is listed):
  dat(129,1:n) = 10.d0**dat(22,1:n) * dat(4,1:n)*dat(8,1:n)**2
  
  ! 130: omega_spin (s^-1):
  dat(130,1:n) = 2*pi/(dat(21,1:n)*day+1.d-30)
  
  ! 131: Average Rho:
  dat(131,1:n) = dat(4,1:n)*m0/(4/3.d0*pi*(dat(8,1:n)*r0)**3)
  
  ! 132: Zsurf = 1 - X - Y; surface metallicity:
  dat(132,1:n) = Zsurf(1:n)
  
  ! 133: t - t_final, avoid by setting dat(,1) = dat(,2):
  dat(133,1:n) = dat(2,n) - min(dat(2,1:n), dat(2,n)-1.d4)
  
  ! 134: Critical (Keplerian) omega:
  !dat(134,1:n) = sqrt(2*g*dat(4,1:n)*m0/(dat(8,1:n)*r0)**3)/day
  
  ! 134: Critical (Keplerian) rotation period:
  dat(134,1:n) = 2*pi*sqrt((dat(8,1:n)*r0)**3/(g*dat(4,1:n)*m0))/day
  
  ! 134: Pcrit -> Prot/Pcrit:
  dat(134,1:n) = dat(21,1:n)/(dat(134,1:n)+1.d-30)
  
  ! 135: g_surf = GM/R^2 (cgs):  
  dat(135,1:n) = G*dat(4,1:n)*M0/((dat(8,1:n)*R0)**2+1.d-30)
  g0 = G*M0/R0**2
  
  ! 136: Reimers wind = 4e-13*(L/Lo)/((g/g0)*(R/Ro))  (Mo/yr):
  dat(136,1:n) = 4.d-13*dat(9,1:n)/(dat(135,1:n)/g0*dat(4,1:n))
  
  
  ! 137: Reimers-like wind: 0.2 x min of 3.16e-14*(M/Mo)(L/Lo)(10^50erg/Ebind) and 9.61e-10 (L/Lo), in Mo/yr:
  dat(137,1:n) = 0.2*min( 3.16e-14*dat(4,1:n)*dat(9,1:n)/(dat(15,1:n)*1.d-10+1.d-30), 9.61e-10*dat(9,1:n))
  
  ! 138: Reimers-like wind / Reimers wind:
  dat(138,1:n) = dat(137,1:n)/(dat(136,1:n)+1.d-30)
  
  ! 139: M_zams - M:
  dat(139,1:n) = dat(4,1) - dat(4,1:n)
  
  ! 140: (M_zams - M)/M_zams:
  dat(140,1:n) = dat(139,1:n)/dat(4,1)
  
  ! 140: (M_zams - M)/M:
  !dat(140,1:n) = dat(139,1:n)/dat(4,1:n)
  
  ! 141: G*M*M_env/R / 1e40:
  dat(141,1:n) = g*dat(4,1:n)*dat(117,1:n)*m0**2 / (dat(8,1:n)*r0*1.d40)
  
  ! 142: Mbin (Mo):
  dat(142,1:n) = dat(4,1:n)+dat(40,1:n)
  
  ! 143: a_orb (Ro):
  do i=1,n
     call p2a(dat(142,i)*M0,dat(28,i)*day,dat(143,i))  ! a_orb (cm)
  end do
  dat(143,1:n) = dat(143,1:n)/R0                       ! cm -> Ro
  
  ! 144: J_orb (G^1/2 Mo^3/2 Ro^1/2):
  dat(144,1:n) = dat(34,1:n)*1.d50/(G**0.5d0 * M0**1.5d0 * R0**0.5d0)
  
  ! 145: J_spin = I*w (G^1/2 Mo^3/2 Ro^1/2):
  dat(145,1:n) = dat(129,1:n)*dat(130,1:n) * M0*R0**2 / (G**0.5d0 * M0**1.5d0 * R0**0.5d0)
  
  ! 146: J_tot = J_orb + J_spin (G^1/2 Mo^3/2 Ro^1/2):
  dat(146,1:n) = dat(144,1:n) + dat(145,1:n)
  
  ! 147: E_orb = M1M2/2a  (G Mo^2 / Ro):
  dat(147,1:n) = dat(4,1:n)*dat(40,1:n)/(2*dat(143,1:n))
  
  ! 148: E_spin = 1/2 I w^2  (G Mo^2 / Ro):
  dat(148,1:n) = 0.5d0*dat(129,1:n)*dat(130,1:n)**2 * M0*R0**2 / (G * M0**2 / R0)
  
  ! 149: E_so = E_orb + E_spin (G Mo^2 / Ro):  
  dat(149,1:n) = dat(147,1:n) + dat(148,1:n)
  
  ! 150: E_bind (G Mo^2 / Ro) - CHECK: use tailored definition
  dat(150,1:n) = dat(15,1:n) * 1.d40 / (G * M0**2 / R0)
  
  ! 151: E_tot = E_so + E_bind (G Mo^2 / Ro):
  dat(151,1:n) = dat(149,1:n) + dat(150,1:n)
  
  
  
  
  
  
  !*** Timescales:
  
  ! 201: Nuclear evolution timescale: 
  dat(201,1:n) = dat(4,1:n)*m0/1.9891/(dat(9,1:n)*l0)*4.d10
  
  ! 202: KH timescale:
  dat(202,1:n) = g*dat(4,1:n)**2*m0*m0 / (dat(8,1:n)*r0*dat(9,1:n)*l0)/yr
  
  ! 203: Mass transfer:
  dat(203,1:n) = dat(4,1:n)/max(abs(dat(33,1:n)),1.d-30)
  
  ! 204: Gravitational waves:
  dat(204,1:n) = dat(34,1:n)/max(dat(36,1:n)*yr,1.d-30)
  
  ! 205: Magnetic braking (Actually SO-coupling!):
  !dat(205,1:n) = dat(34,1:n)/max(abs(dat(38,1:n))*yr,1.d-30)
  
  ! 205 Dynamical: t ~ sqrt(R^3/(G*M)):
  dat(205,1:n) = sqrt(dat(8,1:n)**3/(g*dat(4,1:n)))
  dpdt  = 0
  
  
  ! Replace dJ/dt by dP/dt:
  ! 35 = H_orb,  36 = H_gw, 37 = H_wml, 38 = H_s-o, 39 = H_mtr
  if(1.eq.2) then
     ! dP/dJ = 3/(m1m2)(2piP^2(m1+m2)/G^2)^1/3:
     dpdj(1:n) = 3.d0/(dat(4,1:n)*dat(40,1:n)*m0*m0)*(2.d0*pi* (dat(28,1:n)*day)**2*(dat(4,1:n)+dat(40,1:n))*m0/(g*g)) **(1.d0/3.d0)
     do i=35,39
        dat(i,1:n) = dat(i,1:n)*dpdj(1:n)*1.d50+1.d-30
     end do
     labels(35) = 'dP\dorb\u/dt'
     labels(36) = 'dP\dgw\u/dt'
     labels(37) = 'dP\dwml\u/dt'
     labels(38) = 'dP\ds-o\u/dt'
     labels(39) = 'dP\dmtr\u/dt'
     labels(221) = 'dP\dorb\u/dt'
     dpdt = 1
  end if
  
  ! Replace dP/dt by timescales:
  if(1.eq.1) then
     do i=35,39
        dat(i,1:n) = dat(28,1:n)*day/dat(i,1:n)/yr
     end do
     labels(35) = '\gt\dP\dorb\u\u (yr)'
     labels(36) = '\gt\dP\dgw\u\u (yr)'
     labels(37) = '\gt\dP\dwml\u\u (yr)'
     labels(38) = '\gt\dP\ds-o\u\u (yr)'
     labels(39) = '\gt\dP\dmtr\u\u (yr)'
     labels(221) = '\gt\dP\dorb\u\u (yr)'
     dpdt = 2
  end if
  
  
end subroutine changepltvars
!***********************************************************************************************************************************


