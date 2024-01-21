!> \file plt_functions.f90  Functions and subroutines for plotplt* in the evTools package that need pgplot

! Copyright 2002-2024 Marc van der Sluys - marc.vandersluys.nl
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
!> \brief  Procedures for plt functions

module plt_funcs
  implicit none
  save
  
contains
  
  !*********************************************************************************************************************************
  !> \brief  Compute the analytical zeta_ad from the He core mass
  !!
  !! \param  Mc    Array with helium core masses fraction
  !! \retval zeta  Array with zeta_ad values
  
  subroutine compute_zeta_ad(Mc, zeta)
    use SUFR_kinds, only: double
    implicit none
    real(double), intent(in) :: Mc(:)
    real(double), intent(out) :: zeta(size(Mc))
    
    zeta  =  2.d0/3.d0 * Mc / (1.d0 - Mc)
    zeta  =  zeta - 1.d0/3.d0 * (1.d0 - Mc)/(1.d0 + 2*Mc)
    zeta  =  zeta - 0.03d0*Mc
    zeta  =  zeta + 0.2d0 * Mc/(1.d0+(1.d0-Mc)**(-6))
    
    !print*, Mc(size(Mc)), 2.d0/3.d0 * Mc(size(Mc)) / (1.d0 - Mc(size(Mc))), &
    !     1.d0/3.d0 * (1.d0 - Mc(size(Mc)))/(1.d0 + 2*Mc(size(Mc))), 0.03d0*Mc(size(Mc)),  &
    !     0.2d0 * Mc(size(Mc))/(1.d0+(1.d0-Mc(size(Mc)))**(-6)), zeta(size(Mc))
    
  end subroutine compute_zeta_ad
  !*********************************************************************************************************************************
  
  
  !*********************************************************************************************************************************
  !> \brief  Compute the analytical zeta_rl from the masses of the two binary components, and the MT conservation factor
  !! \note Assuming no wind mass loss
  !! \see  Woods et al., ApJ 744, 12 (2012), Sect. 2.3
  !!
  !!
  !! \param  Md    Array with donor masses
  !! \param  Ma    Array with accretor masses
  !! \param  beta  Mass-conservation factor: beta=1 means fully conservative MT
  !!
  !! \retval zeta  Array with zeta_rl values
  
  subroutine compute_zeta_rl(Md,Ma, beta, zeta)
    use SUFR_kinds, only: double
    implicit none
    real(double), intent(in) :: Md(:),Ma(:), beta
    real(double), intent(out) :: zeta(size(Md))
    real(double) :: Md2(size(Md)),Ma2(size(Md)),Mt(size(Md)), q(size(Md)),q3(size(Md)), c3rd
    real(double) :: dlnAdlnMd(size(Md)), dlnRldlnQ(size(Md)), dlnQdlnMd(size(Md))
    
    c3rd = 1.d0/3.d0
    Md2 = Md**2
    Ma2 = Ma**2
    Mt  = Md+Ma
    q   = Md/Ma
    q3  = q**c3rd  ! q^(1/3)
    
    dlnAdlnMd = (2*(Md2-Ma2) - Md*Ma*(1.d0-beta)) / (Ma*Mt)                                    ! Eq.11, dlna/dlnMd
    dlnRldlnQ = 2*c3rd - c3rd*q3 * (1.2d0*q3 + 1.d0/(1.d0+q3)) / (0.6d0*q3**2 + log(1.d0+q3))  ! Eq.12, dln(Rrl/a)/dlnq
    dlnQdlnMd = 1.d0 + beta*Md/Ma                                                              ! Eq.13, dlnq/dlnMd
    
    zeta = dlnAdlnMd + dlnRldlnQ * dlnQdlnMd                                                   ! Eq.9
    
    !print*,real((/Md(1),Ma(1),beta, q(1),dlnAdlnMd(1), dlnRldlnQ(1), dlnQdlnMd(1),zeta(1)/))
    
  end subroutine compute_zeta_rl
  !*********************************************************************************************************************************
  
end module plt_funcs
!***********************************************************************************************************************************








!***********************************************************************************************************************************
!> \brief Provides the labels for the plot axes of a *.plt? file in plotplt
!! 
!! \param  nf         Number of input files
!! \param  nvar       Number of variables
!! \retval pglabels   PGPlot labels
!! \retval asclabels  ASCII labels
!! \retval defvar     Variable is defined (1) or not (0)

subroutine getpltlabels(nf,nvar,pglabels,asclabels,defvar)
  implicit none
  integer, intent(in) :: nf,nvar
  integer, intent(out) :: defvar(0:nvar)
  character, intent(out) :: pglabels(nvar)*(99),asclabels(nvar)*(99)
  
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
  
  pglabels(152) = 'E\dbind,env,grav\u + E\dbind,env,int\u (10\u40\d erg)'
  pglabels(153) = 'E\dbind,env,recom\u + E\dbind,env,H2ass\u (10\u40\d erg)'
  pglabels(154) = '|E\dbind,env,grav\u / E\dbind,env,int\u|'
  pglabels(155) = '|E\dbind,env,recom\u / E\dbind,env,H2ass\u|'
  pglabels(156) = '|E\dbind,env,grav+int\u / E\dbind,env,recom+H2ass\u|'
  
  pglabels(157) = '\(2137)\denv,gr\u'      ! lambda_env,gr
  pglabels(158) = '\(2137)\denv,gr+in\u'   ! lambda_env,gr+int
  
  pglabels(159) = 'P\dorb\u (h)'           ! Porb in hours
  pglabels(160) = 'P\dorb\u (m)'           ! Porb in minutes
  
  pglabels(161) = '\(0632)\d*\u = dlogR\d*\u/dlogM'             ! zeta_*  = d(logR)/d(logM)
  pglabels(162) = '\(0632)\dRL\u = dlogR\dRL\u/dlogM'           ! zeta_RL = d(logRL)/d(logM)
  pglabels(163) = '\(0632)\dad\u = (dlogR\d*\u/dlogM)\dad\u'    ! zeta_*  = d(logR)/d(logM) - analytical
  pglabels(164) = '\(0632)\drl,an\u, \(0628)=0.0'        ! zeta_RL,an = d(logRL)/d(logM) - analytical
  pglabels(165) = '\(0632)\drl,an\u, \(0628)=0.5'        ! zeta_RL,an = d(logRL)/d(logM) - analytical
  pglabels(166) = '\(0632)\drl,an\u, \(0628)=1.0'        ! zeta_RL,an = d(logRL)/d(logM) - analytical
  
  defvar(101:166) = 1
  
  pglabels(171) = 'q\d1\u'  ! q_1 = M1/M2
  pglabels(172) = 'q\d2\u'  ! q_2 = M2/M1
  defvar(171:172) = 1
  
  
  
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
     pglabels(224:226) = '\(0632)'
     defvar(221:226) = 1
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
  
  asclabels(152) = 'Ebind_grav_int'
  asclabels(153) = 'Ebind_recom_H2ass'
  asclabels(154) = 'Ebind_grav_o_int'
  asclabels(155) = 'Ebind_recom_o_H2ass'
  asclabels(156) = 'Ebind_grav_int_o_recom_H2ass'
  
  asclabels(157) = 'lambda_grav'
  asclabels(158) = 'lambda_grint'
  
  asclabels(159) = 'Porb_hr'
  asclabels(160) = 'Porb_mn'
  
  asclabels(161) = 'Zeta_st'
  asclabels(162) = 'Zeta_RL'
  asclabels(163) = 'Zeta_ad'
  asclabels(164) = 'Zeta_RL_an_beta00'
  asclabels(165) = 'Zeta_RL_an_beta05'
  asclabels(166) = 'Zeta_RL_an_beta10'
  
  asclabels(171) = 'q1'
  asclabels(172) = 'q2'
  
  
  
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
  asclabels(224) = 'Zetas_mdl'
  asclabels(225) = 'Zetas_anal'
  asclabels(226) = 'Zetas_all'
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
!! 
!! \retval pglabels   PGPlot labels
!! \retval asclabels  ASCII labels
!! \param  maxi       Size of the arrays

subroutine set_plotpltn_labels(pglabels,asclabels,maxi)
  implicit none
  integer, intent(in) :: maxi
  character, intent(out) :: pglabels(maxi)*(*),asclabels(maxi)*(*)
  
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
!!
!! \param nf  Number of input files

subroutine printpltvarlist(nf)
  implicit none
  integer, intent(in) :: nf
  
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
  write(6,'(A)')'  Derived variables:                                                                                             '
  write(6,'(A)')'   101: V      111: lambda_env    121: Pcr (MB)         131: Rho_avg             141: GMMenv/R   151: E_tot      '
  write(6,'(A)')'   102: U-B    112: q_crit        122: Sills MB         132: Zsurf               142: M_bin      152: Ebenv_gr+in'
  write(6,'(A)')'   103: B-V    113: M2,crit       123: Tet: int/anal    133: t_f-t               143: a_orb      153: Ebenv_re+H2'
  write(6,'(A)')'   104: V-R    114: Vrot          124: t-to             134: P_rot/crit          144: J_orb      154: Ebenv_gr/in'
  write(6,'(A)')'   105: R-I    115: R/Rzams       125: Ne/O change      135: g_surf              145: J_spin     155: Ebenv_re/H2'
  write(6,'(A)')'   106: U-V    116: Mhe-Mco       126: Pgw,max          136: Reimers Mdot        146: J_tot      156: Ebenv_gi/rH'
  write(6,'(A)')'   107: V-I    117: Menv          127: Rrl              137: Reimers-like        147: E_orb      157: lam_gr     '
  write(6,'(A)')'               118: Mconv         128: Xf               138: Rmrslike/Rmrs       148: E_spin     158: lam_gr+in  '
  write(6,'(A)')'               119: R/(dR/dt)     129: M.I.             139: Mzams-M             149: E_so       159: Porb (hr)  '
  write(6,'(A)')'               120: Rossby nr     130: w_spin           140: (Mzams-M)/Mzams     150: E_bind     160: Porb (min) '
  write(6,'(A)')'                                                                                                                 '
  write(6,'(A)')'   161: zeta_*                  171: q1                                                                          '
  write(6,'(A)')'   162: zeta_RL                 172: q2                                                                          '
  write(6,'(A)')'   163: zeta_ad                                                                                                  '
  write(6,'(A)')'   164: zeta_RL,an, beta=0.0                                                                                     '
  write(6,'(A)')'   165: zeta_RL,an, beta=0.5                                                                                     '
  write(6,'(A)')'   166: zeta_RL,an, beta=1.0                                                                                     '
  write(6,'(A)')'                                                                                                                 '
  write(6,'(A)')'                                                                                                                 '
  write(6,'(A)')'  Special plots:                                                                                                 '
  if(nf.eq.1) then
     write(6,'(A)')"   201: HR Diagram         211: Timescales            221: dJ/dt's                                            "
     write(6,'(A)')'   202: Convection plot    212: Luminosities          222: Mdots                                              '
     write(6,'(A)')'                           213: Surface abundances    223: Winds                                              '
     write(6,'(A)')'                           214: Tmax abundances       224: Zetas: mdl                                         '
     write(6,'(A)')'                           215: Core abundances       225: Zetas: anl                                         '
     write(6,'(A)')'                                                      226: Zetas: all                                         '
  else
     write(6,'(A)')'   201: HR Diagram                                                                                            '
  end if
  write(6,'(A)')'                                                                                                                 '
  
end subroutine printpltvarlist
!***********************************************************************************************************************************



!***********************************************************************************************************************************
!> \brief Read the *.plt[12] file fname from unit u and return its length and the contents
!!
!! \param u         Input unit
!! \param fname     Input file name
!! \param nmax      Maximum number of model lines
!! \param nvar      Number of variables
!! \param nc        Expected number of columns in the input file - no longer used
!! \param verbose   Verbosity (0,1)
!! 
!! \retval datf     Data array
!! \retval nfi      Number of models read
!! \retval version  Code-output version

subroutine read_plt_bse(u,fname,nmax,nvar,nc,verbose,datf,nfi,version)
  use SUFR_kinds, only: double
  
  implicit none
  integer, intent(in) :: u,nmax,nvar,nc,verbose
  character, intent(in) :: fname*(*)
  integer, intent(out) :: nfi,version
  real(double), intent(out) :: datf(nvar,nmax)
  
  if(len_trim(fname)-index(trim(fname),'.dat',.true.).eq.3) then  ! File name ends in '.dat', assume BSE output
     call read_bse(u,trim(fname),nmax,nvar,verbose,datf,nfi,version)
  else
     call read_plt(u,trim(fname),nmax,nvar,nc,verbose,datf,nfi,version)
  end if
  
end subroutine read_plt_bse
!***********************************************************************************************************************************



!***********************************************************************************************************************************
!> \brief Read the *.plt[12] file fname from unit u and return its length and the contents
!!
!! \param u         Input unit
!! \param fname     Input file name
!! \param nn        Maximum number of model lines
!! \param nvar      Number of variables
!! \param nc        Expected number of columns in the input file - no longer used
!! \param verbose   Verbosity (0,1)
!! 
!! \retval dat      Data array
!! \retval n        Number of models read
!! \retval version  Code-output version

subroutine read_plt(u,fname,nn,nvar,nc,verbose,dat,n,version)
  use SUFR_kinds, only: double
  use SUFR_dummy, only: dumint
  
  implicit none
  integer, intent(in) :: u,nn,nvar,nc,verbose
  character, intent(in) :: fname*(*)
  integer, intent(out) :: n,version
  real(double), intent(out) :: dat(nvar,nn)
  integer :: ncols,i,j
  
  dumint = nc ! Get rid of 'unused' message
  
  if(verbose.eq.1) write(6,'(A)', advance='no')' Reading ev file '//trim(fname)//':'
  
  !*** Unformatted:
  dat = 0.d0
  open(unit=u,form='formatted',status='old',file=trim(fname))
  rewind u
  read(u,*) ncols
  if(verbose.eq.1) write(6,'(A,I4,A)', advance='no')'  Found',ncols,' columns.'
  version = 2005   ! Can no longer distinguish with unformatted read
  if(ncols.eq.83.or.ncols.eq.92) version = 2011  ! Latest version 
  do j=1,nn
     read(u,*,err=12,end=11) (dat(i,j),i=1,ncols)
     if(verbose.eq.1.and.j.eq.1) write(6,'(A,F6.2,A)', advance='no')'  Mi =',dat(4,j),'Mo.'
  end do
  write(0,'(A)')'  ***  ERROR:  End of file reached, arrays too small!  ***'
  close(u)
  n = j-1   ! Number of models in the file
  return
  
11 if(verbose.eq.1) write(6,'(A,I6,A)')'  File read OK,',j-1,' lines read.'
  close(u)
  n = j-1   ! Number of models in the file
  return
  
12 if(verbose.eq.1.or.j.ge.3) write(6,'(A,I6)')'  Error reading ev file, line',j
  close(u)
  if(j.lt.3) call quit_program('Error reading input file')
  if(verbose.eq.1) write(6,'(A)')"  I'll skip the rest of the file and use the first part."
  
  n = j-1   ! Number of models in the file
  
end subroutine read_plt
!***********************************************************************************************************************************



!***********************************************************************************************************************************
!> \brief Read a BSE binary.dat file fname from unit u and return its length and contents
!!
!! \param u         Input unit
!! \param fname     Input file name
!! \param nn        Maximum number of model lines
!! \param nvar      Number of variables
!! \param verbose   Verbosity (0,1)
!! 
!! \retval dat      Data array
!! \retval n        Number of models read
!! \retval version  Code-output version

subroutine read_bse(u,fname,nn,nvar,verbose,dat,n,version)
  use SUFR_kinds, only: double
  use SUFR_constants, only: pi, pc_sigma,solday, msun,rsun,lsun
  
  implicit none
  integer, intent(in) :: u,nn,nvar,verbose
  character, intent(in) :: fname*(*)
  integer, intent(out) :: n,version
  real(double), intent(out) :: dat(nvar,nn)
  
  integer :: j  !ncols
  real(double) :: tmpdat(19), Porb, a2j
  
  if(verbose.eq.1) write(6,'(A)', advance='no')' Reading BSE file '//trim(fname)//':'
  
  ! Fixed for BSE:
  !ncols = 19
  version = 1
  
  dat = 0.d0
  open(unit=u,form='formatted',status='old',file=trim(fname))
  rewind u
  
  do j=1,nn
     read(u,*,err=12,end=11) tmpdat  ! (dat(i,j),i=1,ncols)
     if(verbose.eq.1.and.j.eq.1) write(6,'(A,F6.2,A)', advance='no')'  Mi =',tmpdat(4),'Mo.'
     if(tmpdat(1).lt.0.d0) goto 11  ! Final model has t=-1.0
     
     ! Copy variables:
     dat(4,j)  = tmpdat(4)   ! M1
     dat(40,j) = tmpdat(5)   ! M2
     dat(5,j)  = tmpdat(6)   ! Mc1
     dat(8,j)  = tmpdat(8)   ! log R1
     dat(9,j)  = tmpdat(12)  ! log L1
     dat(31,j) = tmpdat(16)  ! dM1/dt
     dat(33,j) = tmpdat(16)  ! dM1/dt
     dat(41,j) = tmpdat(19)  ! e_orb
     
     ! Convert variables:
     dat(1,j)  = dble(j)  ! Model number
     dat(2,j)  = tmpdat(1)*1.d6   ! age Myr -> yr
     if(j.gt.1) dat(3,j)  = dat(2,j)-dat(2,max(j-1,1))  ! Delta t - max: silence array out of bounds compiler warning
     
     dat(29,j) = log(tmpdat(10))     ! R1/RL1 -> FLR = ln(R1/RL1)
     dat(21,j) = 2*pi*tmpdat(14)/solday  ! Ospin1 -> Prot1
     dat(10,j) = log10( 10.d0**tmpdat(12)*lsun / (4*pi*(10.d0**tmpdat(8)*rsun)**2 * pc_sigma) )*0.25d0  ! log L1,log R1 -> log Teff1
     
     call a2p(sum(tmpdat(4:5))*msun, tmpdat(18)*rsun, Porb)  ! a_orb -> P_orb in s
     dat(28,j) = Porb/solday                                ! P_orb: s -> day
     dat(34,j) = a2j(tmpdat(4), tmpdat(5), tmpdat(18))*1.d-50   ! M1,M2,a_orb -> J_orb in 10^50 g cm^2 s^-1
  end do
  
  
  write(0,'(A)')'  ***  ERROR:  End of file reached, arrays too small!  ***'
  close(u)
  n = j-1   ! Number of models in the file
  return
  
11 continue
  if(verbose.eq.1) write(6,'(A,I6,A)')'  File read OK,',j-1,' lines read.'
  close(u)
  n = j-1   ! Number of models in the file
  return
  
12 continue
  if(verbose.eq.1.or.j.ge.3) write(6,'(A,I6)')'  Error reading BSE file, line',j
  print*,real(tmpdat)
  close(u)
  if(j.lt.3) call quit_program('Error reading input file')
  if(verbose.eq.1) write(6,'(A)')"  I'll skip the rest of the file and use the first part."
  
  n = j-1   ! Number of models in the file
  
end subroutine read_bse
!***********************************************************************************************************************************



!***********************************************************************************************************************************
!> \brief Change (e.g. de-log) and add plot variables for a *.plt? file
!!
!! \param nn        Maximum number of model lines
!! \param nvar      Number of variables
!! \param n         Number of models read
!! \retval dat      Data array (I/O)
!! \retval labels   Variable labels (I/O)
!! \retval dpdt     Pdot mode: 0: dJ/dt,  1: dP/dt,  2: timescales

subroutine changepltvars(nn,nvar,n,dat,labels,dpdt)
  use SUFR_kinds, only: double
  use SUFR_constants, only: pi,pi2, pc_c,pc_g, km,julyear,solday, lsun,msun,rsun
  use SUFR_numerics, only: deq,deq0
  use plt_funcs, only: compute_zeta_ad, compute_zeta_rl
  
  implicit none
  integer, intent(in) :: nn,nvar,n
  real(double), intent(inout) :: dat(nvar,nn)
  character, intent(inout) :: labels(nvar)*(99)
  integer, intent(out) :: dpdt
  
  integer :: i,j,j0,ib
  real(double) :: var(nn),dpdj(nn), beta
  real(double) :: c126(nn),c119a,c119b,x,z,mbol,bc,g0, Zsurf(nn),Menv(nn)
  
  ! de-log some variables:
  do i=4,nvar
     if(deq0(dat(i,1))) dat(i,1) = dat(i,2)  !In case you want to log them. Skip t,dt
  end do
  do i=36,39
     if(dat(i,1).le.0.) dat(i,1) = dat(i,2)
  end do
  
  do i=8,14  ! De-log them
     dat(i,1:n) = 10.d0**dat(i,1:n)
  end do
  
  ! 'Clean' the convection data:
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
  
  dat(5,1:n) = dat(5,1:n) + 1.d-30                              ! Still necessary?
  
  ! Ebind in 10^40 ergs:
  dat(15,:) = dat(15,:)*msun*1.d-40                               ! Total envelope BE
  dat(84:87,:) = dat(84:87,:)*msun*1.d-40                         ! Envelope BE terms
  
  ! Abundances: limit them to >10^-10
  ! isn't it weird that the compiler actually understands this...?  You'd need at least two for-loops in freakin' C!
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
  dat(111,1:n) = pc_g*dat(4,1:n)*Menv(1:n)*msun**2 / (dat(15,1:n)*dat(8,1:n)*rsun*1.d40+1.d-30)
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
  dat(114,1:n) = pi2*dat(8,1:n)*rsun/(dat(21,1:n)*solday)/km
  
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
     if(dat(64,i).lt.0.d0 .and. abs(dat(64,i)).lt.dat(4,i)*0.99d0) dat(118,i) = abs(dat(64,i))
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
  dat(122,1:n) = dat(122,1:n)/solday**3
  
  
  ! Calculate actual magnetic braking, according to Rappaport, Joss, Verbunt 1983:
  dat(38,1:n)  = 3.8e-30*dat(4,1:n)*msun*(dat(8,1:n)*rsun)**4* (2*pi/(dat(21,1:n)*solday))**3/1.d50
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
  dpdj(1:n) = 3.d0/(dat(4,1:n)*dat(40,1:n)*msun*msun)*(2.d0*pi* (dat(28,1:n)*solday)**2*(dat(4,1:n)+dat(40,1:n))*msun/(pc_g**2)) &
       **(1.d0/3.d0)
  
  ! 39: Replace AML due to non-conservative MT by 'negative AML' due to MT
  !     dJ/dt needed to obtain the same effect on Porb as from (conservative) mass transfer, 
  !     in case of no wind: use dat(31) instead of dat(33):
  !dat(39,1:n) = (dat(4,1:n)-dat(40,1:n))*msun*dat(31,1:n)*msun/julyear* **(1.d0/3.d0)/1.d50
  
  
  
  ! ~Hyashi track radius:  
  var(1:n) = (1.1487d0*dat(9,1:n)**0.47d0 +  0.1186d0*dat(9,1:n)**0.8d0)/dat(4,1:n)**0.31d0
  
  ! 123: Analytic convective turnover timescale (days), adapted from Eggleton's CFUNCS.F:
  dat(123,1:n) = 28.437d0*(dat(8,1:n)**2*dat(4,1:n)/ dat(9,1:n))**(1.d0/3.d0) * (dat(8,1:n)/var(1:n))**2.7  
  dat(123,1:n) = dat(25,1:n)/dat(123,1:n)  !Actual Tet / analitic Tet
  
  ! 124: t-t0:
  dat(124,1:n) = dat(2,1:n) - dat(2,1)
  
  ! 125: (Ne/O)cen/(Ne/O)surf:
  dat(125,1:n) = (dat(61,1:n)/dat(60,1:n))/(dat(47,1:n)/dat(46,1:n))
  
  
  c126(1:n)    = 2*pi*(256.d0/5.d0)**(3.d0/8.d0) * pc_g**(5.d0/8.d0)/pc_c**(15.d0/8.d0) *  &
       (dat(5,1:n)*1.4)**(3.d0/8.d0) * msun**(5.d0/8.d0) / (dat(5,1:n)+1.4)**(1.d0/8.d0)
  
  ! 126: Pmax that can still be converged for a WD with the mass of the He core and a NS of 1.4Mo in a time t-t_H due to GWs:
  dat(126,1:n) = ((13.6d9-dat(2,1:n))*julyear)**(3.d0/8.d0)*c126(1:n)/solday
  
  ! 127: Roche-lobe radius:
  dat(127,1:n) = dat(8,1:n)/exp(dat(29,1:n))     
  
  ! 128: Xf := 2Xc + Yc + 1:
  dat(128,1:n) = 2*dat(56,1:n) + dat(57,1:n) + 1.
  
  ! 129: M.I. = k^2*M*R^2 in MoRo^2  (in some models, log(VK2) is listed):
  dat(129,1:n) = 10.d0**dat(22,1:n) * dat(4,1:n)*dat(8,1:n)**2
  
  ! 130: omega_spin (s^-1):
  dat(130,1:n) = 2*pi/(dat(21,1:n)*solday+1.d-30)
  
  ! 131: Average Rho:
  dat(131,1:n) = dat(4,1:n)*msun/(4/3.d0*pi*(dat(8,1:n)*rsun)**3)
  
  ! 132: Zsurf = 1 - X - Y; surface metallicity:
  dat(132,1:n) = Zsurf(1:n)
  
  ! 133: t - t_final, avoid by setting dat(,1) = dat(,2):
  dat(133,1:n) = dat(2,n) - min(dat(2,1:n), dat(2,n)-1.d3)
  
  ! 134: Critical (Keplerian) omega:
  !dat(134,1:n) = sqrt(2*pc_g*dat(4,1:n)*msun/(dat(8,1:n)*rsun)**3)/solday
  
  ! 134: Critical (Keplerian) rotation period:
  dat(134,1:n) = 2*pi*sqrt((dat(8,1:n)*rsun)**3/(pc_g*dat(4,1:n)*msun))/solday
  
  ! 134: Pcrit -> Prot/Pcrit:
  dat(134,1:n) = dat(21,1:n)/(dat(134,1:n)+1.d-30)
  
  ! 135: g_surf = GM/R^2 (cgs):  
  dat(135,1:n) = pc_g*dat(4,1:n)*msun/((dat(8,1:n)*rsun)**2+1.d-30)
  g0 = pc_g*msun/rsun**2
  
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
  dat(141,1:n) = pc_g*dat(4,1:n)*dat(117,1:n)*msun**2 / (dat(8,1:n)*rsun*1.d40)
  
  ! 142: Mbin (Mo):
  dat(142,1:n) = dat(4,1:n)+dat(40,1:n)
  
  ! 143: a_orb (Ro):
  do i=1,n
     call p2a(dat(142,i)*msun,dat(28,i)*solday,dat(143,i))  ! a_orb (cm)
  end do
  dat(143,1:n) = dat(143,1:n)/rsun                       ! cm -> Ro
  
  ! 144: J_orb (G^1/2 Mo^3/2 Ro^1/2):
  dat(144,1:n) = dat(34,1:n)*1.d50/(pc_g**0.5d0 * msun**1.5d0 * rsun**0.5d0)
  
  ! 145: J_spin = I*w (G^1/2 Mo^3/2 Ro^1/2):
  dat(145,1:n) = dat(129,1:n)*dat(130,1:n) * msun*rsun**2 / (pc_g**0.5d0 * msun**1.5d0 * rsun**0.5d0)
  
  ! 146: J_tot = J_orb + J_spin (G^1/2 Mo^3/2 Ro^1/2):
  dat(146,1:n) = dat(144,1:n) + dat(145,1:n)
  
  ! 147: E_orb = M1M2/2a  (G Mo^2 / Ro):
  dat(147,1:n) = dat(4,1:n)*dat(40,1:n)/(2*dat(143,1:n))
  
  ! 148: E_spin = 1/2 I w^2  (G Mo^2 / Ro):
  dat(148,1:n) = 0.5d0*dat(129,1:n)*dat(130,1:n)**2 * msun*rsun**2 / (pc_g * msun**2 / rsun)
  
  ! 149: E_so = E_orb + E_spin (G Mo^2 / Ro):  
  dat(149,1:n) = dat(147,1:n) + dat(148,1:n)
  
  ! 150: E_bind (G Mo^2 / Ro) - CHECK: use tailored definition
  dat(150,1:n) = dat(15,1:n) * 1.d40 / (pc_g * msun**2 / rsun)
  
  ! 151: E_tot = E_so + E_bind (G Mo^2 / Ro):
  dat(151,1:n) = dat(149,1:n) + dat(150,1:n)
  
  
  ! 152: E_bind_env_grav + E_bind_env_int (10^40 erg)
  dat(152,1:n) = dat(84,1:n) + dat(85,1:n)
  
  ! 153: E_bind_env_recom + E_bind_int_H2ass (10^40 erg)
  dat(153,1:n) = dat(86,1:n) + dat(87,1:n)
  
  ! 154: E_bind_env_grav / E_bind_env_int
  dat(154,1:n) = abs(dat(84,1:n) / dat(85,1:n))
  
  ! 155: E_bind_env_recom / E_bind_int_H2ass
  dat(155,1:n) = abs(dat(86,1:n) / dat(87,1:n))
  
  ! 156: (E_bind_env_grav + E_bind_env_int) / (E_bind_env_recom + E_bind_int_H2ass)
  dat(156,1:n) = abs(dat(152,1:n) / dat(153,1:n))
  
  ! 157: lambda_env,gr = GMMenv/R / E_bind_env_grav 
  dat(157,1:n) = dat(141,1:n) / dat(84,1:n)
  
  ! 158: lambda_env,gr+int =  GMMenv/R / (E_bind_env_grav+E_bind_env_int)
  dat(158,1:n) = dat(141,1:n) / dat(152,1:n)
  
  ! 159, 160: Porb in hours and minutes
  dat(159,1:n) = dat(28,1:n) * 24.
  dat(160,1:n) = dat(28,1:n) * 1440.
  
  ! 161, 162: zeta_*, zeta_rl
  do i=2,n-1
     if(deq(dat(4,i+1),dat(4,i-1))) then
        dat(161,i) = dat(161,i-1)
        dat(162,i) = dat(162,i-1)
     else
        ! dlogR/dlogM = M/R dR/dM
        dat(161,i) = dat(4,i)/dat(8,i)   * (dat(8,i+1)   - dat(8,i-1))   / (dat(4,i+1) - dat(4,i-1))  ! zeta_*  =   dlogR/dlogM
        dat(162,i) = dat(4,i)/dat(127,i) * (dat(127,i+1) - dat(127,i-1)) / (dat(4,i+1) - dat(4,i-1))  ! zeta_rl = dlogRrl/dlogM
     end if
  end do
  dat(161,1) = dat(161,2)
  dat(162,1) = dat(162,2)
  dat(161,n) = dat(161,n-1)
  dat(162,n) = dat(162,n-1)
  
  ! 163: analytical zeta_ad
  call compute_zeta_ad(dat(5,1:n)/dat(4,1:n), dat(163,1:n))  ! Compute zeta_ad from the relative He core mass fraction
  
  ! 164 - 166: analytical zeta_rl
  beta = 0.0d0
  call compute_zeta_rl(dat(4,1:n),dat(40,1:n),beta, dat(164,1:n))  ! Compute zeta_rl from the component masses and beta=0.0
  beta = 0.5d0
  call compute_zeta_rl(dat(4,1:n),dat(40,1:n),beta, dat(165,1:n))  ! Compute zeta_rl from the component masses and beta=0.5
  beta = 1.0d0
  call compute_zeta_rl(dat(4,1:n),dat(40,1:n),beta, dat(166,1:n))  ! Compute zeta_rl from the component masses and beta=1.0
  
  ! 171 - 172: mass ratios:
  dat(171,:) = dat(4,:)/dat(40,:)  ! q_1 = M1/M2
  dat(172,:) = dat(40,:)/dat(4,:)  ! q_2 = M2/M1
  
  
  !*** Timescales:
  
  ! 201: Nuclear evolution timescale: 
  dat(201,1:n) = dat(4,1:n)*msun/1.9891/(dat(9,1:n)*lsun)*4.d10
  
  ! 202: KH timescale:
  dat(202,1:n) = pc_g*dat(4,1:n)**2*msun*msun / (dat(8,1:n)*rsun*dat(9,1:n)*lsun)/julyear
  
  ! 203: Mass transfer:
  dat(203,1:n) = dat(4,1:n)/max(abs(dat(33,1:n)),1.d-30)
  
  ! 204: Gravitational waves:
  dat(204,1:n) = dat(34,1:n)/max(dat(36,1:n)*julyear,1.d-30)
  
  ! 205: Magnetic braking (Actually SO-coupling!):
  !dat(205,1:n) = dat(34,1:n)/max(abs(dat(38,1:n))*julyear,1.d-30)
  
  ! 205 Dynamical: t ~ sqrt(R^3/(G*M)):
  dat(205,1:n) = sqrt(dat(8,1:n)**3/(pc_g*dat(4,1:n)))
  dpdt  = 0
  
  
  ! Replace dJ/dt by dP/dt:
  ! 35 = H_orb,  36 = H_gw, 37 = H_wml, 38 = H_s-o, 39 = H_mtr
  if(1.eq.2) then
     ! dP/dJ = 3/(m1m2)(2piP^2(m1+m2)/G^2)^1/3:
     dpdj(1:n) = 3.d0/(dat(4,1:n)*dat(40,1:n)*msun*msun)*(pi2* (dat(28,1:n)*solday)**2*(dat(4,1:n)+dat(40,1:n))*msun/(pc_g**2)) &
          **(1.d0/3.d0)
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
        dat(i,1:n) = dat(28,1:n)*solday/dat(i,1:n)/julyear
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







