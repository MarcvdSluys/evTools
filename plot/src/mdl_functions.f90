!> \file mdl_functions.f90  Routines to help plot the data contained in mdl[12] files

! AF, 21-08-2010

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




!***********************************************************************************************************************************
!> \brief  Shared variables for mdl[12] programs
module mdl_data
  implicit none
  save
  
  ! Constants:
  integer, parameter :: nn=2001, nq=400  !nq: max number of columns
  
  ! Variable labels:
  integer :: nv_der, nv_sp
  character :: pxns(0:nq)*99, pxfns(0:nq)*99, labels(nq)*99
  character :: abds(7)*99, nabs(3)*99, CEs(2)*99
end module mdl_data
!***********************************************************************************************************************************




!***********************************************************************************************************************************
!> \brief  Defines the variable labels for the mdl[12] format
subroutine set_mdl_labels
  use mdl_data
  implicit none
  
  abds = [character(len=99) :: 'H ','He','C ','N ','O ','Ne','Mg']    ! Line labels in abundances plot
  nabs = [character(len=99) :: 'ad ','rad','true']                    ! Line labels in nablas plot
  CEs  = [character(len=99) :: 'RLOF','\ga-CE']                       ! Line labels in CEs plot
  
  !Names of the variables in px
  pxns(0) = ''
  pxns(1:10)  = [character(len=99) :: 'Psi','P','Rho','T','k','Nad','Ntrue','Nrad-Nad','M','H']
  pxns(11:20) = [character(len=99) :: 'He','C','N','O','Ne','Mg','R','L','Eth','Enuc']
  pxns(21:30) = [character(len=99) :: 'Enu','dM','...','Thom','Uhom','Vhom','Uint','S','L/Ledd','wxl']
  pxns(31:40) = [character(len=99) :: 'mu','wt?','Nel','NeO','w?','MI','phi','Fm','DGOS','...']
  pxns(41:50) = [character(len=99) :: '...','LDRK','Enth','V^2','FAC','','','','','Rpp']
  pxns(51:60) = [character(len=99) :: 'Rpc','Rpng','Rpn','Rpo','Ran','','','','','N^2']
  
  pxns(201:210) = [character(len=99) :: 'Mesh pt','Nrad','m/M*','r/R*','C/O','Ne/O','Ugr-Uint','M.f.p.','n.dens','g']
  pxns(211:220) = [character(len=99) :: 'mu','n','Prad','Pgas','Pr/Pg','dM','Ub,*','Ub,env','P/rho','Prlof']
  pxns(221:221) = [character(len=99) :: 'Poace']
  pxns(301:303) = [character(len=99) :: 'Abundances','Nablas','CEs']
  
  !Names of the variables in px, to be used in output file name (no /.?*)
  pxfns(0) = ''
  pxfns(1:10)  = [character(len=99) :: 'Psi','P','Rho','T','k','Nad','Ntrue','Nrad-Nad','M','H']
  pxfns(11:20) = [character(len=99) :: 'He','C','N','O','Ne','Mg','R','L','Eth','Enuc']
  pxfns(21:30) = [character(len=99) :: 'Enu','dM','-','Thom','Uhom','Vhom','Uint','S','LLedd','wxl']
  pxfns(31:40) = [character(len=99) :: 'mu','wt','Nel','NeO','w','MI','phi','Fm','DGOS','-']
  pxfns(41:50) = [character(len=99) :: '-','LDRK','Enth','V2','FAC','-','-','-','-','Rpp']
  pxfns(51:60) = [character(len=99) :: 'Rpc','Rpng','Rpn','Rpo','Ran','-','-','-','-','N2']
  
  pxfns(201:210) = [character(len=99) :: 'Mesh pt','Nrad','mM','rR','CO','NeO','Ugr-Uint','Mfp','Ndens','g']
  pxfns(211:220) = [character(len=99) :: 'mu','n','Prad','Pgas','PrPg','dM','Ubst','Ubenv','Prho','Prlof']
  pxfns(221:221) = [character(len=99) :: 'Poace']
  pxfns(301:303) = [character(len=99) :: 'Abundances','Nablas','CEs']
  
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
  labels(9)  = 'm (M\d\(2281)\u)'
  labels(10) = 'H abundance'
  labels(11) = 'He abundance'
  labels(12) = 'C abundance'
  labels(13) = 'N abundance'
  labels(14) = 'O abundance'
  labels(15) = 'Ne abundance'
  labels(16) = 'Mg abundance'
  labels(17) = 'r (R\d\(2281)\u)'
  labels(18) = 'L (L\d\(2281)\u)'
  labels(19) = '\ge\dth\u'
  labels(20) = '\ge\dnucl\u'
  labels(21) = '\ge\d\gn\u'
  labels(22) = 'dM (M\d\(2281)\u)'
  labels(24) = 'T\dhom\u'
  labels(25) = 'U\dhom\u'
  labels(26) = 'V\dhom\u'
  labels(27) = 'E\dint\u (erg g\u-1\d)'
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
  labels(207) = 'E\dgr\u + E\dint\u'
  labels(208) = 'mean free path (cm)'
  labels(209) = 'n (cm\u-3\d)'
  labels(210) = 'g (cm s\u-2\d)'
  labels(211) = '\(2138)'  !\mu - mean molecular weight
  labels(212) = 'n (cm\u-3\d)'
  labels(213) = 'P\drad\u (dyn cm\u-2\d)'
  labels(214) = 'P\dgas\u (dyn cm\u-2\d)'
  labels(215) = '\(2128) = P\drad\u/P\dgas\u'  ! \beta - Prad/Pgas
  labels(216) = 'dM (M\d\(2281)\u)'            ! Mass of each shell
  !labels(217) = 'E\db,*\u (10\u40\d erg)'      ! Binding energy of the star
  !labels(218) = 'E\db,env\u (10\u40\d erg)'    ! Binding energy of the envelope
  labels(217) = 'E\db,*\u (GM\d\(2281)\u\u2\d/R\d\(2281)\u)'      ! Binding energy of the star
  labels(218) = 'E\db,env\u (GM\d\(2281)\u\u2\d/R\d\(2281)\u)'    ! Binding energy of the envelope
  labels(219) = 'P/\(2143) (cgs)'              ! P/rho
  labels(220) = 'P\dr(m)=Rrlof\u (day)'        ! Porb if r(m)=Rrl
  labels(221) = 'P\dpost-\ga-CE\u (day)'       ! Porb after \alpha CE
  
  nv_der = 221 - 200  !Number of derived variables
  
  
  
  !Special plots:
  labels(301) = 'Abundances'
  labels(302) = "\(2266)'s"
  labels(303) = 'P\dorb,post-CE\u (day)'
  
  nv_sp = 303 - 300  !Number of special plots
  
end subroutine set_mdl_labels
!***********************************************************************************************************************************
  
  


