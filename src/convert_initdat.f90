!> \file convert_initdat.f90  Read an old-format (pre-2009) ev init.dat file and spit out a namelist version.

! Use default values for new parameters


! Copyright 2002-2012 AstroFloyd - astrofloyd.org
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



!***********************************************************************************************************************************
!> \brief  Store the parameters in init.dat

module initdat
  use SUFR_kinds, only: double
  save
  
  real(double) :: eps, del, dh0, wanted_eps
  
  real(double) :: cdc_ms, cdc_hec, cdc_hes, cdc_dblsh, cdc5
  
  integer :: ke1, ke2, ke3, kbc, kev, kfn, kl, jh(3)
  integer :: kp_var(40), kp_eqn(40), kp_bc(40)
  
  integer :: kh2, kr1, kr2, jch!, kth, kx, ky, kz
  !integer :: kcl, kion, kam, kop, kcc, knuc, kcn
  !integer :: kt1, kt2, kt3, kt4, kt5, ksv
  integer :: kt5, ksv
  
  !integer :: kn, kjn(40)
  
  real(double) :: ct1, ct2, ct3
  real(double) :: ct(10)
  
  real(double) :: ch, cc, cn, co, cne, cmg, csi, cfe
  real(double) :: calp, cu, cos, cps, crd, cth, cgrs, ccac, csmc
  real(double) :: cdsi, cshi, cssi, cesc, cgsf, cfmu, cfc
  real(double) :: artmix
  
  real(double) :: cxb, cgr, cea, cet
  real(double) :: cmt, cms, cml, chl, ctf, clt
  real(double) :: cmi, cmr, cmj, cmv, cmk, cmnl
  real(double) :: cpa, cbr, csu, csd, cdf, cgw, cso, cmb
  real(double) :: cphotontire
  
  integer :: convection_scheme
  logical :: use_fudge_control, allow_extension, allow_underrelaxation
  logical :: allow_overrelaxation, allow_mdotrelaxation
  logical :: allow_egenrelaxation, use_previous_mu
  logical :: allow_avmurelaxation, use_quadratic_predictions
  
  real(double) :: convection_ledoux,smart_mass_loss,climit,enc_parachute
  real(double) :: multiplier_reimers,multiplier_vasiliadis_wood,multiplier_schroeder,multiplier_wachter,multiplier_achmad
  logical :: eos_include_pairproduction, accret_composition!Obsolete?
  
  real(double) :: off_centre_weight
  
  integer :: ke1_2, ke2_2, ke3_2, kbc_2, kev_2, kfn_2, kl_2, jh_2(3)
  integer :: kp_var_2(40), kp_eqn_2(40), kp_bc_2(40)
  
  
  !Not defined in init_dat_settings in stars:
  integer :: kh,nfunc,cmi_mode
  real(double) :: cdc(10),zscaling_mdot
  
  
  
  
  
  
  !From module "settings" in stars
  real(double) :: czs
  real(double) :: initial_ct(10)
  !real(double) :: cc, cn, co, cne, cmg, csi, cfe
  
  !real(double) :: calp, cu, cos, cps, crd, cth
  !real(double) :: cxb, cgr
  !real(double) :: cea, cet
  !real(double) :: cmt, cms, cmi, cml, chl, ctf, clt
  !real(double) :: cpa, cbr, csu, csd, cdf, cgw, cso, cmb
  
  !individual mass loss recipe switches.
  real(double) :: cmrr,cmvw,cmsc,cmw,cmal
  real(double) :: cmdotrot_hlw,cmdotrot_mm
  
  !non-conservative mass transfer options
  real(double) :: cmtel,cmtwl
  
  
  integer :: kth, kx, ky, kz
  integer :: kt1, kt2, kt3, kt4
  integer :: kcl, kion, kam, kop, kcc, knuc, kcn
  integer :: ksx(45)
  integer :: kn, kjn(40)
  
  ! number of allowed iterations for the nucleosynthesis code
  integer :: kr_nucsyn
  
  ! variables derived from settings and never changed
  real(double) :: clogz
  logical :: rigid_rotation
  
  ! switches for the new "smooth" remesher
  logical :: use_smooth_remesher,relax_loaded_model
  
  ! unused, but the code relies on this being defined:
  real(double) :: cq1, cq(17) 
  
  
  
  
  
  ! From read_init_dat in bbegin:
  real(double) :: ch_opac
  real(double) :: cdc_ems, cdc_hg, cdc_1dup, cdc_rlof, cdc_rlof_reduce, unused1, unused(17)
  
  real(double) :: x1ac, x4ac, x12ac, x14ac, x16ac, x20ac, x24ac
  real(double) :: xac(7, 2)
  
  
  
  ! From module zams_nucleosynthesis_abundances:
  real(double) :: cxd,cxhe3,cxli7,cxbe7,cxb11,cxc12,cxc13,cxc14,cxn14,cxn15,cxo16,cxo18,cxo17,cxf19,cxne21,cxne20,cxne22
  real(double) :: cxna22,cxna23,cxmg24,cxmg25,cxmg26,cxal26m,cxal27,cxal26g,cxsi28,cxsi30,cxsi29,cxp31,cxs33,cxs32,cxs34
  real(double) :: cxfe57,cxfe60,cxfe56,cxfe58,cxfe59,cxco59,cxni61,cxni59,cxni58,cxni60
  
end module initdat
!***********************************************************************************************************************************


!***********************************************************************************************************************************
program convert_initdat
  implicit none
  character :: infilename*(99),outfilename*(99)
  
  ! Read command-line variables:
  if(command_argument_count().eq.1) then
     call get_command_argument(1,infilename)
     outfilename = trim(infilename)//'.new'
  else if(command_argument_count().eq.2) then
     call get_command_argument(1,infilename)
     call get_command_argument(2,outfilename)
  else
     write(0,'(/,A,/)')'  Syntax:  convert_initdat <old file> [<new file>]'
     stop
  end if
  
  write(6,'(/,A)')'  Input file:   '//trim(infilename)
  write(6,'(A,/)')'  Output file:  '//trim(outfilename)
  
  call set_defaults()
  call read_old_initdat(trim(infilename))
  call write_new_initdat(trim(outfilename))
  
end program convert_initdat
!***********************************************************************************************************************************



!***********************************************************************************************************************************
subroutine set_defaults()
  use initdat, only: kh2,kh,jch,kl,jh, kt1,kt2,kt3,kt4,kt5,ksv, kth,kx,ky,kz, kcl,kion,kam,kcc,knuc,kcn, kr1,kr2
  use initdat, only: eps,del,dh0, use_quadratic_predictions
  use initdat, only: wanted_eps,use_fudge_control,allow_extension,allow_underrelaxation,allow_overrelaxation
  use initdat, only: allow_egenrelaxation,allow_mdotrelaxation,allow_avmurelaxation,use_previous_mu,off_centre_weight
  use initdat, only: cdc_ms,cdc_hec,cdc_hes,cdc_dblsh,cdc5, ct1,ct2,ct3, ke1,ke2,ke3,kbc,kev, kp_var,kp_eqn,kp_bc
  use initdat, only: cc,cn,co,cne,cmg,csi,cfe, ct, convection_scheme,calp,cu,cos,cps,crd,cxb,cgr,cth,cgrs,ccac
  use initdat, only: multiplier_reimers, multiplier_vasiliadis_wood, multiplier_schroeder, multiplier_wachter, multiplier_achmad

  use initdat, only: artmix, cdsi,cshi,cssi,cesc,cgsf, cea,cet, cmi_mode,zscaling_mdot
  use initdat, only: cmt,cms,cmi,cmr,cmj,cmv,cmk,cmnl,cml,chl,ctf,clt, cpa,cbr,csu,csd,cdf,cgw,cso,cmb, kn,kjn, cdc
  use initdat, only: ch_opac,cphotontire,cmv,cmk,cmnl,ch,artmix,cgrs,ccac,cdc,cdc_ems,cdc_hg,cdc_1dup,cdc_rlof
  use initdat, only: cdc_rlof_reduce,csmc,convection_ledoux,cmdotrot_hlw,cmdotrot_mm,cmtel,cmtwl
  use initdat, only: smart_mass_loss,cmrr,cmvw,cmsc,cmw,cmal, ccac,x1ac,x4ac,x12ac,x14ac,x16ac,x20ac,x24ac,cmi_mode
  use initdat, only: climit, wanted_eps, cphotontire, enc_parachute, eos_include_pairproduction, smart_mass_loss
  use initdat, only: kr_nucsyn, rigid_rotation, use_smooth_remesher,relax_loaded_model
  use initdat, only: zscaling_mdot, artmix, ccac, cgrs, csmc,  cdsi,cshi,cssi,cesc,cgsf, cfmu,cfc
  use initdat, only: cxd,cxhe3,cxli7,cxbe7,cxb11,cxc12,cxc13,cxc14,cxn14,cxn15,cxo16,cxo18,cxo17,cxf19,cxne21,cxne20,cxne22
  use initdat, only: cxna22,cxna23,cxmg24,cxmg25,cxmg26,cxal26m,cxal27,cxal26g,cxsi28,cxsi30,cxsi29,cxp31,cxs33,cxs32,cxs34
  use initdat, only: cxfe57,cxfe60,cxfe56,cxfe58,cxfe59,cxco59,cxni61,cxni59,cxni58,cxni60
  
  implicit none
  
  
  kh2   =  kh
  jch   =    2
  kl    =    1
  jh(1:3) =   (/ 0,0,0 /)
  
  kt1   =  100
  kt2   =    2    
  kt3   =    0    
  kt4   =    1 
  kt5   =    0
  ksv   = 1000
  
  kth   = 1 
  kx    = 1 
  ky    = 1 
  kz    = 1
  
  kcl   = 1
  kion  = 5
  kam   = 1
  !kop   = 4
  kcc   = 0
  knuc  = 0
  kcn   = 0
  
  kr1   = 20
  kr2   = 5
  
  eps   = 1.00e-006
  del   = 1.00e-002
  dh0   = 1.00e-007
  
  use_quadratic_predictions = .false.
  
  wanted_eps             = 1.00e-008
  use_fudge_control      = .true.
  allow_extension        = .false.
  allow_underrelaxation  = .false.
  allow_overrelaxation   = .false.
  allow_egenrelaxation   = .false.
  allow_mdotrelaxation   = .false.
  allow_avmurelaxation   = .false.
  use_previous_mu        = .true.
  off_centre_weight      = 1.0d16
  
  cdc_ms    = 0.01
  cdc_hec   = 0.25
  cdc_hes   = 1.00
  cdc_dblsh = 4.00
  cdc5      = 1.00
  
  ct1   =  0.8
  ct2   =  1.2
  ct3   =  0.3
  
  ke1   =  6
  ke2   =  5
  ke3   =  0
  kbc   =  3
  kev   =  1
  !kfn = nfunc    ! just copy all functions; common error to forget.  Since nfunc isn't set, we don't need this line
  
  kp_var(1:12)  =  (/ 7, 8, 4, 5, 3, 9,10,11,16, 1, 2, 6 /)
  kp_eqn(1:11)  =  (/ 1, 2, 3, 4, 5,13, 6, 7, 8, 9,10    /)
  kp_bc(1:12)   =  (/ 1, 2, 3, 4, 5,13, 6, 7, 8, 6, 7, 8 /)
  
  cc    =  0.176e+000
  cn    =  5.200e-002
  co    =  0.502e+000
  cne   =  9.200e-002
  cmg   =  3.400e-002
  csi   =  7.200e-002
  cfe   =  7.200e-002
  
  ct =  (/ 0.0e+00, 0.0e+00, 5.0e-02, 5.0e-02, 0.15e+00, 2.0e-02, 0.45e+0, 1.0e-04, 1.0e+15, 2.0e+04 /)
  
  convection_scheme = 1
  calp  =  2.000e+000
  cu    =  0.100e+000
  cos   =  0.120e+000
  cps   =  0.120e+000
  crd   =  1.000e-002
  cxb   =  0.150e+000
  cgr   =  1.000e-003
  cth   =  1.000e+000
  cgrs  =  0.000e+000
  ccac  =  0.000e+000
  
  artmix = 0.0
  
  cdsi  =  0.000e+000
  cshi  =  0.000e+000
  cssi  =  0.000e+000
  cesc  =  0.000e+000
  cgsf  =  0.000e+000
  
  cea   =  1.0e+02
  cet   =  1.0e-08
  
  cmi_mode = 1
  zscaling_mdot = 0.8
  
  cmt   =  0.0e0
  cms   =  0.0e0
  cmi   =  0.0e0
  cmr   =  0.0e0
  cmj   =  0.0e0
  cmv   =  0.0e0
  cmk   =  0.0e0
  cmnl  =  0.0e0
  cml   =  0.0e0
  chl   =  0.0e0
  ctf   =  0.0e0
  clt   =  0.0e0
  
  cpa   =  0.0e0
  cbr   =  0.0e0
  csu   =  0.0e0
  csd   =  0.0e0
  cdf   =  1.0e-02
  cgw   =  1.0e0
  cso   =  1.0e0
  cmb   =  0.0e0
  
  kn      = 12
  kjn(1:12) =  (/ 1,  2,  3,  5,  6,  7, 25, 26, 27, 29, 30, 31 /)
  
  ! export variables to common block
  cdc(1) = cdc_ms 
  cdc(2) = cdc_hec
  cdc(3) = cdc_hes
  cdc(4) = cdc_dblsh
  cdc(5) = cdc5
  
  
  
  
  !*********************************************************************************************************************************
  
  !From read_init_dat in bbegin:
  
  ! Default values:
  ch_opac = ch
  cphotontire = 0.0d0  ! do not keep track of photon tiring (do not take kinetic energy of the wind from the luminosity)
  cmv = 0.0d0    ! vink mass loss, disabled by default
  cmk = 0.0d0    ! kudritzki 2002 mass loss, disabled by default
  cmnl = 0.0d0   ! nugis&lamers, for wr stars, disabled by default
  ch = -1.0d0    ! use default value from opacity table 
  artmix = 0.0d0 ! no artificial mixing
  cgrs  = 0.0d0  ! no gravitational settling/diffusion
  ccac = 0.0d0   ! default (single star): don't accrete composition
  cdc(:) = 1.0d0 ! default timestep control
  cdc_ems = 1.0d0! timestep parameter at end of ms
  cdc_hg = 1.0d0 ! timestep parameter in hg
  cdc_1dup = 1.0d0! timestep parameter at 1dup
  cdc_rlof = 1.0d0! timestep parameter if star is close to rlof
  cdc_rlof_reduce = 1.0d0! timestep if star is close to rlof but contracting
  csmc = 0.04d0  ! efficiency of semi-convection
  convection_ledoux = 0.0d0  ! default: schwarzschild convection
  cmdotrot_hlw = 0.0d0 ! enhanced mass loss from heger, langer & woosely
  cmdotrot_mm = 0.0d0  ! enhanced mass loss from maeder & meynet
  cmtel = 0.0d0  ! no eddington-limited accretion
  cmtwl= 0.0d0   ! no angular momentum limited accretion
  
  ! improved mass loss rates, can be used in a general mass loss recipe
  ! instead of the current de jager rate. these individual mass loss
  ! recipes can be switched on and off there.
  smart_mass_loss = 0.0
  cmrr = multiplier_reimers
  cmvw = multiplier_vasiliadis_wood
  cmsc = multiplier_schroeder
  cmw = multiplier_wachter
  cmal = multiplier_achmad
  
  !if (ktw == 2) ccac = 1.0d0 ! default (binary/twin): do accrete composition
  ccac = 1.0d0 ! default (binary/twin): do accrete composition - ktw not defined here, but in init.run
  !              CHECK is ccac=1.0 ok for single-star run?
  x1ac = -1.0
  x4ac = -1.0
  x12ac = -1.0
  x14ac = -1.0
  x16ac = -1.0
  x20ac = -1.0
  x24ac = -1.0
  cmi_mode = 1
  
  
  
  
  
  
  
  climit = 1.0d-1    ! limit changes in variables during iterations
  
  ! desired accuracy
  wanted_eps = 1.0d-8
  
  cphotontire = 0.0  ! switch to include photon tiring
  
  ! emergency energy generation term, normally set to 0.
  ! this cannot be set from the input file. it will be set by remesh
  ! if there is no nuclear energy generation in the initial model at
  ! all. in that case, the first iteration(s) will return lom = 0.0
  ! throughout the star because the thermal energy term is initially
  ! 0 as well. this is a numerical fudge to remove the resulting
  ! singularity. this term will be set to l/m (constant energy
  ! generation throughout the star) and will be reduced to 0 by printb.
  enc_parachute = 0.0
  
  ! should the equation-of-state include the effects of pair production?
  ! this is only important in the very late burning stages of very massive
  ! stars. positrons are only calculated if their degeneracy parameter
  ! >= -15.0 - otherwise they are negligible anyway.
  eos_include_pairproduction = .false.
  
  !turn on smart mass loss routine
  smart_mass_loss = 0.0
  
  
  ! number of allowed iterations for the nucleosynthesis code
  kr_nucsyn = 60
  
  rigid_rotation = .true.     ! rigid rotation or differential rotation?
  
  ! switches for the new "smooth" remesher
  use_smooth_remesher = .false.
  relax_loaded_model = .true.
  
  
  
  ! scaling with metallicity applied to de jager mass loss rate in funcs1
  zscaling_mdot = 0.8
  
  artmix = 0.0d0  ! artificial mixing coefficient [cm^2/s]
  
  ccac = 0.0d0 ! switch for composition accretion
  
  cgrs = 0.0d0 ! switch for gravitational settling 
  
  csmc = 0.04d0! semi-convection efficiency, after langer (1991)
  
  
  cdsi = 1.0d0 ! switch for the dynamical shear instability
  cshi = 1.0d0 ! switch for the solberg-hoiland instability (not implmented)
  cssi = 1.0d0 ! switch for the secular shear instability
  cesc = 1.0d0 ! switch for the eddington-sweet circulation
  cgsf = 1.0d0 ! switch for the goldreich-schubert-fricke instability
  
  cfmu = 0.05d0 ! weight of mu gradient in rotational instabilities [see hegers thesis page 36 and pinsonneault]
  cfc = 1.0d0/30.0d0 ! ratio of tubulent viscosity over the diffusion coefficient [see hegers thesis page 35]
  
  
  
  
  !From module zams_nucleosynthesis_abundances:
  cxd     = 2.406e-03
  cxhe3   = 1.468e-03
  cxli7   = 4.687e-07
  cxbe7   = 0.000e+00
  cxb11   = 2.368e-07
  cxc12   = 1.764e-01
  cxc13   = 1.829e-03
  cxc14   = 0.000e+00
  cxn14   = 5.212e-02
  cxn15   = 2.186e-04
  cxo16   = 5.031e-01
  cxo18   = 1.086e-03
  cxo17   = 1.948e-04
  cxf19   = 2.030e-05
  cxne21  = 2.068e-04
  cxne20  = 9.221e-02
  cxne22  = 6.525e-03
  cxna22  = 0.000e+00
  cxna23  = 1.673e-03
  cxmg24  = 2.580e-02
  cxmg25  = 3.391e-03
  cxmg26  = 3.889e-03
  cxal26m = 0.000e+00
  cxal27  = 2.906e-03
  cxal26g = 0.000e+00
  cxsi28  = 3.272e-02
  cxsi30  = 1.179e-03
  cxsi29  = 1.717e-03
  cxp31   = 4.087e-03
  cxs33   = 1.615e-04
  cxs32   = 1.984e-02
  cxs34   = 9.351e-04
  cxfe57  = 1.431e-03
  cxfe60  = 0.000e+00
  cxfe56  = 5.858e-02
  cxfe58  = 1.853e-04
  cxfe59  = 0.000e+00
  cxco59  = 1.683e-04
  cxni61  = 4.307e-05
  cxni59  = 0.000e+00
  cxni58  = 2.478e-03
  cxni60  = 9.812e-04
  
  
end subroutine set_defaults
!***********************************************************************************************************************************




!***********************************************************************************************************************************
subroutine read_old_initdat(infilename)
  use initdat,only: kh2,kr1,kr2,jch,kth,kx,ky,kz, kcl,kion,kam,kop,kcc,knuc,kcn,kt1,kt2,kt3,kt4,kt5,ksv
  use initdat,only: eps,del,dh0,cdc_ms,cdc_hec,cdc_hes,cdc_dblsh,cdc5, ke1,ke2,ke3,kbc,kev,kfn,kl,jh,kp_var,kp_eqn,kp_bc
  use initdat,only: ke1_2,ke2_2,ke3_2,kbc_2,kev_2,kfn_2,kl_2,jh_2,kp_var_2,kp_eqn_2,kp_bc_2, ksx,kn,kjn,ct1,ct2,ct3,ct
  use initdat,only: cc,cn,co,cne,cmg,csi,cfe,  calp,cu,cos,cps,crd,cxb,cgr,cea,cet,  cmt,cms,cmi,cmr,cmj,cml,chl,ctf,clt
  use initdat,only: cpa,cbr,csu,csd,cdf,cgw,cso,cmb,unused1, cth,unused

  implicit none
  character, intent(in) :: infilename*(*)
  integer :: io
  
  open(unit=10,form='formatted',status='old',action='read',position='rewind',file=trim(infilename),iostat=io)
  if(io.ne.0) then
     write(0,'(A,/)')'  Error opening '//trim(infilename)//', aborting...'
     stop
  end if
  
  read(10, 993, iostat=io) kh2, kr1, kr2, jch, kth, kx, ky, kz,  &
       kcl, kion, kam, kop, kcc, knuc, kcn, kt1, kt2, kt3, kt4, kt5, ksv,   &
       eps, del, dh0, cdc_ms, cdc_hec, cdc_hes, cdc_dblsh, cdc5,  &
       ke1, ke2, ke3, kbc, kev, kfn, kl, jh, kp_var, kp_eqn, kp_bc,   &
       ke1_2, ke2_2, ke3_2, kbc_2, kev_2, kfn_2, kl_2, jh_2, kp_var_2, kp_eqn_2, kp_bc_2,  &
       ksx, kn, kjn, ct1, ct2, ct3, ct,   &
       cc, cn, co, cne, cmg, csi, cfe,   &
       calp, cu, cos, cps, crd, cxb, cgr, cea, cet,   &
       cmt, cms, cmi, cmr, cmj, cml, chl, ctf, clt,   &
       cpa, cbr, csu, csd, cdf, cgw, cso, cmb, unused1,  &
       cth, unused
  
993 format(8I5, /, 7I5, /, 6I5, /, 1P, 8D8.1, 0P, /, 2(10I4, /,  &
         6(20I3, /)), 3(15I3, /), I3, /, 2(20I3, /), 10F5.2, 1P, 3D8.1, /,  &
         0P, 7F6.3, /, 1P, 5(9D9.2, /), 0P)
  
  
  close(10)
  
  if(io.ne.0) then
     if(io.lt.0) write(0,'(A)')'  End of file reached before finishing read, aborting...'
     if(io.gt.0) write(0,'(A)')'  Error reading '//trim(infilename)//', aborting...'
     stop
  end if
  
end subroutine read_old_initdat
!***********************************************************************************************************************************




!***********************************************************************************************************************************
subroutine write_new_initdat(outfilename)
  use initdat, only: kh2,kr1,kr2,jch,kth,kx,ky,kz,  kcl,kion,kam,kop,kcc,knuc,kcn, kt1,kt2,kt3,kt4,kt5,ksv
  use initdat, only: eps,wanted_eps,del,dh0,cdc_ms,cdc_hec,cdc_hes,cdc_dblsh,cdc5,  cdc_ems,cdc_hg,cdc_1dup,cdc_rlof,cdc_rlof_reduce
  use initdat, only: ke1,ke2,ke3,kbc,kev,kfn,kl,jh,kp_var,kp_eqn,kp_bc
  use initdat, only: ke1_2,ke2_2,ke3_2,kbc_2,kev_2,kfn_2,kl_2,jh_2,kp_var_2,kp_eqn_2,kp_bc_2, ksx,kn,kjn
  use initdat, only: ct1,ct2,ct3,ct,  ch,cc,cn,co,cne,cmg,csi,cfe,  calp,cu,cos,cps,crd,cxb,cgr,cea,cet,  cmt,cms,cmi,cmr,cmj,cmv
  use initdat, only: cmk,cmnl,cmrr,cmvw,cmsc,cmw, cmal,smart_mass_loss,cml,chl,ctf,clt,cmdotrot_hlw,cmdotrot_mm
  use initdat, only: cmtel,cmtwl, cpa,cbr,csu,csd,cdf,cgw,cso,cmb,cphotontire,unused1, cth,unused,use_fudge_control,allow_extension
  use initdat, only: allow_underrelaxation,allow_overrelaxation,convection_scheme,allow_egenrelaxation
  use initdat, only: use_previous_mu,off_centre_weight,allow_mdotrelaxation, use_smooth_remesher,relax_loaded_model
  use initdat, only: convection_ledoux, allow_avmurelaxation,use_quadratic_predictions,climit, cmi_mode,zscaling_mdot,cdsi,cshi,cssi
  use initdat, only: cesc,cgsf,cfmu,cfc,artmix,cgrs,ccac,csmc
  
  !use initdat, only: x1ac,x4ac,x12ac,x14ac,x16ac,x20ac,x24ac,accret_composition
  
  !use initdat, only: cxd,cxhe3,cxli7,cxbe7,cxb11,cxc12,cxc13, cxc14,cxn14,cxn15,cxo16,cxo18,cxo17,cxf19,cxne21,cxne20,cxne22
  !use initdat, only: cxna22,cxna23,cxmg24,cxmg25,cxmg26,cxal26m,cxal27, cxal26g,cxsi28,cxsi30,cxsi29,cxp31,cxs33,cxs32,cxs34,cxfe57
  !use initdat, only: cxfe60,cxfe56,cxfe58,cxfe59,cxco59,cxni61,cxni59,cxni58, cxni60
  
  implicit none
  integer :: io
  character, intent(in) :: outfilename*(*)
  
  namelist /init_dat/ kh2, kr1, kr2, jch, kth, kx, ky, kz,   &
       kcl, kion, kam, kop, kcc, knuc, kcn,  &
       kt1, kt2, kt3, kt4, kt5, ksv,  &
       eps, wanted_eps, del, dh0, cdc_ms, cdc_hec, cdc_hes, cdc_dblsh, cdc5,   &
       cdc_ems, cdc_hg, cdc_1dup, cdc_rlof, cdc_rlof_reduce,  &
       ke1, ke2, ke3, kbc, kev, kfn, kl, jh, kp_var, kp_eqn, kp_bc,   &
       ke1_2, ke2_2, ke3_2, kbc_2, kev_2, kfn_2, kl_2, jh_2, kp_var_2, kp_eqn_2, kp_bc_2,  &
       ksx, kn, kjn,   &
       ct1, ct2, ct3, ct,   &
       ch, cc, cn, co, cne, cmg, csi, cfe,   &
       calp, cu, cos, cps, crd, cxb, cgr, cea, cet,  &
       cmt, cms, cmi, cmr, cmj, cmv, cmk, cmnl, cmrr, cmvw, cmsc, cmw,  &
       cmal, smart_mass_loss, cml, chl, ctf, clt, cmdotrot_hlw, cmdotrot_mm,  &
       cmtel, cmtwl,  &
       cpa, cbr, csu, csd, cdf, cgw, cso, cmb, cphotontire, unused1,  &
       cth, unused, use_fudge_control, allow_extension, allow_underrelaxation,  &
       allow_overrelaxation, convection_scheme, allow_egenrelaxation,  &
       use_previous_mu, off_centre_weight, allow_mdotrelaxation,  &
       use_smooth_remesher, relax_loaded_model, convection_ledoux,  &
       allow_avmurelaxation, use_quadratic_predictions, climit,  &
       cmi_mode, zscaling_mdot, cdsi, cshi, cssi,  &
       cesc, cgsf, cfmu, cfc, artmix, cgrs, ccac, csmc
  
  ! namelist for the accretion of matter
  !namelist /accret/ x1ac, x4ac, x12ac, x14ac, x16ac, x20ac, x24ac, accret_composition
  
  ! namelist for initial (zams) nucleosynthesis abundances
  !namelist /abund/ cxd, cxhe3, cxli7, cxbe7, cxb11, cxc12, cxc13,  &
  !     cxc14, cxn14, cxn15, cxo16, cxo18, cxo17, cxf19, cxne21, cxne20,  &
  !     cxne22, cxna22, cxna23, cxmg24, cxmg25, cxmg26, cxal26m, cxal27,  &
  !     cxal26g, cxsi28, cxsi30, cxsi29, cxp31, cxs33, cxs32, cxs34, cxfe57,  &
  !     cxfe60, cxfe56, cxfe58, cxfe59, cxco59, cxni61, cxni59, cxni58,  &
  !     cxni60
  
  
  open(unit=10,form='formatted',status='replace',action='write',position='rewind',file=trim(outfilename),iostat=io)
  if(io.ne.0) then
     write(0,'(A,/)')'  Error opening '//trim(outfilename)//', aborting...'
     stop
  end if
  
  write(10, nml=init_dat, iostat=io)
  if(io.ne.0) then
     write(0,'(A,/)')'  Error writing init_dat namelist to '//trim(outfilename)//', aborting...'
     stop
  end if
  
  
  ! There's no need to output the two namelists below, since they are not set in the old init.dat files, 
  !   and hence contain default values anyway (right?)
  
  ! Try for the accretion information in the same file:
  ! This is a bit ugly, but do we want to use another fort.nnn file for this?
  !write(10, nml=accret, iostat=io)
  !if(io.ne.0) then
  !   write(0,'(A,/)')'  Error writing accret namelist to '//trim(outfilename)//', aborting...'
  !   stop
  !end if
  
  
  ! Finally, write the nucleosynthesis abundances:
  !write(10, nml=abund, iostat=io)
  !if(io.ne.0) then
  !   write(0,'(A,/)')'  Error writing abund namelist to '//trim(outfilename)//', aborting...'
  !   stop
  !end if
  
  close(10)
  
end subroutine write_new_initdat
!***********************************************************************************************************************************
