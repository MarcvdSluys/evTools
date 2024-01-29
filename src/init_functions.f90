!> \file init_functions.f90  Procedures to deal with init.dat and init.run.
!!
!! 2024-01-27, MvdS: initial version - adapted from ev initrun.f90.


! Copyright 2002-2024 Marc van der Sluys - marc.vandersluys.nl
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
!> \brief  Procedures to deal with init.run.

module init_run
  use SUFR_kinds, only: double
  use SUFR_system, only: find_free_io_unit, file_open_error_quit, file_read_error_quit, file_write_error_quit
  
  
  implicit none
  save
  
  integer :: isb, ktw       ! Binary and/or TWIN mode
  integer :: ip1,ip2        ! Input unit for initial model for star 1,2 (13-16)
  integer :: im1,im2        ! Model number for initial model for star 1,2
  
  integer :: kpt            ! Maximum number of timesteps for each component
  integer :: kp             ! Number of timesteps before switching to the other component in a non-TWIN binary
  
  integer :: kml, kql, kxl  ! Number of iterations to perform in the primary-mass/mass-ratio/orbital-period loops
  
  integer :: kr             ! Used to set initial rotation of the star (see user manual)
  integer :: jmx            ! Starting model number (see user manual)
  
  
  real(double) :: ml1, dml  ! log(M) in the first, and dlog(M) to add in the next iterations in the primary-mass loop
  real(double) :: ql1, dql  ! log(q) in the first, and dlog(q) to add in the next iterations in the mass-ratio loop
  real(double) :: xl1, dxl  ! log(P) in the first, and dlog(P) to add in the next iterations in the orbital-period loop
  
  real(double) :: rot       ! Initial rotation (see user manual)
  real(double) :: ex        ! Initial exentricity
  
  real(double) :: sm, dty, age, per, bms, ecc, p, enc  ! Set exact values for primary mass, time step, age, Porb, binary mass, eccentricity, rotation period and artificial energy rate
  
  ! Conditions for triggering special behaviour, for instance terminating the code. Note that in many cases
  ! the code is not actually stopped unless the timestep criterion is also satisfied.
  !  1: rlf1   2: age    3: LCarb     4: rlf2    5: LHe      6: rho    7: MCO 
  !  8: rho    9: mdot  10: XHe      11: He-eps 12: dtmin   13: sm8   14: vmh8
  ! 15: XH    16: Rmax  17: LHestop  18-21: not yet used
  real(double) :: uc(21)
  
  character(len=500) :: startfile    ! File name to read starting model from (*1)
  character(len=500) :: startfile2   ! File name to read starting model from (*2)
  
  logical :: start_model_only, composition_only, start_with_rigid_rotation, stop_at_target_mass
  
contains
  
  
  !*********************************************************************************************************************************
  !> \brief Read an init.run file in namelist format
  !! 
  !! \param infile  Input file name.
  
  subroutine read_init_run(infile)
    implicit none
    character, intent(in) :: infile*(*)
    integer :: ip, iostat
    
    ! Namelist for init.run:
    namelist /init_run/ isb, ktw, ip1, im1, ip2, im2, kpt, kp, ml1, dml, kml,  &
         ql1, dql, kql, xl1, dxl, kxl, rot, kr, ex,  &
         sm, dty, age, per, bms, ecc, p, enc, jmx,  &
         uc, start_model_only, composition_only, startfile, startfile2,  &
         start_with_rigid_rotation, stop_at_target_mass
    
    call find_free_io_unit(ip)
    open(unit=ip, form='formatted', status='old', file=trim(infile), iostat=iostat)
    
    ! First try to use namelist I/O:
    startfile = ''
    startfile2 = ''
    start_model_only = .true.
    composition_only = .false.
    
    read(ip, nml=init_run, iostat=iostat)
    
    
    ! If that fails use old-fashioned I/O:
    if(iostat.ne.0) then
       write(*,'(A)') 'Reading namelist format failed, trying old format'
       rewind(ip)
       read(ip, *, iostat=iostat) isb, ktw, ip1, im1, ip2, im2, kpt, kp, ml1, dml, kml,   &
            ql1, dql, kql, xl1, dxl, kxl, rot, kr, ex, &
            sm, dty, age, per, bms, ecc, p, enc, jmx, uc
       
       if(iostat.ne.0) then
          write(*,'(A,/)') 'Reading old format failed, aborting...'
          stop
       end if
       
    end if
    
    close(ip)
    
  end subroutine read_init_run
  !*********************************************************************************************************************************
  
  !*********************************************************************************************************************************
  !> \brief Read an init.run file in namelist format
  !! 
  !! \param op  Output file unit
  
  subroutine write_init_run(outfile)
    implicit none
    character, intent(in) :: outfile*(*)
    integer :: op, iostat
    
    call find_free_io_unit(op)
    open(unit=op, form='formatted', status='replace', file=trim(outfile), iostat=iostat)
    
    write(op,'("! This file contains parameters that control a normal run.  See the section init.run in the manual.")')
    
    ! Mode and start models:
    write(op,'("")')
    write(op,'("&INIT_RUN")')
    write(op,'(" ISB  = ",I9,",    KTW  = ",I9,"    ! Evolve one or two stars / normal or twin mode")') isb, ktw
    write(op,'(" IP1  = ",I9,",    IM1  = ",I9,"    ! File for initial model (16=ZAMS) / model number")') ip1, im1
    write(op,'(" IP2  = ",I9,",    IM2  = ",I9,"    ! ... same for star 2")') ip2, im2
    
    ! Number of timesteps:
    write(op,'(" ")')
    write(op,'(" KPT  = ",I9,",    KP   = ",I9,"    ! max. number of timesteps for each component / for *1")') kpt, kp
    
    ! Start files:
    write(op,'(" ")')
    write(op,'(" STARTFILE  = ",A)') "'"//trim(startfile)//"'"
    write(op,'(" STARTFILE2 = ",A)') "'"//trim(startfile2)//"'"
    
    ! Grid of M1/q/Porb:
    write(op,'(" ")')
    write(op,'(" ! Grid of log(M1), log(q) and log (P_orb/P_zams,rlof):")')
    write(op,'(" ML1  = ",ES9.2,",  DML  = ",ES9.2,",  KML  = ",I4,"    ! log(M1):    grid start / increment / number")') &
         ml1, dml, kml
    write(op,'(" QL1  = ",ES9.2,",  DQL  = ",ES9.2,",  KQL  = ",I4,"    ! log(q):     grid start / increment / number")') &
         ql1, dql, kql
    write(op,'(" XL1  = ",ES9.2,",  DXL  = ",ES9.2,",  KXL  = ",I4,"    ! log(Porb):  grid start / increment / number")') &
         xl1, dxl, kxl
    
    ! Spin and eccentricity:
    write(op,'(" ")')
    write(op,'(" ROT  = ",F9.2,",          KR   = ",I3,"    ! Initial spin period *1: value / type")') rot, kr
    write(op,'(" EX   = ",F9.2,"                         ! Initial orbital eccentricity")') ex
    
    ! Exact initial parameters:
    write(op,'(" ")')
    write(op,'(" ! Specify exact initial parameters:")')
    write(op,'(" SM   = ",F9.2,"    ! Primary mass")') sm
    write(op,'(" DTY  = ",F9.2,"    ! Timestep")') dty
    write(op,'(" AGE  = ",F9.2,"    ! Binary age")') age
    write(op,'(" PER  = ",F9.2,"    ! Orbital period")') per
    write(op,'(" BMS  = ",F9.2,"    ! Binary mass")') bms
    write(op,'(" ECC  = ",F9.2,"    ! Orbital eccentricity")') ecc
    write(op,'(" P    = ",F9.2,"    ! Rotational period *1")') p
    write(op,'(" ENC  = ",F9.2,"    ! Artificial energy rate")') enc
    write(op,'(" JMX  = ",I9,  "    ! New model number (JMOD)")') jmx
    
    ! Rigid rotation, target mass:
    write(op,'(" ")')
    if(start_with_rigid_rotation) then
       write(op,'(" START_WITH_RIGID_ROTATION = .true.     ! Start model with rigid rotation")')
    else
       write(op,'(" START_WITH_RIGID_ROTATION = .false.    ! Start model with rigid rotation")')
    end if
    write(op,'(" ")')
    if(stop_at_target_mass) then
       write(op,'(" STOP_AT_TARGET_MASS = .true.           ! Stop the code when mass reaches target mass")')
    else
       write(op,'(" STOP_AT_TARGET_MASS = .false.          ! Stop the code when mass reaches target mass")')
    end if
    
    ! "Termination" conditions:
    write(op,'(" ")')
    write(op,'(" ! Conditions for triggering special behaviour, for instance terminating the")')
    write(op,'(" ! code. Note that in many cases the code is not actually stopped unless the")')
    write(op,'(" ! timestep criterion is also satisfied.")')
    write(op,'(" !    1: rlf1      2: age       3: LCarb     4: rlf2      5: LHe       6: rho       7: MCO ")')
    write(op,'(" !    8: rho       9: mdot     10: XHe      11: He-eps   12: dtmin    13: sm8      14: vmh8")')
    write(op,'(" !   15: XH       16: Rmax     17: LHestop  18: -        19: -        20: -        21: -   ")')
    write(op,'(" UC = ")')
    write(op,'("  ",7(ES12.3,","))')         uc(1:7)
    write(op,'("  ",7(ES12.3,","))')         uc(8:14)
    write(op,'("  ",6(ES12.3,","),ES12.3)')  uc(15:21)
    write(op,'("/")')
    
    close(op)
    
  end subroutine write_init_run
  !*********************************************************************************************************************************

end module init_run
!***********************************************************************************************************************************



