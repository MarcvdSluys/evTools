!> \file mdl_functions.f90  Routines to help plot the data contained in mdl[12] files

! AF, 21-08-2010

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
!> \brief  Shared variables for mdl[12] programs
module mdl_data
  implicit none
  save
  
  ! Constants:
  integer, parameter :: nn=501, nq=400  ! nn: max number of lines, nq: max number of columns
  
  integer :: nc, nmsh, nv, nm
  integer :: pxnr(nq), pxin(nq)
  real :: mdlver
  
  ! Variable labels:
  integer :: nv_der, nv_sp
  character :: pxns(0:nq)*(99), pxfns(0:nq)*(99), labels(nq)*(99)
  character :: abds(7)*(99), nabs(3)*(99), CEs(3)*(99)
end module mdl_data
!***********************************************************************************************************************************





!***********************************************************************************************************************************
!> \brief  Compute secondary variables from the primary variables in a .mdl[12] file
!! \param  dat  Data array (in/out)
subroutine compute_mdl_variables(dat)
  use SUFR_kinds, only: double
  use SUFR_constants, only: rsun,msun,pc_g,pc_amu,pc_mh,c3rd,pc_arad,pc_kb,solday
  use mdl_data, only: pxin,pxnr,nq,nm,nc
  
  implicit none
  real(double), intent(inout) :: dat(nq,nm)
  integer :: i,j
  real(double) :: rl2a
  real(double) :: m1,m1i,m2i,r1i, Mbin,Mbini, Mc
  real(double) :: dE,Eorb,Eorbi, a_orb,a_orbi, Porb, Jorb,Jorbi, alphaCE,gammaCE
  
  
  ! Create inverse pxnr index, pxin:  if pxin(i) = 0, then the variable px(i) is not in the file
  do i=1,nc
     if(pxnr(i).gt.0) pxin(pxnr(i)) = i
  end do
  
  do i=1,nm
     dat(201,i) = dble(i)     ! Mesh point (1 = centre)
     dat(202,i) = dble(nm-i)  ! Reversed mesh point (1 = surface)
  end do
  !Difference between Nabla_rad and Nabla_ad, +1 or -1, +1: convection, calculate after Nabla_rad:
  dat(8,1:nm)   = dat(pxin(8),1:nm)/abs(dat(pxin(8),1:nm))
  dat(203,1:nm) = dat(pxin(9),1:nm)/dat(pxin(9),nm)                                     ! M/M*
  dat(204,1:nm) = dat(pxin(17),1:nm)/dat(pxin(17),nm)                                   ! R/R*
  dat(205,1:nm) = dat(pxin(12),1:nm)/dat(pxin(14),1:nm)                                 ! C/O
  dat(206,1:nm) = dat(pxin(13),1:nm)/dat(pxin(14),1:nm)                                 ! Ne/O
  dat(207,1:nm) = -pc_g*dat(pxin(9),1:nm)*msun/(dat(pxin(17),1:nm)*rsun) + dat(pxin(27),1:nm)  ! Ugr + Uint  
  dat(208,1:nm) = 1.d0/(dat(pxin(3),1:nm)*dat(pxin(5),1:nm))                            ! Mean free path = 1/(rho * kappa)
  if(pxin(31).ne.0) then
     dat(209,1:nm) = dat(pxin(2),1:nm)/(dat(pxin(31),1:nm)*pc_amu)                !n = rho / (mu * pc_amu)
     pxnr(209) = 209
  end if
  dat(210,1:nm) = real(pc_g*dble(dat(pxin(9),1:nm))*msun/(dble(dat(pxin(17),1:nm))**2*rsun**2))  ! n = rho / (mu * pc_amu)
  pxnr(201:210) = (/201,202,203,204,205,206,207,208,209,210/)
  
  !Mean molecular weight mu = 2/(1 + 3X + 0.5Y), Astrophysical Formulae I, p.214, below Eq. 3.62):
  dat(211,1:nm) = 2.d0 / (1.d0 + 3*dat(9,1:nm) + 0.5d0*dat(10,1:nm))
  dat(212,1:nm) = dat(4,1:nm)/(dat(211,1:nm)*pc_mh)                                     ! Particle density       n = rho/(mu*m_H)
  dat(213,1:nm) = pc_arad*dat(5,1:nm)**4*c3rd                                           ! P_rad = aT^4/3
  dat(214,1:nm) = dat(212,1:nm)*pc_kb*dat(5,1:nm)                                       ! P_gas = nkT
  dat(215,1:nm) = dat(213,1:nm)/(dat(214,1:nm)+1.d-30)                                  ! beta = Prad/Pgas
  
  !216-217: binding energy:
  dat(216,1:nm) = 0.0d0
  dat(217,1:nm) = 0.0d0
  dat(218,1:nm) = 0.0d0
  do i=2,nm
     dat(216,i) = dat(pxin(9),i) - dat(pxin(9),i-1)                                     ! Mass of the current shell (Mo)
     !dE = dat(207,i)*dat(216,i) * msun*1.d-40                                          ! BE of the shell (10^40 erg)
     dE = dat(207,i)*dat(216,i) * msun / (PC_G*msun**2/rsun)                            ! BE of the shell       (G Mo^2 / Ro)
     dat(217,i) = dat(217,i-1) + dE                                                     ! BE of the whole star  (same units)
     if(dat(pxin(10),i).gt.0.1d0) dat(218,i) = dat(218,i-1) + dE                        ! BE of envelope        (same units)
  end do
  do i=nm-1,1,-1
     if(abs(dat(218,i)).lt.1.d-19) dat(218,i) = dat(218,i+1)                            ! Set the core BE to first non-zero BE
  end do
  
  dat(219,1:nm) = dat(3,1:nm)/dat(4,1:nm)                                               ! P/rho
  
  
  
  ! *** COMMON ENVELOPES ***
  
  
  m1i = dat(pxin(9),nm)                                                                  ! (Initial) total mass of star 1
  m2i = m1i                                                                              ! (Initial) total mass of star 2
  if(1.eq.2) then
     m2i = 0.25                                                                          ! CHECK: using custom M2
     write(0,'(/,A,/)')'  *** Warning: using custom value for M2 ***'
  end if
  Mbini = m1i+m2i                                                                        ! (Initial) binary mass
  r1i = dat(pxin(17),nm)                                                                 ! (Initial) surface radius of star 1
  
  a_orbi = rl2a(m1i,m2i,r1i)                                                             ! a_orb (Ro)
  Eorbi = -m1i*m2i/(2*a_orbi)                                                            ! Eorb (G Mo^2 / Ro)
  Jorbi = m1i*m2i*sqrt(a_orbi/Mbini)                                                     ! Jorb,i (G^1/2 Mo^3/2 Ro^1/2)
  
  alphaCE = 1.d0
  gammaCE = 1.5d0
  
  
  ! r(m) = Rrl:
  dat(220:223,:) = 0.d0
  do i=2,nm
     m1 = dat(pxin(9),i)
     Mbin = m1+m2i
     a_orb = rl2a( m1, m2i, dat(pxin(17),i) )                                            ! a_orb (Ro)
     call a2p( Mbin*msun, a_orb*rsun, Porb)                                                  ! Porb (s)
     Porb = Porb/solday                                                                     ! Porb (day)
     Eorb = m1*m2i/(2*a_orb)                                                             ! Eorb (G Mo^2 / Ro)
     Jorb = m1*m2i*sqrt(a_orb/Mbin)                                                      ! Jorb (G^1/2 Mo^3/2 Ro^1/2)
     dat(220,i) = a_orb
     dat(221,i) = Porb
     dat(222,i) = Eorb
     dat(223,i) = Jorb
  end do
  pxnr(211:220) = (/211,212,213,214,215,216,217,218,219,220/)
  
  
  ! alpha-CE:
  dat(224:227,:) = 0.d0
  do i=2,nm
     m1 = dat(pxin(9),i)
     Mbin = m1+m2i
     Eorb = Eorbi + dat(217,nm) - dat(217,i)/alphaCE                                     ! Eorb (G Mo^2 / Ro)
     a_orb = -m1*m2i/(2*Eorb)                                                            ! a_orb (Ro)
     call a2p(Mbin*msun,a_orb*rsun,Porb)                                                     ! Porb (s)
     Porb = Porb/solday                                                                     ! Porb (day)
     Jorb = m1*m2i*sqrt(a_orb/Mbin)                                                      ! Jorb (G^1/2 Mo^3/2 Ro^1/2)
     
     dat(224,i) = a_orb
     dat(225,i) = Porb
     dat(226,i) = Eorb
     dat(227,i) = Jorb
  end do
  
  
  ! gamma-CE:
  dat(228:231,:) = 0.d0
  do i=2,nm
     m1 = dat(pxin(9),i)
     Mbin = m1+m2i
     Jorb = Jorbi * (1.d0 - gammaCE*(m1i-m1)/Mbini)
     a_orb = Mbin * (Jorb/(m1*m2i))**2                                                   ! Jorb,i (G^1/2 Mo^3/2 Ro^1/2)
     call a2p(Mbin*msun,a_orb*rsun,Porb)                                                     ! Porb (s)
     Porb = Porb/solday                                                                     ! Porb (day)
     Eorb = m1*m2i/(2*a_orb)                                                             ! Eorb (G Mo^2 / Ro)
     !Eorb = Eorbi + dat(217,nm) - dat(217,i)/alphaCE                                    ! Eorb (G Mo^2 / Ro)  FOR ALPHA_CE !!!
     
     dat(228,i) = a_orb
     dat(229,i) = Porb
     dat(230,i) = Eorb
     dat(231,i) = Jorb
  end do
  
  ! Get a minimum core mass: X<1.e-10:
  Mc = 0.d0
  do i=nm,1,-1
     !if(dat(pxin(10),i).gt.0.1) Mc = dat(pxin(9),i)
     if(dat(pxin(10),i).gt.1.d-10) Mc = dat(pxin(9),i)
  end do
  
  ! Make sure dat(220-231,1) are not 0:
  do i=nm-1,1,-1
     do j=220,231
        !if(dat(j,i).eq.0.d0) dat(j,i) = dat(j,i+1)
        if(dat(pxin(9),i).lt.Mc) dat(j,i) = dat(j,i+1)  ! Stop at the core-envelope boundary
     end do
  end do
  
  pxnr(221:230) = (/221,222,223,224,225,226,227,228,229,230/)
  
  dat(232,1:nm) = dat(pxin(6),1:nm) + dat(pxin(8),1:nm)                                 ! Nabla_rad
  
  pxnr(231:232) = (/231,232/)
  
  
  if(pxin(60).ne.0) then                                                                ! Brint-Vailasakatralala frequency
     dat(pxin(60),1:nm) = abs(dat(pxin(60),1:nm))
  end if
  
  pxnr(301:305) = (/301,302,303,304,305/)  ! Abundances, Nablas, CEs
  
end subroutine compute_mdl_variables
!***********************************************************************************************************************************


  
!***********************************************************************************************************************************
!> \brief  Read all structure models in the file and display main properties
!!
!! \param infile  Name of the .mdl[12] input file
!! \retval nblk   Number of stellar-structure blocks in the file

subroutine list_mdl_models(infile,nblk)
  use SUFR_constants, only: pc_g
  use SUFR_dummy, only: dmrl=>dumreal, dumstr
  use mdl_data, only: pxnr,nc, nmsh,nv,mdlver
  
  implicit none
  character, intent(in) :: infile*(*)
  integer, intent(out) :: nblk
  
  integer :: nmdl
  integer :: bl,mp,io
  real :: age,vk,mm1,be,be1
  real :: mm,rr,rrh,tt,hh,hhe,ccc,oo  ! ,kk,mmg,nnrad,nnn,nne,nnad,pp
  real :: ll  ! ,eeth,eenc,eenu,ss,uuint
  real :: m1,r1,l1,ts,tc,mhe,mco,rhoc
  real :: hc,hec,cc,oc, hs,hes,zs  ! ,cs,os,zc
  
  
  
  write(6,*)''
  write(6,'(A)')'  Reading file '//trim(infile)
  open(unit=10,form='formatted',status='old',file=trim(infile))
  
  read(10,'(2x,I4,4x,I2,F7.3)',iostat=io) nmsh,nv,mdlver
  if(io.ne.0) then
     write(0,'(A,/)')'1  Error reading first line (header) of the file, aborting...'
     close(10)
     stop
  end if
  
  if(mdlver.gt.1.) then
     read(10,*) dumstr
  else
     nc = 21
     pxnr(1:nc)=(/9,17,2,3,4,5,6,8,10,11,12,13,14,15,16,18,19,20,21,28,27/)!,50,51,52,53,54,55,31,7,24,25,26,60
  end if
  
  if(nmsh.eq.0) then ! Then post-2005 version output
     rewind(10)
  end if
  
  write(6,*)''
  write(6,'(A)')'  Nr  Model Nmsh          Age        M1   Mhe   Mco     Menv         R        L     Teff       Tc     Rhoc'// &
       '      Xc     Yc     Cc     Oc     Xs    Ys    Zs   k^2'
  
  mp = 1  !Silence compiler warnings
  bl = 1
  block: do 
     if(mod(bl,25).eq.0) then
        write(6,*)''
        write(6,'(A)')'  Nr  Model Nmsh          Age        M1   Mhe   Mco     Menv         R        L     Teff       Tc'// &
             '     Rhoc      Xc     Yc     Cc     Oc     Xs    Ys    Zs   k^2'
     end if
     read(10,'(I6,1x,ES16.9)',iostat=io) nmdl,age
     if(io.lt.0) then
        write(0,'(A,I5,A)')'  Model',bl,' seems incomplete, skipping...'
        exit block !EOF
     end if
     if(io.gt.0) then  !Error
        write(0,'(A,I5,A,/)')'2  Error reading first line (header) of model block',bl,', aborting...'
        close(10)
        stop
     end if
     
     
     mhe = 0.
     mco = 0.
     vk = 0.
     mm1 = 0.
     be = 0.
     be1 = 0.
     
     if(nmsh.eq.0) nmsh = 199
     mesh: do mp=1,nmsh
        read(10,'(ES13.6,4ES11.4,16ES11.3)',iostat=io) &
             !mm,rr,pp,rrh,tt,kk,nnad,nnrad,hh,hhe,ccc,nnn,oo,nne,mmg,ll,eeth,eenc,eenu,ss,uuint
             mm,rr,dmrl,rrh,tt,dmrl,dmrl,dmrl,hh,hhe,ccc,dmrl,oo,dmrl,dmrl,ll,dmrl,dmrl,dmrl,dmrl,dmrl
        !print*,bl,mp,io
        if(io.lt.0) then
           write(0,'(A,I5,A)')'  Model',bl,' seems incomplete, skipping...'
           exit block !EOF
        end if
        if(io.gt.0) then  !Error
           write(0,'(A,2(I5,A),/)')'  Error reading model',bl-1,'line',mp-1,', aborting...'
           close(10)
           stop
        end if
        
        if(mp.eq.1) then
           tc  = tt
           hc  = hh
           hec = hhe
           cc = ccc
           oc = oo
           !zc  = 1. - hh - hhe
           rhoc = rrh
        end if
        if(mp.eq.nmsh) then
           m1  = mm
           r1  = rr
           l1  = ll
           ts  = tt
           hs  = hh
           hes = hhe
           dmrl = ccc  ! cs = ccc
           dmrl = oo   ! os = oo
           zs  = 1. - hh - hhe
        end if
        if(mhe.eq.0.0.and.hh.gt.0.1) mhe = mm
        if(mco.eq.0.0.and.hhe.gt.0.1) mco = mm
        
        ! Calculate V.K. of the envelope:
        if(mp.gt.2.and.hh.gt.0.1) then
           vk = vk + (mm-mm1)*rr**2
           be = be + real(pc_g*(mm-mm1)*mm1/rr*5.6847d15)    ! in 10^40 erg
        end if
        if(mp.gt.2.and.hh.gt.0.001) then
           be1 = be1 + real(pc_g*(mm-mm1)*mm1/rr*5.6847d15)  ! in 10^40 erg
        end if
        
        mm1 = mm ! Remember the previous value
     end do mesh !do mp=1,nmsh
     
     vk = vk/((m1-mhe)*r1**2)   
     write(6,'(I4,I7,I5,ES13.5,f10.4,2f6.3,ES9.2,1x,4ES9.2,ES9.2,1x,4f7.4,1x,4f6.3,2ES8.1)') &
          bl,nmdl,nmsh,age,m1,mhe,mco,m1-mhe,r1,l1,ts,tc,rhoc,hc,hec,cc,oc,hs,hes,zs,vk !,be,be1 !,bms,p,p1
     
     bl = bl+1
  end do block
  
  
  close(10)
  
  nblk = bl-1
  write(6,*)''
  write(*,*)' EOF reached,',nblk,' structure models read.'
  write(6,*)''
  
  if(nblk.eq.0) then
     write(6,'(A,/)')'  Program finished'
     stop
  end if
  
  
end subroutine list_mdl_models
!***********************************************************************************************************************************








!***********************************************************************************************************************************
!> \brief  Print the details of model blk in file infile to screen
!!
!! \param infile  Name of the input file
!! \param blk     Number of the stellar-structure block to display
!! \param svblk   Save block or not (in/out)

subroutine print_mdl_details(infile,blk,svblk)
  use mdl_data, only: nmsh,nv,mdlver
  
  implicit none
  character, intent(in) :: infile*(99)
  integer,intent(in) :: blk
  logical, intent(inout) :: svblk
  
  real :: age
  integer :: nmdl
  real :: mm,rr,pp,rrh,tt,kk,nnad,nnrad,hh,hhe,ccc,nnn,oo,nne,mmg
  real :: ll,eeth,eenc,eenu,ss,uuint
  real :: m1,r1,l1,ts,tc,mhe,mco,mhenv
  real :: hc,hec,cc,oc,nec,zc  !,mgc,nic
  real :: hs,hes,cs,ns,os,nes,mgs,zs
  real :: rhoc,pc  !,ethc,enuc,encc
  
  integer :: mp,in,io
  character :: outfile*(99)
  
  mp = 1  ! Silence compiler warnings
  
  
  ! Open the input file and read the first blk-1 models:
  call read_first_mdls(infile,blk-1)
  
  
  !************************************************************************      
  !***   READ CHOSEN STRUCTURE MODEL AND GET VARIABLES TO PRINT
  !************************************************************************      
  
  ! Read header:
  read(10,'(I6,1x,ES16.9)',iostat=io) nmdl,age
  if(io.ne.0) then
     write(0,'(A,I5,A,/)')'5  Error reading first line (header) of model',blk,', aborting...'
     close(10)
     stop
  end if
  
  ! Read mesh point 1:
  read(10,'(ES13.6,4ES11.4,16ES11.3)',iostat=io) &
       mm,rr,pp,rrh,tt,kk,nnad,nnrad,hh,hhe,ccc,nnn,oo,nne,mmg,ll,eeth,eenc,eenu,ss,uuint
  
  if(io.ne.0) then  ! Error/EOF
     close(10)
     if(io.lt.0) then
        write(6,'(A,/)')'  Program finished'  !EOF
     else
        write(0,'(A,2(I5,A),/)')'  Error reading model',blk,'line',mp-1,', aborting...'  ! Read error
     end if
     stop
  end if
  
  
  if(svblk) then
     ! Create output filename:
     in = index(trim(infile),'.mdl',.true.)
     write(outfile,'(A,I5.5,A)')infile(1:in-1)//'_',nmdl,trim(infile(in:))
     
     ! Open output file and write header and mesh point 1
     open(unit=20,form='formatted',status='replace',file=trim(outfile))
     write(20,'(2x,I4,4x,I2,F7.3)') nmsh,nv,mdlver
     write(20,'(I6,1x,ES16.9)') nmdl,age
     write(20,'(ES13.6,4ES11.4,16ES11.3)') &
          mm,rr,pp,rrh,tt,kk,nnad,nnrad,hh,hhe,ccc,nnn,oo,nne,mmg,ll,eeth,eenc,eenu,ss,uuint
  end if
  
  tc   = tt
  hc   = hh
  hec  = hhe
  cc   = ccc
  !nic   = nnn
  oc   = oo
  nec  = nne
  !mgc  = mmg
  zc   = 1. - hh - hhe
  rhoc = rrh
  pc   = pp
  !encc = eenc
  !ethc = eeth
  !enuc = eenu
  
  mhe = 0.
  mco = 0.
  
  do mp=2,nmsh ! Number of mesh points
     read(10,'(ES13.6,4ES11.4,16ES11.3)') &
          mm,rr,pp,rrh,tt,kk,nnad,nnrad,hh,hhe,ccc,nnn,oo,nne,mmg,ll,eeth,eenc,eenu,ss,uuint
     
     if(io.ne.0) then  ! EOF/read error
        close(10)
        write(0,'(A,2(I5,A),/)')'  Error reading model',blk,'line',mp-1,', aborting...'
        stop
     end if
     

     if(svblk) write(20,'(ES13.6,4ES11.4,16ES11.3)') &
          mm,rr,pp,rrh,tt,kk,nnad,nnrad,hh,hhe,ccc,nnn,oo,nne,mmg,ll,eeth,eenc,eenu,ss,uuint
     if(mhe.eq.0.0.and.hh.ge.0.1) mhe = mm
     if(mco.eq.0.0.and.hhe.ge.0.1) mco = mm
  end do
  
  close(10)
  
  m1  = mm
  r1  = rr
  l1  = ll
  ts  = tt
  hs  = hh
  hes = hhe
  cs  = ccc
  ns  = nnn
  os  = oo
  nes = nne
  mgs = mmg
  zs  = 1. - hh - hhe
  
  mhenv = m1 - mhe
  
  
  
  
  
  
  !************************************************************************      
  !***   PRINT MODEL DETAILS
  !************************************************************************      
  if(.not.svblk) then
     write(6,'(A)')' Properties of this model:'
     write(6,*)''
     write(6,81)nmdl,nmsh,m1,age,zs
     write(6,*)''
     write(6,83)m1,r1,l1,ts
     write(6,84)tc,pc,rhoc
     write(6,*)''
     write(6,85)mhe,mco,mhenv
     write(6,*)''
     !write(6,88)hs,hes,cs,ns,os,nes,mgs,zs
     !write(6,89)hc,hec,cc,ns,oc,nec,mgs,zc
     write(6,90)hs,hes,cs,ns,os,nes,mgs,zs
     write(6,91)hc,hec,cc,ns,oc,nec,mgs,zc
     write(6,*)''
  end if
  
81 format('  Model:        Model nr:',i5,',    Mesh pts: ',i4,',    Mass:',f7.2,' Mo,    Age: ',es12.6,' yr,    Z =',f7.4)
83 format('  Surface:      M   = ',f9.5,' Mo,  R   =',f11.5,' Ro,   L    =  ',es10.4,' Lo,   Teff =',f8.0,' K')
84 format('  Centre:       Tc  = ',es10.4,' K,  Pc =  ',es10.4,' dyn,  RHOc = ',es10.4,' g/cm3')
85 format('  Cores:        Mhe = ',f9.5,' Mo,  Mco =',f9.5,' Mo,     Menv =',f9.5,' Mo')
  
90 format('  Composition:  Surface:  H: ',es10.4,',  He: ',es10.4,',  C: ',es10.4,',  N: ',es10.4,',  O: ',es10.4, &
        ',  Ne: ',es10.4,',  Mg: ',es10.4,',  Z: ',es10.4)
91 format('  Composition:     Core:  H: ',es10.4,',  He: ',es10.4,',  C: ',es10.4,',  N: ',es10.4,',  O: ',es10.4, &
        ',  Ne: ',es10.4,',  Mg: ',es10.4,',  Z: ',es10.4)
  
  
  if(svblk) then
     close(20)
     write(6,'(A)')' Output model saved in '//trim(outfile)//'.'
     svblk = .false.  ! Stop saving the model block
  end if
  
  
  
end subroutine print_mdl_details
!***********************************************************************************************************************************



!***********************************************************************************************************************************
!> \brief  Open a .mdl[12] file and read the first blk models without storing the data
!!
!! \param infile   Name of the input file
!! \param blk      Number of the stellar-structure blocks to read (and ignore)

subroutine read_first_mdls(infile,blk)
  use SUFR_dummy, only: dumint, dumreal, dumstr
  use mdl_data, only: pxnr, nmsh,nv,mdlver, nc
  
  implicit none
  character, intent(in) :: infile*(*)
  integer, intent(in) :: blk
  
  integer :: io,bl,mp !,ii,nmdl
  !real :: age !,x
  !character :: tmpstr*(3)
  
  
  open(unit=10,form='formatted',status='old',file=trim(infile))
  rewind(10)
  read(10,'(2x,I4,4x,I2,F7.3)',iostat=io) nmsh,nv,mdlver
  if(io.ne.0) then
     write(0,'(A,/)')'3  Error reading first line (header) of the file, aborting...'
     close(10)
     stop
  end if
  
  if(mdlver.gt.1.) then
     read(10,'(60I4)')pxnr(1:nc)
  else
     nc = 21
  end if
  
  if(nmsh.eq.0) then ! Then post-2005 version output
     rewind(10)
  end if
  
  ! Read file, upto chosen model:
  if(blk.gt.0) then
     do bl=1,blk
        read(10,'(I6,1x,ES16.9)',iostat=io) dumint, dumreal  !nmdl,age
        if(io.ne.0) then
           write(0,'(A,I5,A,/)')'4  Error reading first line (header) of model',bl,', aborting...'
           close(10)
           stop
        end if
        
        if(nmsh.eq.0) nmsh = 199
        do mp=1,nmsh
           !read(10,'(ES13.6,4ES11.4,16ES11.3)',iostat=io) (x, ii=1,21) 
           read(10,*,iostat=io) dumstr
           
           if(io.ne.0) then  !Error/EOF
              close(10)
              if(io.lt.0) then
                 write(6,'(A,/)')'  Program finished'  !EOF
              else
                 write(0,'(A,2(I5,A),/)')'  Error reading model',bl-1,'line',mp-1,', aborting...'  ! Read error
              end if
              stop
           end if
           
        end do !mp
     end do !bl
  end if
  
end subroutine read_first_mdls
!***********************************************************************************************************************************


!***********************************************************************************************************************************
!> \brief  Read the chosen structure model from an open .mdl[12] file
!!
!! \param  blk  Number of the stellar-structure block to display
!!
!! \retval mdl  Model number of the selected model
!! \retval age  Age of the selected model
!! \retval dat  Data for the selected model

subroutine read_chosen_mdl(blk, mdl,age,dat)
  use SUFR_kinds, only: double
  use mdl_data, only: nq,nn, nmsh,nc,nm
  
  implicit none
  integer, intent(in) :: blk
  integer, intent(out) :: mdl
  real(double), intent(out) :: age, dat(nq,nn)
  
  integer :: i,io,mp
  real(double) :: dat1(nq)
  
  read(10,'(I6,1x,E16.9)',iostat=io) mdl,age
  if(io.ne.0) then
     write(0,'(A,I5,A,/)')'4  Error reading first line (header) of model',blk,', aborting...'
     close(10)
     stop
  end if
  
  if(nmsh.eq.0) nmsh = 199
  do mp=1,nm
     
     read(10,*,iostat=io) (dat1(i),i=1,nc)  ! gfortran reports a read error when the number is smaller or larger than the accuracy
     dat(1:nc,mp) = dat1(1:nc)
     
     if(io.ne.0) then  !Error/EOF
        close(10)
        if(io.lt.0) then
           write(6,'(A,/)')'  Program finished'  !EOF
        else
           write(0,'(A,2(I5,A),/)')'  Error reading model',blk-1,'line',mp-1,', aborting...'  ! Read error
           print*,real(dat1(1:nc))
        end if
        stop
     end if
     
  end do !mp
  
end subroutine read_chosen_mdl
!***********************************************************************************************************************************




!***********************************************************************************************************************************
!> \brief  Defines the variable labels for the mdl[12] format

subroutine set_mdl_labels
  use mdl_data, only: pxns,pxfns, labels,abds,nabs,CEs,nv_der,nv_sp
  implicit none
  
  abds = [character(len=99) :: 'H ','He','C ','N ','O ','Ne','Mg']    ! Line labels in abundances plot
  nabs = [character(len=99) :: 'ad ','rad','true']                    ! Line labels in nablas plot
  CEs  = [character(len=99) :: 'r=R\drl\u','\ga-CE','\gg-CE']         ! Line labels in CEs plot
  
  !Names of the variables in px
  pxns(0) = ''
  pxns(1:10)  = [character(len=99) :: 'Psi','P','Rho','T','k','Nad','Ntrue','Nrad-Nad','M','H']
  pxns(11:20) = [character(len=99) :: 'He','C','N','O','Ne','Mg','R','L','Eth','Enuc']
  pxns(21:30) = [character(len=99) :: 'Enu','dM','...','Thom','Uhom','Vhom','Uint','S','L/Ledd','wxl']
  pxns(31:40) = [character(len=99) :: 'mu','wt?','Nel','NeO','w?','MI','phi','Fm','DGOS','...']
  pxns(41:50) = [character(len=99) :: '...','LDRK','Enth','V^2','FAC','','','','','Rpp']
  pxns(51:60) = [character(len=99) :: 'Rpc','Rpng','Rpn','Rpo','Ran','','','','','N^2']
  
  pxns(201:210) = [character(len=99) :: 'Mesh pt','Rev mesh pt','m/M*','r/R*','C/O','Ne/O','Ugr-Uint','M.f.p.','n.dens','g']
  pxns(211:220) = [character(len=99) :: 'mu','n','Prad','Pgas','Pr/Pg','dM','Ub,*','Ub,env','P/rho','a_rlof']
  pxns(221:230) = [character(len=99) :: 'Prlof','Erlof','Jrlof','a_ace','Pace','Eace','Jace','a_gce','Pgce','Egce']
  pxns(231:232) = [character(len=99) :: 'Jgce','Nrad']
  pxns(301:305) = [character(len=99) :: 'Abundances','Nablas','CEPs','CEEs','CEJs']
  
  !Names of the variables in px, to be used in output file name (no /.?*)
  pxfns(0) = ''
  pxfns(1:10)  = [character(len=99) :: 'Psi','P','Rho','T','k','Nad','Ntrue','Nrad-Nad','M','H']
  pxfns(11:20) = [character(len=99) :: 'He','C','N','O','Ne','Mg','R','L','Eth','Enuc']
  pxfns(21:30) = [character(len=99) :: 'Enu','dM','-','Thom','Uhom','Vhom','Uint','S','LLedd','wxl']
  pxfns(31:40) = [character(len=99) :: 'mu','wt','Nel','NeO','w','MI','phi','Fm','DGOS','-']
  pxfns(41:50) = [character(len=99) :: '-','LDRK','Enth','V2','FAC','-','-','-','-','Rpp']
  pxfns(51:60) = [character(len=99) :: 'Rpc','Rpng','Rpn','Rpo','Ran','-','-','-','-','N2']
  
  pxfns(201:210) = [character(len=99) :: 'Mesh pt','Rev mesh pt','mM','rR','CO','NeO','Ugr-Uint','Mfp','Ndens','g']
  pxfns(211:220) = [character(len=99) :: 'mu','n','Prad','Pgas','PrPg','dM','Ubst','Ubenv','Prho','arlof']
  pxfns(221:230) = [character(len=99) :: 'Prlof','Erlof','Jrlof','a_ace','Pace','Eace','Jace','a_gce','Pgce','Egce']
  pxfns(231:232) = [character(len=99) :: 'Jgce','Nrad']
  pxfns(301:305) = [character(len=99) :: 'Abundances','Nablas','CEPs','EEPs','JEPs']
  
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
  labels(202) = '\u\(2263) surface\d    Mesh point    \ucentre \(2261)'
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
  
  labels(220) = 'a\dr(m)=Rrlof\u (R\d\(2281)\u)'                                           ! a_orb if r(m)=Rrl
  labels(221) = 'P\dr(m)=Rrlof\u (day)'                                                    ! Porb if r(m)=Rrl
  labels(222) = 'E\dr(m)=Rrlof\u (GM\d\(2281)\u\u2\d/R\d\(2281)\u)'                        ! Eorb if r(m)=Rrl
  labels(223) = 'J\dr(m)=Rrlof\u (G\u1/2\dM\d\(2281)\u\u3/2\dR\d\(2281)\u\u1/2\d)'         ! Jorb if r(m)=Rrl
  
  labels(224) = 'a\dpost-\ga-CE\u (R\d\(2281)\u)'                                          ! a_orb after \alpha CE
  labels(225) = 'P\dpost-\ga-CE\u (day)'                                                   ! Porb after \alpha CE
  labels(226) = 'E\dpost-\ga-CE\u (GM\d\(2281)\u\u2\d/R\d\(2281)\u)'                       ! Eorb after \alpha CE
  labels(227) = 'J\dpost-\ga-CE\u (G\u1/2\dM\d\(2281)\u\u3/2\dR\d\(2281)\u\u1/2\d)'        ! Jorb after \alpha CE
  
  labels(228) = 'a\dpost-\gg-CE\u (R\d\(2281)\u)'                                          ! a_orb after \gamma CE
  labels(229) = 'P\dpost-\gg-CE\u (day)'                                                   ! Porb after \gamma CE
  labels(230) = 'E\dpost-\gg-CE\u (GM\d\(2281)\u\u2\d/R\d\(2281)\u)'                       ! Eorb after \gamma CE
  labels(231) = 'J\dpost-\gg-CE\u (G\u1/2\dM\d\(2281)\u\u3/2\dR\d\(2281)\u\u1/2\d)'        ! Jorb after \gamma CE
  labels(232) = '\(2266)\drad\u'
  
  nv_der = 232 - 200  ! Number of derived variables
  
  
  
  ! Special plots:
  labels(301) = 'Abundances'
  labels(302) = "\(2266)'s"
  labels(303) = 'P\dorb,post-CE\u (day)'
  labels(304) = 'E\dorb,post-CE\u (GM\d\(2281)\u\u2\d/R\d\(2281)\u)'
  labels(305) = 'J\dorb,post-CE\u (G\u1/2\dM\d\(2281)\u\u3/2\dR\d\(2281)\u\u1/2\d)'
  
  nv_sp = 305 - 300  !Number of special plots
  
end subroutine set_mdl_labels
!***********************************************************************************************************************************
  
  


