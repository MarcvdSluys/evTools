!> \file mdl_functions.f90  Routines to help plot the data contained in mdl[12] files

! AF, 21-08-2010

! Copyright 2002-2010 AstroFloyd - astrofloyd.org
! 
! 
! This file is part of the eggleton-plot package.
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
  integer, parameter :: nn=2001, nq=400  !nq: max number of columns
  
  ! Variable labels:
  integer :: nv_der, nv_sp
  character :: pxns(0:nq)*99, pxfns(0:nq)*99, labels(nq)*99
  character :: abds(7)*99, nabs(3)*99, CEs(2)*99
end module mdl_data
!***********************************************************************************************************************************






!***********************************************************************************************************************************
!> \brief  Read all structure models in the file and display main properties
!! \param infile  Name of the .mdl[12] input file
!! \retval nblk   Number of stellar-structure blocks in the file
subroutine list_mdl_models(infile,nblk)
  use constants
  use mdl_data
  
  implicit none
  character, intent(in) :: infile*(*)
  integer, intent(out) :: nblk
  
  integer :: nmsh,nv,nmdl
  integer :: bl,mp,io
  real :: age,mdlver,vk,mm1,be,be1
  real :: mm,rr,pp,rrh,tt,kk,nnad,nnrad,hh,hhe,ccc,nnn,oo,nne,mmg
  real :: ll,eeth,eenc,eenu,ss,uuint
  real :: m1,r1,l1,ts,tc,mhe,mco,rhoc
  real :: hc,hec,cc,oc,zc, hs,hes,cs,os,zs
  character :: tmpstr*3
  
  
  
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
     read(10,*)tmpstr
  else
     nc = 21
     pxnr(1:nc)=(/9,17,2,3,4,5,6,8,10,11,12,13,14,15,16,18,19,20,21,28,27/)!,50,51,52,53,54,55,31,7,24,25,26,60
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
        write(0,'(A,I5,A,/)')'2  Error reading first line (header) of model',bl,', aborting...'
        close(10)
        stop
     end if
     
     
     mhe = 0.
     mco = 0.
     vk = 0.
     mm1 = 0.
     be = 0.
     be1 = 0.
     
     mesh: do mp=1,nmsh
        read(10,'(ES13.6,4ES11.4,16ES11.3)',iostat=io) &
             mm,rr,pp,rrh,tt,kk,nnad,nnrad,hh,hhe,ccc,nnn,oo,nne,mmg,ll,eeth,eenc,eenu,ss,uuint
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
           zc  = 1. - hh - hhe
           rhoc = rrh
        end if
        if(mp.eq.nmsh) then
           m1  = mm
           r1  = rr
           l1  = ll
           ts  = tt
           hs  = hh
           hes = hhe
           cs = ccc
           os = oo
           zs  = 1. - hh - hhe
        end if
        if(mhe.eq.0.0.and.hh.gt.0.1) mhe = mm
        if(mco.eq.0.0.and.hhe.gt.0.1) mco = mm
        
        !Calculate V.K. of the envelope
        if(mp.gt.2.and.hh.gt.0.1) then
           vk = vk + (mm-mm1)*rr**2
           be = be + g*(mm-mm1)*mm1/rr*5.6847e15 !In 10^40 erg
        end if
        if(mp.gt.2.and.hh.gt.0.001) then
           be1 = be1 + g*(mm-mm1)*mm1/rr*5.6847e15 !In 10^40 erg
        end if
        
        mm1 = mm !Remember the previous value
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
subroutine print_mdl_details(infile,blk,svblk)
  use constants
  implicit none
  character, intent(in) :: infile*99
  integer,intent(in) :: blk
  logical, intent(inout) :: svblk
  
  real :: age,mdlver,x
  integer :: nmsh,nv,nmdl
  real :: mm,rr,pp,rrh,tt,kk,nnad,nnrad,hh,hhe,ccc,nnn,oo,nne,mmg
  real :: ll,eeth,eenc,eenu,ss,uuint
  real :: m1,r1,l1,ts,tc,mhe,mco,mhenv
  real :: hc,hec,cc,nc,oc,nec,mgc,zc
  real :: hs,hes,cs,ns,os,nes,mgs,zs
  real :: rhoc,pc,ethc,enuc,encc
  
  integer :: ii,bl,mp,in,io
  character :: outfile*99
  
  mp = 1  !Silence compiler warnings
  
  open(unit=10,form='formatted',status='old',file=trim(infile))
  read(10,'(2x,I4,4x,I2,F7.3)',iostat=io) nmsh,nv,mdlver
  if(io.ne.0) then
     write(0,'(A,/)')'3  Error reading first line (header) of the file, aborting...'
     close(10)
     stop
  end if
  
  !Read file, upto chosen model (blk-1)
  if(blk.ne.1) then
     do bl=1,blk-1
        read(10,'(I6,1x,ES16.9)',iostat=io) nmdl,age
        if(io.ne.0) then
           write(0,'(A,I5,A,/)')'4  Error reading first line (header) of model',bl,', aborting...'
           close(10)
           stop
        end if

        do mp=1,nmsh
           read(10,'(ES13.6,4ES11.4,16ES11.3)',iostat=io) (x, ii=1,21) 
           
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
  
  if(io.ne.0) then  !Error/EOF
     close(10)
     if(io.lt.0) then
        write(6,'(A,/)')'  Program finished'  !EOF
     else
        write(0,'(A,2(I5,A),/)')'  Error reading model',bl-1,'line',mp-1,', aborting...'  ! Read error
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
  nc   = nnn
  oc   = oo
  nec  = nne
  mgc  = mmg
  zc   = 1. - hh - hhe
  rhoc = rrh
  pc   = pp
  encc = eenc
  ethc = eeth
  enuc = eenu
  
  mhe = 0.
  mco = 0.
  
  do mp=2,nmsh ! Number of mesh points
     read(10,'(ES13.6,4ES11.4,16ES11.3)') &
          mm,rr,pp,rrh,tt,kk,nnad,nnrad,hh,hhe,ccc,nnn,oo,nne,mmg,ll,eeth,eenc,eenu,ss,uuint
     
     if(io.ne.0) then  ! EOF/read error
        close(10)
        write(0,'(A,2(I5,A),/)')'  Error reading model',bl-1,'line',mp-1,', aborting...'
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
83 format('  Surface:      M   = ',f9.5,' Mo,  R   =',f11.5,' Ro,   L    =  ',es10.4' Lo,   Teff =',f8.0,' K')
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
  
  


