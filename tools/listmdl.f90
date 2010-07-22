! Lists the data contained in a mdl* file
! Lines are longer than 72 chars, so add --wide (lf) or -132 (ifort) to compile
! AF
!
!   Copyright 2002-2009 AstroFloyd - astrofloyd.org
!   
!   
!   This file is part of the eggleton-tools package.
!   
!   This is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by
!   the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
!   
!   This software is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
!   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details.
!   
!   You should have received a copy of the GNU General Public License along with this code.  If not, see <http://www.gnu.org/licenses/>.

program listmdl
  use constants
  implicit none
  
  real :: age,dov,x,vk,mm1,be,be1
  integer :: nmsh,nv,nmdl
  real :: mm,rr,pp,rrh,tt,kk,nnad,nnrad,hh,hhe,ccc,nnn,oo,nne,mmg
  real :: ll,eeth,eenc,eenu,ss,uuint
  real :: m1,r1,l1,ts,tc,mhe,mco,mhenv
  real :: hc,hec,cc,nc,oc,nec,mgc,zc
  real :: hs,hes,cs,ns,os,nes,mgs,zs
  real :: rhoc,pc,ethc,enuc,encc
  
  integer ii,bl,mp,nblk,blk,ans,in
  character findfile*99,infile*99,outfile*99
logical :: svblk
  
  call setconstants()
  
  svblk = .false.
  
  if(iargc().eq.1) then
     call getarg(1,infile)
  else
     infile = findfile('*.mdl*') !Search for input file in current dir
     if(len_trim(infile).le.0) call quit_program('No file found in this directory.')
  end if
     
  
  
  
  !************************************************************************      
  !***   READ ALL STRUCTURE MODELS IN THE FILE AND DISPLAY MAIN PROPERTIES
  !************************************************************************      
  
  write(6,*)''
4 continue
  write(6,'(A)')'  Reading file '//trim(infile)
  open(unit=10,form='formatted',status='old',file=trim(infile))
  
  read(10,'(2x,I4,4x,I2,F7.3)',err=11,end=11) nmsh,nv,dov
  write(6,*)''
  write(6,'(A)')'  Nr  Model Nmsh          Age        M1   Mhe   Mco     Menv         R        L     Teff       Tc     Rhoc'// &
       '      Xc     Yc     Cc     Oc     Xs    Ys    Zs   k^2'
  
  mp = 1  !Silence compiler warnings
  do bl=1,999
     if(mod(bl,25).eq.0) then
        write(6,*)''
        write(6,'(A)')'  Nr  Model Nmsh          Age        M1   Mhe   Mco     Menv         R        L     Teff       Tc'// &
             '     Rhoc      Xc     Yc     Cc     Oc     Xs    Ys    Zs   k^2'
     end if
     read(10,'(I6,1x,ES16.9)',err=12,end=15) nmdl,age
     
     
     mhe = 0.
     mco = 0.
     vk = 0.
     mm1 = 0.
     be = 0.
     be1 = 0.
     
     do mp=1,nmsh
        read(10,'(ES13.6,4ES11.4,16ES11.3)',err=13,end=15) &
             mm,rr,pp,rrh,tt,kk,nnad,nnrad,hh,hhe,ccc,nnn,oo,nne,mmg,ll,eeth,eenc,eenu,ss,uuint
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
     end do !do mp=1,nmsh
     
     vk = vk/((m1-mhe)*r1**2)   
     write(6,'(I4,I7,I5,ES13.5,f10.4,2f6.3,ES9.2,1x,4ES9.2,ES9.2,1x,4f7.4,1x,4f6.3,2ES8.1)') &
          bl,nmdl,nmsh,age,m1,mhe,mco,m1-mhe,r1,l1,ts,tc,rhoc,hc,hec,cc,oc,hs,hes,zs,vk !,be,be1 !,bms,p,p1
     
  end do !bl
  
  
  
11 continue
  write(6,'(A)')'  Error reading first line of file, aborting...'
  close(10)
  write(6,'(A,/)')'  Program finished'
  stop
  
12 continue
  write(6,'(A)')'  Error reading first line of block, aborting...'
  close(10)
  write(6,'(A,/)')'  Program finished'
  stop
  
13 continue
  write(*,*)'  Error reading block',bl-1,'line',mp-1,', aborting...'
  close(10)
  write(6,'(A,/)')'  Program finished'
  stop
  
15 continue
  close(10)
  
  nblk = bl-1
  write(6,*)''
  write(*,*)' EOF reached,',nblk,' blocks read.'
  write(6,*)''
  
  if(nblk.eq.0) then
     write(6,'(A,/)')'  Program finished'
     stop
  end if
  
  
  
  
  
  !************************************************************************      
  !***   CHOOSE STRUCTURE MODEL
  !************************************************************************      
  
20 continue
  if(nblk.eq.1) then
     blk = 1 
  else  
     
     blk = 0
     do while(blk.lt.1.or.blk.gt.nblk)
        write(6,'(A50,I3,A3,$)')' For which model do you want to print details (1-',nblk,'): '
        read*,blk
        if(blk.eq.0) then
           write(6,'(A,/)')'  Program finished'
           stop
        end if
     end do
     
     
  end if
  
22 continue
  
  open(unit=10,form='formatted',status='old',file=trim(infile))
  read(10,'(2x,I4,4x,I2,F7.3)',err=11,end=11) nmsh,nv,dov
  
  !Read file, upto chosen model (blk-1)
  if(blk.ne.1) then
     do bl=1,blk-1
        read(10,'(I6,1x,ES16.9)',err=12,end=12) nmdl,age
        do mp=1,nmsh
           read(10,'(ES13.6,4ES11.4,16ES11.3)',err=13,end=999) (x, ii=1,21) 
        end do !mp
     end do !bl
  end if
  
  
  !************************************************************************      
  !***   READ CHOSEN STRUCTURE MODEL AND GET VARIABLES TO PRINT
  !************************************************************************      
  
  ! Read header and mesh point 1:
  read(10,'(I6,1x,ES16.9)',err=12,end=12) nmdl,age
  read(10,'(ES13.6,4ES11.4,16ES11.3)',err=13,end=999) &
       mm,rr,pp,rrh,tt,kk,nnad,nnrad,hh,hhe,ccc,nnn,oo,nne,mmg,ll,eeth,eenc,eenu,ss,uuint
  
  if(svblk) then
     ! Create output filename:
     in = index(trim(infile),'.mdl',.true.)
     write(outfile,'(A,I5.5,A)')infile(1:in-1)//'_',nmdl,trim(infile(in:))
     
     ! Open output file and write header and mesh point 1
     open(unit=20,form='formatted',status='replace',file=trim(outfile))
     write(20,'(2x,I4,4x,I2,F7.3)') nmsh,nv,dov
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
     read(10,'(ES13.6,4ES11.4,16ES11.3)',err=13) &
          mm,rr,pp,rrh,tt,kk,nnad,nnrad,hh,hhe,ccc,nnn,oo,nne,mmg,ll,eeth,eenc,eenu,ss,uuint
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
  
81 format('  Model:        Model nr:',i5,',    Mesh pts: ',i4,',    Mass:',f7.2,' Mo,    Age: ',es12.6,' yr,    Z ='f7.4)
83 format('  Surface:      M   = ',f9.5,' Mo,  R   =',f11.5,' Ro,   L    =  ',es10.4' Lo,   Teff =',f8.0,' K')
84 format('  Centre:       Tc  = ',es10.4,' K,  Pc =  ',es10.4,' dyn,  RHOc = ',es10.4,' g/cm3')
85 format('  Cores:        Mhe = ',f9.5,' Mo,  Mco =',f9.5,' Mo,     Menv =',f9.5,' Mo')
  
90 format('  Composition:  Surface:  H: ',es10.4,',  He: ',es10.4,',  C: ',es10.4,',  N: ',es10.4,',  O: ',es10.4, &
        ',  Ne: ',es10.4,',  Mg: ',es10.4,',  Z: ',es10.4)
91 format('  Composition:     Core:  H: ',es10.4,',  He: ',es10.4,',  C: ',es10.4,',  N: ',es10.4,',  O: ',es10.4, &
        ',  Ne: ',es10.4,',  Mg: ',es10.4,',  Z: ',es10.4)
  
  
  
  
  
  
  
  
  !************************************************************************      
  !***   FINISH
  !************************************************************************      
  
  if(svblk) then
     close(20)
     write(6,'(A)')' Output model saved in '//trim(outfile)//'.'
     svblk = .false.  ! Stop saving the model block
  end if
  
  ans = -1
  do while(ans.lt.0.or.ans.gt.3)
     if(nblk.eq.1) then
        write(6,'(A,/)')'  Program finished'
        stop
     end if
     
     write(6,*)''
     write(6,'(A)')' You can:'
     write(6,'(A)')'   0) Quit'
     write(6,'(A)')'   1) See another block'
     write(6,'(A)')'   2) List all blocks again'
     write(6,'(A)')'   3) Save this block'
     write(6,*)''
     write(6,'(A27,$)')' What do you want to do ?  '
     
     read*,ans
  end do
  
  select case(ans)
  case(1)
     goto 20
  case(2)
     goto 4
  case(3)
     svblk = .true.
     goto 22
  end select
  
  
  
999 continue
  close(10)
  write(6,'(A,/)')'  Program finished'
  
end program listmdl

