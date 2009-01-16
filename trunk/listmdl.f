! Lists the data contained in a mdl* file
! Lines are longer than 72 chars, so add --wide (lf) or -132 (ifort) to compile
! AF

program listmdl
  implicit none
  real :: age,dov,x,vk,mm1,be,be1
  real :: pi,sigma,l0,m0,r0,g,day
  integer :: nmsh,nv,nmdl
  real :: mm,rr,pp,rrh,tt,kk,nnad,nnrad,hh,hhe,ccc,nnn,oo,nne,mmg
  real :: ll,eeth,eenc,eenu,ss,uuint
  real :: m1,r1,l1,ts,tc,mhe,mco,mhenv
  real :: hc,hec,cc,nc,oc,nec,mgc,zc
  real :: hs,hes,cs,ns,os,nes,mgs,zs
  real :: rhoc,pc,ethc,enuc,encc
  
  integer i,ii,j,nblk,blk,ans,svblk
  character findfile*99,fname*99
  
  svblk = 0
  
  pi	=	4.d0*datan(1.d0)
  sigma	=	5.67051d-5
  l0	=	3.83d33
  r0	=	6.9599d10
  m0	=	1.9891d33
  g		=	6.67259d-8
  day 	=	8.64d4
  
  
  !Search for input file in current dir
  fname = findfile('*.mdl*',6)
  
  
  
  
  !************************************************************************      
  !***   READ ALL STRUCTURE MODELS IN THE FILE AND DISPLAY MAIN PROPERTIES
  !************************************************************************      
  
  write(6,*)''
4 write(6,'(A)')'  Reading file '//trim(fname)
  open (unit=10,form='formatted',status='old',file=trim(fname))
  rewind 10
  
  read(10,5,err=11,end=11) nmsh,nv,dov
5 format (2x,I4,4x,I2,F7.3)
  write(6,*)''
  write(6,'(A)')'  Nr  Model Nmsh          Age        M1   Mhe   Mco     Menv         R        L     Teff       Tc     Rhoc      Xc     Yc     Cc     Oc     Xs    Ys    Zs   k^2'
  do ii=1,999
     if(mod(ii,25).eq.0) then
        write(6,*)''
        write(6,'(A)')'  Nr  Model Nmsh          Age        M1   Mhe   Mco     Menv         R        L     Teff       Tc     Rhoc      Xc     Yc     Cc     Oc     Xs    Ys    Zs   k^2'
     end if
     read(10,6,err=12,end=15) nmdl,age
6    format (1P,I6,1x,E16.9)
     mhe = 0.
     mco = 0.
     vk = 0.
     mm1 = 0.
     be = 0.
     be1 = 0.
     do j=1,nmsh
        read(10,7,err=13,end=15)mm,rr,pp,rrh,tt,kk,nnad,nnrad,hh,hhe,ccc,nnn,oo,nne,mmg,ll,eeth,eenc,eenu,ss,uuint
        if(j.eq.1) then
           tc  = tt
           hc  = hh
           hec = hhe
           cc = ccc
           oc = oo
           zc  = 1. - hh - hhe
           rhoc = rrh
        end if
        if(j.eq.nmsh) then
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
        if(j.gt.2.and.hh.gt.0.1) then
           vk = vk + (mm-mm1)*rr**2
           be = be + g*(mm-mm1)*mm1/rr*5.6847e15 !In 10^40 erg
        end if
        if(j.gt.2.and.hh.gt.0.001) then
           be1 = be1 + g*(mm-mm1)*mm1/rr*5.6847e15 !In 10^40 erg
        end if
        
        mm1 = mm !Remember the previous value
     end do !do j=1,nmsh
     
     vk = vk/((m1-mhe)*r1**2)	
     write(6,9)ii,nmdl,nmsh,age,m1,mhe,mco,m1-mhe,r1,l1,ts,tc,rhoc,hc,hec,cc,oc,hs,hes,zs,vk!,be,be1!,bms,p,p1
     write(20,'(4E11.4)')mhe,be,be1,be/be1
     
7    format (1P,E13.6,4E11.4,16E11.3)
     
  end do !ii
  
  
  
9 format (I4,I7,I5,ES13.5,f10.4,2f6.3,ES9.2,1x,4ES9.2,ES9.2,1x,4f7.4,1x,4f6.3,2ES8.1)
  
11 write(6,'(A)')'  Error reading first line of file, aborting...'
  close(10)
  goto 9999
12 write(6,'(A)')'  Error reading first line of block, aborting...'
  close(10)
  goto 9999
13 write(*,*)'  Error reading block',i-1,'line',j-1,', aborting...'
  close(10)
  goto 9999
15 close(10)
  
  nblk = ii-1
  write(6,*)''
  write(*,*)' EOF reached,',nblk,' blocks read.'
  write(6,*)''
  
  if(nblk.eq.0) goto 9999
  if(nblk.eq.1) then
     blk = 1 
     goto 25
  end if
  
  
  
  
  
  
  !************************************************************************      
  !***   CHOOSE STRUCTURE MODEL
  !************************************************************************      
  
20 write(6,'(A50,I3,A3,$)')' For which model do you want to print details (1-',nblk,'): '
  read*,blk
22 if(blk.eq.0) goto 9999
  if(blk.lt.1.or.blk.gt.nblk) goto 20
  
  !Read file, upto chosen model (blk-1)
25 open (unit=10,form='formatted',status='old',file=fname)
  rewind 10
  read(10,5,err=11,end=11) nmsh,nv,dov
  if(blk.eq.1) goto 27
  do i=1,blk-1
     read(10,6,err=12,end=12) nmdl,age
     do j=1,nmsh
        read(10,7,err=13,end=999) (x, ii=1,21) 
     end do !j
  end do !i
  
  
  !************************************************************************      
  !***   READ CHOSEN STRUCTURE MODEL AND GET VARIABLES TO PRINT
  !************************************************************************      
  
27 read(10,6,err=12,end=12) nmdl,age
  read(10,7,err=13,end=999) mm,rr,pp,rrh,tt,kk,nnad,nnrad,hh,hhe,ccc,nnn,oo,nne,mmg,ll,eeth,eenc,eenu,ss,uuint
  
  if(svblk.eq.1) then
     write(20,5) nmsh,nv,dov
     write(20,6) nmdl,age
     write(20,7) mm,rr,pp,rrh,tt,kk,nnad,nnrad,hh,hhe,ccc,nnn,oo,nne,mmg,ll,eeth,eenc,eenu,ss,uuint
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
  do i=2,nmsh !Number of Mesh points
     read(10,7,err=13)mm,rr,pp,rrh,tt,kk,nnad,nnrad,hh,hhe,ccc,nnn,oo,nne,mmg,ll,eeth,eenc,eenu,ss,uuint
     if(svblk.eq.1) write(20,7) mm,rr,pp,rrh,tt,kk,nnad,nnrad,hh,hhe,ccc,nnn,oo,nne,mmg,ll,eeth,eenc,eenu,ss,uuint
     if(mhe.eq.0.0.and.hh.ge.0.1) mhe = mm
     if(mco.eq.0.0.and.hhe.ge.0.1) mco = mm
  end do
  
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
  if(svblk.eq.0) then
     write(6,'(A)')' Properties of this model:'
     write(6,*)''
     write(6,81)nmdl,nmsh,m1,age,zs
     write(6,*)''
     write(6,83)m1,r1,l1,ts
     write(6,84)tc,pc,rhoc
     write(6,*)''
     write(6,85)mhe,mco,mhenv
     write(6,*)''
     !        write(6,88)hs,hes,cs,ns,os,nes,mgs,zs
     !        write(6,89)hc,hec,cc,ns,oc,nec,mgs,zc
     write(6,90)hs,hes,cs,ns,os,nes,mgs,zs
     write(6,91)hc,hec,cc,ns,oc,nec,mgs,zc
     write(6,*)''
  end if
  
81 format('  Model:        Model nr:',i5,',    Mesh pts: ',i4,',    Mass:',f7.2,' Mo,    Age: ',es12.6,' yr,    Z ='f7.4)
83 format('  Surface:      M   = ',f9.5,' Mo,  R   =',f11.5,' Ro,   L    =  ',es10.4' Lo,   Teff =',f8.0,' K')
84 format('  Centre:       Tc  = ',es10.4,' K,  Pc =  ',es10.4,' dyn,  RHOc = ',es10.4,' g/cm3')
85 format('  Cores:        Mhe = ',f9.5,' Mo,  Mco =',f9.5,' Mo,     Menv =',f9.5,' Mo')
  
88 format('  Composition:  Surface:  H: ',f6.4,',   He: ',f6.4,',   C: ',f6.4,',   N: ',f6.4,',   O: ',f6.4,',   Ne: ',f6.4,',  Mg: ',f6.4,',    Z: ',f6.4)
89 format('  Composition:     Core:  H: ',f6.4,',   He: ',f6.4,',   C: ',f6.4,',   N: ',f6.4,',   O: ',f6.4,',   Ne: ',f6.4,',  Mg: ',f6.4,',    Z: ',f6.4)
  
90 format('  Composition:  Surface:  H: ',es10.4,',  He: ',es10.4,',  C: ',es10.4,',  N: ',es10.4,',  O: ',es10.4,',  Ne: ',es10.4,',  Mg: ',es10.4,',  Z: ',es10.4)
91 format('  Composition:     Core:  H: ',es10.4,',  He: ',es10.4,',  C: ',es10.4,',  N: ',es10.4,',  O: ',es10.4,',  Ne: ',es10.4,',  Mg: ',es10.4,',  Z: ',es10.4)
  
  
  
  
  
  
  
  
  !************************************************************************      
  !***   FINISH
  !************************************************************************      
  
  if(svblk.eq.1) close(20)
  svblk = 0 !Don't save the block (anymore)
120 if(nblk.eq.1) goto 9999
  write(6,*)''
  write(6,'(A)')' You can:'
  write(6,'(A)')'   0) Quit'
  write(6,'(A)')'   1) Save this block'
  write(6,'(A)')'   2) See another block'
  write(6,'(A)')'   3) List all blocks again'
  write(6,*)''
  write(6,'(A27,$)')' What do you want to do ?  '
  read*,ans
  
  if(ans.gt.3.or.ans.lt.0) goto 120
  if(ans.eq.1) then 
     open(unit=20,form='formatted',status='replace',file='pltmdl.mdl')
     svblk = 1
  end if
  if(ans.eq.0) goto 999
  if(ans.eq.1) goto 22
  if(ans.eq.2) goto 20
  if(ans.eq.3) goto 4
  
  
999 close(10)
9999 write(6,'(A)')'  Program finished'
  write(6,*)''
end program listmdl

