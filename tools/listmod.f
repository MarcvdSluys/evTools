! Listmod.f
! Reads an input or output structure model file for Eggeltons TWIN code and lists the properties of each model it contains.
! One can then select a model to display its contents more precisely and optionally copy the model to a different file to serve as input. 
! AF 2003-12-17
! Write an extra 0 at the end of the first line to make the output with the V.2005 code.

program listmod
  use constants
  implicit none
  real*8 :: m1,dt,t,p,bms,ecc,p1,enc,horb
  real*8 :: lnf,lnt,x16,lnm,x1,dqdk,lnr,l,x4,x12,x20
  real*8 :: mi,pr,phi,phis,e,f
  real*8 :: m2,q1,q2,a,a1,a2,rl1,rl2,x
  real*8 :: r1,l1,ts,hs,hes,zs,cs,os,nes,tc,hc,hec,cc,oc,nec,zc
  real*8 :: mhe,mco,mhenv
  real*8 :: dat1(8),dat2(24),dat(99)
  integer :: i,j,kh,kp,jmod,jb,jin,n
  integer :: narg,iargc,blk,ans,iout
  character :: fname*99,findfile*99,outname*7
  
  call setconstants()
  
  
  !***   READ COMMAND LINE VARIABLES
  narg = iargc()
  if(narg.gt.0) then
     call getarg(1,fname)
  else
     write(6,'(A)')'  listmod: lists the contents of a mod-file to screen'
     write(6,'(A)')'           syntax:  listmod <filename>'
     write(6,*)''
     write(6,'(A)')"  I'll look in the current directory for a *.mod file..."
     fname=findfile('*.mod',5)
  end if
  
  
  
  
  !***   READ ALL STRUCTURE MODELS IN THE FILE AND DISPLAY MAIN PROPERTIES
  
  write(6,*)''
  write(6,'(A)')'  Reading file '//trim(fname)
  write(6,*)''
  open (unit=10,form='formatted',status='old',file=trim(fname))
3 rewind 10
  
  write(6,'(A)')'  Nr  Model Nmsh          Age       dT        M1    Mhe    Mco     Menv         R        L     Teff      Xs     Ys     Zs         Tc      Xc     Yc     Zc      Mtot     Porb     Prot'
  do i=1,999
     read(10,*,err=5,end=10)m1,dt,t,p,bms,ecc,p1,enc,kh,kp,jmod,jb,jin
     !print*,kh,kp,jmod,jb,jin
     mhe = 0.d0
     mco = 0.d0
     do j=1,kh
        !read(10,*,err=6,end=10)lnf,lnt,x16,lnm,x1,dqdk,lnr,l,x4,x12,x20,mi,pr,phi,phis,x,horb,e,f,x,x,x,x,x
        read(10,*,err=6,end=10)dat(1:jin)
        lnf = dat(1)
        lnt = dat(2)
        x16 = dat(3)
        lnm = dat(4)
        x1 = dat(5)
        dqdk = dat(6)
        lnr = dat(7)
        l = dat(8)
        x4 = dat(9)
        x12 = dat(10)
        x20 = dat(11)
        mi = dat(12)
        pr = dat(13)
        phi = dat(14)
        phis = dat(15)
        !x = dat(16)
        horb = dat(17)
        e = dat(18)
        f = dat(19)
        
        if(j.eq.1) then
           r1  = exp(lnr)*1.e11/r0
           l1  = l*1.d33/l0
           !l1  = l/3.844d0  !Peter's CLSN
           ts  = exp(lnt)
           m1 = lnm*1.d33/m0
           hs  = x1
           hes = x4
           zs  = 1.d0 - x1 - x4
        end if
        if(j.eq.kh) then
           tc  = exp(lnt)
           hc  = x1
           hec = x4
           zc  = 1.d0 - x1 - x4
        end if
        if(mhe.eq.0.0.and.x1.lt.0.1) mhe = lnm*1.d33/m0
        if(mco.eq.0.0.and.x4.lt.0.1) mco = lnm*1.d33/m0
     end do !j
     if(mod(i,50).eq.0) write(6,'(/,A)')'  Nr  Model Nmsh          Age       dT        M1    Mhe    Mco     Menv         R        L     Teff      Xs     Ys     Zs         Tc      Xc     Yc     Zc      Mtot     Porb     Prot'
     write(6,9)i,jmod,kh,t,dt,m1,mhe,mco,m1-mhe,r1,l1,ts,hs,hes,zs,tc,hc,hec,zc,bms,p,p1
  end do !i
  write(6,'(A)')'  EOF not reached, array too small!'
  n=999
  goto 12
5 write(6,'(A35,I3)')'  Error reading first line of block',i
  goto 10
6 write(6,'(A36,I3)')'  Error reading second line of block',i
9 format (I4,I7,I5, ES13.5,ES9.2, F10.4,2F7.3,ES9.2, 1x,3ES9.2,1x,3F7.4, 2x,ES9.2,1x,3f7.4,1x,3ES9.2)
10 n=i-1
  write(6,'(A)')'  Nr  Model Nmsh          Age       dT        M1    Mhe    Mco     Menv         R        L     Teff      Xs     Ys     Zs         Tc      Xc     Yc     Zc      Mtot     Porb     Prot'
  
  
  write(6,'(I5,A)')n,' blocks read.'
12 if(n.eq.0) goto 999
  write(6,*)''
  
  
  
  
  !***   CHOOSE STRUCTURE MODEL
  
  blk = 1
20 write(6,'(A,I3,A4,$)')'  For which model do you want to print details (1 -',n,'):  '
  read*,blk
  if(blk.eq.0) goto 9999
  if(blk.lt.1.or.blk.gt.n) goto 20
  
  
  !Read file, upto chosen model (blk-1)
  rewind 10
  do i=1,blk-1
     read(10,*,err=991)m1,dt,t,p,bms,ecc,p1,enc,kh,kp,jmod,jb,jin
     do j=1,kh
        read(10,*,err=993)lnf,lnt,x16,lnm,x1,dqdk,lnr,l,x4,x12,x20,mi,pr,phi,phis,x,horb,e,f,x,x,x,x,x
     end do !j
  end do !i
  
  
  
  
  !***   READ CHOSEN STRUCTURE MODEL AND GET VARIABLES TO PRINT
  
  read(10,*,err=991)m1,dt,t,p,bms,ecc,p1,enc,kh,kp,jmod,jb,jin   !jin = # columns
  read(10,*,err=993)lnf,lnt,x16,lnm,x1,dqdk,lnr,l,x4,x12,x20,mi,pr,phi,phis,x,horb,e,f,x,x,x,x,x
  
  m1  = lnm*1.d33/m0
  r1  = exp(lnr)*1.e11/r0
  l1  = l*1.d33/l0
  !l1  = l/3.844d0  !Peter's CLSN
  ts  = exp(lnt)
  hs  = x1
  hes = x4
  cs  = x12
  os  = x16
  nes = x20
  zs  = 1.d0 - hs - hes
  
  mhe = 0.d0
  mco = 0.d0
  do i=1,kh-1 !Number of Mesh points
     read(10,*,err=993)lnf,lnt,x16,lnm,x1,dqdk,lnr,l,x4,x12,x20,mi,pr,phi,phis,x,horb,e,f,x,x,x,x,x
     if(mhe.eq.0.0.and.x1.lt.0.1) mhe = lnm*1.d33/m0
     if(mco.eq.0.0.and.x4.lt.0.1) mco = lnm*1.d33/m0
  end do
  mhenv = m1 - mhe
  
  tc  = exp(lnt)
  hc  = x1
  hec = x4
  cc  = x12
  oc  = x16
  nec = x20
  zc  = 1.d0 - hc - hec
  
  m2  = bms - m1
  q1  = m1/m2
  q2  = m2/m1
  
  a   = (p*day/((g*bms*m0)/(4.d0*pi**2))**(-.5d0))**(2.d0/3.d0)/r0
  
  a1  = a *m2/bms
  a2  = a *m1/bms
  
  rl1 = a*(0.49*q1**(2.d0/3.)/(0.6d0*q1**(2.d0/3.)+ dlog(1.d0+q1**(1.d0/3.))))
  rl2 = a*(0.49*q2**(2.d0/3.)/(0.6d0*q2**(2.d0/3.)+ dlog(1.d0+q2**(1.d0/3.))))
  
  
  
  

  
  
  
  
  !***   PRINT MODEL DETAILS
  
  write(6,'(A)')'  Properties of this model:'
  write(6,*)''
  write(6,81)jmod,m1,t,dt,zs
  write(6,82)kh,kp,jin,jb
  write(6,*)''
  write(6,83)m1,r1,l1,tc,ts
  write(6,84)mhe,mco,mhenv
  write(6,*)''
  write(6,85)m1,m2,bms,q1,q2
  write(6,*)''
  write(6,86)p,a,a1,a2,rl1,rl2
  write(6,87)horb*1.d50,e,pr
  write(6,*)''
  write(6,88)hs,hes,cs,os,nes,zs
  write(6,89)hc,hec,cc,oc,nec,zc
  
81 format ('  Model:        Model nr:',i5,',    Mass:',f7.2,' Mo,    Age: ',es10.4,' yr,  Time step:   ',es10.4,' yr,    Z ='f7.4)
82 format ('                Mesh pts: ',i4,',      Kp: ',i6,',       Jin: ',i3',            Binary component:',i2)
  !83 format ('  Primary:      M  =',f9.5,' Mo,  R  =',f9.5,' Ro,   L  =  ',es10.4' Lo,  Tc =  ',es10.4,' K,   Teff =',f8.0,' K,   Mhe =',f9.5,',   Mco =',f9.5)
83 format ('  Primary:      M   =',f9.5,' Mo,  R   =',f9.5,' Ro,   L   =  ',es10.4' Lo,  Tc =  ',es10.4,' K,   Teff =',f8.0,' K')
84 format ('                Mhe =',f9.5,' Mo,  Mco =',f9.5,' Mo,   Menv=',f9.5,' Mo')
85 format ('  Binary:       M1  =',f9.5,' Mo,  M2  =',f9.5,' Mo,   Mb  =',f9.4' Mo,     q1 =',f9.5,',        q2   =',f9.5)
  !86 format ('  Orbit:        P  =',f9.5,' d,   a  =',f9.5,' Ro,   a1 =',f9.5,' Ro,    a2 =',f9.5,' Ro,   Rrl1 =',f9.5' Ro,   Rrl2 = ',f9.5,' Ro')
86 format ('  Orbit:        P  =',ES12.5,' d,   a  =',ES12.5,' Ro,   a1 =',ES12.5,' Ro,    a2 =',ES12.5,' Ro,   Rrl1 =',ES12.5' Ro,   Rrl2 = ',ES12.5,' Ro')
!87 format ('                J  =  ',es10.4,' erg s,                e  =',f9.5,',     Prot =',f9.5,' days')
87 format ('                J  =  ',es10.4,' erg s,                e  =',f9.5,',     Prot =',ES12.5,' days')
88 format ('  Composition:  Surface:  H: ',f6.4,',   He: ',f6.4,',   C: ',f6.4,',   O: ',f6.4,',   Ne: ',f6.4,',    Z: ',f6.4)
89 format ('  Composition:     Core:  H: ',f6.4,',   He: ',f6.4,',   C: ',f6.4,',   O: ',f6.4,',   Ne: ',f6.4,',    Z: ',f6.4)
  
  
  
  
  
  
  
  
  
  !***   FINISH
  
120 if(n.eq.1) goto 9999
  write(6,*)''
  write(6,'(A)')'  You can:'
  write(6,'(A)')'    0) Quit'
  write(6,'(A)')'    1) See another block'
  write(6,'(A)')'    2) Write this model to another file'
  write(6,*)''
  write(6,'(A28,$)')'  What do you want to do ?  '
  read*,ans
  
  if(ans.gt.2.or.ans.lt.0) goto 120
  if(ans.eq.1) goto 3
  if(ans.eq.0) goto 999
  

  
  
  
  !***   COPY MODEL TO DIFFERENT FILE
  
  rewind 10
  do i=1,blk-1
     read(10,*,err=991)m1,dt,t,p,bms,ecc,p1,enc,kh,kp,jmod,jb,jin
     do j=1,kh
        read(10,*,err=993)lnf,lnt,x16,lnm,x1,dqdk,lnr,l,x4,x12,x20,mi,pr,phi,phis,x,horb,e,f,x,x,x,x,x
     end do !j
  end do !i
  
  outname = 'fort.  '   !Copy to fort.99. If it exists, fort.98, etc
  iout = 99
205 write(outname(6:7),'(I2)')iout
  
  open (unit=20,form='formatted',status='new',file=outname,iostat=i)
  if(i.gt.0) then
     write(6,'(A)')'  File exists: '//trim(outname)
     iout = iout-1
     if(iout.gt.50) goto 205
     write(6,'(A,$)')'  Please enter the name of the output file: '
     read*,outname
     goto 205
  end if
  
  
  read(10,*,err=991)dat1,kh,kp,jmod,jb,jin
  write(20,211)dat1,kh,kp,jmod,jb,jin,0
  do j=1,kh
     read(10,*,err=993)dat2
     write(20,212)dat2
  end do !j
211 format(1X, 8ES23.15, 6I6)
212 format(1X, 24ES23.15)
  close(20)
  write(6,'(A)')'  Model written to '//trim(outname)//'.'
  
  
  
  
  
  goto 999
991 write(6,'(A)')'  Error reading first line.'
  goto 999
993 write(6,'(A)')'  Error reading second line.'
  
999 close(10)
9999 write(6,*)''
end program listmod




