! Reads, optinally changes and (over!)writes an init.run (fort.23) input file
! AF January 21, 2004

program makerun  
  implicit none
  real*8 :: ct1(7),ct2(7),ct3(7)
  real*8 :: ml1,dml,ql1,dql,xl1,dxl
  real*8 :: rot,ex
  real*8 :: sm,dty,age,per,bms,ecc,p1,enc
  real*8 :: m2
  integer :: isb,ktw,ip1,im1,ip2,im2,kpt,kp
  integer :: kml,kql,kxl,kr,jmx
  integer :: io,narg,iargc
  character :: file*8,arg*10
  
  write(6,*)''
  file = 'init.run'
  
  
  open(unit=10,form='formatted',status='old',file=file,iostat=io)
  if(io.ne.0) goto 90
  rewind 10
  read(10,*,err=91) isb,ktw,ip1,im1,ip2,im2,kpt,kp
  read(10,*,err=92) ml1,dml,kml
  read(10,*,err=93) ql1,dql,kql
  read(10,*,err=94) xl1,dxl,kxl
  read(10,*,err=95) rot,kr,ex
  read(10,*,err=96) sm,dty,age,per,bms,ecc,p1,enc,jmx
  read(10,*,err=97) ct1
  read(10,*,err=98) ct2
  read(10,*,err=99) ct3
  close(10)
  
  kml = 1  !Only one iteration in mass
  m2 = 0.5d0*sm
  if(bms.gt.0) m2 = bms-sm
  
  narg = iargc()
  if(narg.eq.2) then
     call getarg(1,arg)
     read(arg,*)sm
     call getarg(2,arg)
     read(arg,*)per
  else if(narg.eq.3) then
     call getarg(1,arg)
     read(arg,*)sm
     call getarg(2,arg)
     read(arg,*)m2
     call getarg(3,arg)
     read(arg,*)per
  else
     write(6,'(A)')'  Syntax:  makerun <M1> <M2> <Porb>'
     write(6,'(A)')'      or:  makerun <M1> <Porb>'
     write(6,'(A)')''
     write(6,'(A33,$)')'  Give the mass of star 1 (Mo):  '
     read*,sm
     write(6,'(A33,$)')'  Give the mass of star 2 (Mo):  '
     read*,m2
     write(6,'(A33,$)')'  Give the orbital period (d):   '
     read*,per
  end if
  
  if(bms.gt.0.d0.or.narg.eq.3) bms = sm + m2
  !dty = 1.d5
  !p1 = per  !Syncronise the initial binary
  !print*,sm,m2,bms
  
  ml1 = log10(sm)
  if(narg.eq.3) ql1 = log10(sm/m2)
  xl1 = log10(per)
  
  open(unit=20,form='formatted',file=file)
  write(20,50) isb,ktw,ip1,im1,ip2,im2,kpt,kp,  &
    ml1,dml,kml,ql1,dql,kql,xl1,dxl,kxl,  &
    rot,kr,ex,  &
    sm,dty,age,per,bms,ecc,p1,enc,jmx,  &
    ct1,ct2,ct3
  write(20,*)''
  write(20,'(A)')'last five lines:'
  write(20,'(A)')'    ROT     KR   EX   : KR=1 -> P1 = Pcrit1*10**ROT;  KR=2 -> P1 = Porb*10**ROT'
  write(20,'(A)')'alternative initial conditions if JMX >= 0, and if any item is non-negative:'
  write(20,'(A)')'    SM       DTY          AGE       PER       BMS       ECC       P1        ENC       JMX'
  write(20,'(A)')'conditions for termination:'
  write(20,'(A)')'    rlf1     age      LCarb     rlf2      LHe        rho      MCO '
  write(20,'(A)')'    rho      mdot     XHe       eps       dtmin      sm8      vmh8'
  write(20,'(A)')'    last     line     not       yet       used       ...      ... '
  close(20)
  
50 format (8I6,/,  3(2ES11.3,I5,/),  ES11.3,I3,ES10.2,/,   ES11.3,ES12.4,6ES10.2,I6,/,      3(7ES10.2,/))
  
  
  
  write(6,'(A,/)')'Program done'
  stop
  
90 write(6,'(A,/)')'Error opening file: '//trim(file)
  stop
91 write(6,'(A,/)')'Error reading file: '//trim(file)//', line 1'
  stop
92 write(6,'(A,/)')'Error reading file: '//trim(file)//', line 2'
  stop
93 write(6,'(A,/)')'Error reading file: '//trim(file)//', line 3'
  stop
94 write(6,'(A,/)')'Error reading file: '//trim(file)//', line 4'
  stop
95 write(6,'(A,/)')'Error reading file: '//trim(file)//', line 5'
  stop
96 write(6,'(A,/)')'Error reading file: '//trim(file)//', line 6'
  stop
97 write(6,'(A,/)')'Error reading file: '//trim(file)//', line 7'
  stop
98 write(6,'(A,/)')'Error reading file: '//trim(file)//', line 8'
  stop
99 write(6,'(A,/)')'Error reading file: '//trim(file)//', line 9'
  stop
  
end program makerun
