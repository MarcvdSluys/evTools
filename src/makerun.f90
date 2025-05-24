!> \file makerun.f90  Reads, optinally changes and (over!)writes an init.run (fort.23) input file
!!
!! 2004-01-21, MvdS: initial version.


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
!> \brief  Reads, optinally changes and (over!)writes an init.run (fort.23) input file

program makerun
  use SUFR_kinds, only: double
  use init_run
  
  implicit none
  real(double) :: ct1(7),ct2(7),ct3(7), m2
  integer :: io,narg,command_argument_count
  character :: infile*(99),outfile*(99),arg*(10),bla*(500)
  
  write(*,*) ''
  infile = 'init.run'
  outfile = 'init.run.new'
  
  ! Read input:
  call read_init_run(trim(infile))
  
  ! Old format:
  if(.false.) then
     if(io.ne.0) goto 90
     rewind 10
     read(10,*,err=91) isb,ktw,ip1,im1,ip2,im2,kpt,kp
     read(10,*,err=92) ml1,dml,kml
     read(10,*,err=93) ql1,dql,kql
     read(10,*,err=94) xl1,dxl,kxl
     read(10,*,err=95) rot,kr,ex
     read(10,*,err=96) sm,dty,age,per,bms,ecc,p,enc,jmx
     read(10,*,err=97) ct1
     read(10,*,err=98) ct2
     read(10,*,err=99) ct3
     close(10)
  end if
  
  
  kml = 1  ! Only one iteration in mass
  m2 = 0.5d0*sm
  if(bms.gt.0) m2 = bms-sm
  
  narg = command_argument_count()
  if(narg.eq.1) then  ! Mass 1 only
     call get_command_argument(1,arg)
     read(arg,*) sm
     if(bms.gt.0.d0) m2 = bms - sm
     
  else if(narg.eq.2) then  ! Mass 1 + Porb (silly w/o M2? - not if you want to change Porb only)
     call get_command_argument(1,arg)
     read(arg,*) sm
     if(bms.gt.0.d0) m2 = bms - sm
     
     call get_command_argument(2,arg)
     read(arg,*) per
     
     write(*,'(A)')'  Orbital period provided, but no rotational period - synchronising binary...'
     p = per
     
  else if(narg.eq.3) then  ! Mass 1+2 + Porb
     call get_command_argument(1,arg)
     read(arg,*) sm
     call get_command_argument(2,arg)
     read(arg,*) m2
     call get_command_argument(3,arg)
     read(arg,*) per
     
     write(*,'(A)')'  Orbital period provided, but no rotational period - synchronising binary...'
     p = per
     
  else if(narg.eq.4) then  ! Mass 1+2 + Porb + Prot
     call get_command_argument(1,arg)
     read(arg,*) sm
     call get_command_argument(2,arg)
     read(arg,*) m2
     call get_command_argument(3,arg)
     read(arg,*) per
     call get_command_argument(4,arg)
     read(arg,*) p
     
  else
     write(*,'(/,A)')'  No arguments given - not changing anything.'
     write(*,'(/,A)')'  Syntax: '
     write(*,'(A)')'    makerun <M1>'
     write(*,'(A)')'    makerun <M1> <Porb>'
     write(*,'(A)')'    makerun <M1> <M2> <Porb> (synchonise: Prot=Porb)'
     write(*,'(A,/)')'    makerun <M1> <M2> <Porb> <Prot>'
     
     write(*,'(A)')'  Contents of the current init.run:'
     call print_main_settings(sm,m2,per,p)
     
     stop
  end if
  
  if(bms.gt.0.d0 .or. narg.ge.3) bms = sm + m2
  ! dty = 1.d5
  ! p = per  ! Syncronise the initial binary
  ! print*,sm,m2,bms
  
  ml1 = log10(sm)
  ! if(narg.eq.3)
  ql1 = log10(sm/m2)
  if(per.gt.0.d0) xl1 = log10(per)
  
  ! Write output:
  call write_init_run(trim(outfile))
  
  
  if(.false.) then  ! Old format
     open(unit=20,form='formatted',file=trim(outfile))
     write(20,50) isb,ktw,ip1,im1,ip2,im2,kpt,kp,  &
          ml1,dml,kml,ql1,dql,kql,xl1,dxl,kxl,  &
          rot,kr,ex,  &
          sm,dty,age,per,bms,ecc,p,enc,jmx,  &
          ct1,ct2,ct3
     read(10,*) bla
     write(20,'(/,A)')'last five lines:'
     
     io = 0
     do while(io.eq.0)
        read(10,'(A500)',iostat=io) bla
        if(io.ne.0.or.len_trim(bla).eq.0.or.len_trim(bla).eq.500) exit
        write(20,'(A)') trim(bla)
     end do
     
     close(10)
     close(20)
     
     call system('mv -f '//trim(outfile)//' '//trim(infile))
  end if
  
50 format (6I6,1x,2I7,/,  3(2ES11.3,I5,/),  ES11.3,I3,ES10.2,/,   ES11.3,ES12.4,6ES10.2,I6,/,      3(7ES10.2,/))
  
  
  call print_main_settings(sm,m2,per,p)
  
  write(*,'(A)') '  New init.run was written as '//trim(outfile)
  write(*,'(A,/)') '  Program done'
  stop
  
  
90 write(*,'(A,/)')'  Error opening file: '//trim(infile)
  stop
91 write(*,'(A,/)')'  Error reading file: '//trim(infile)//', line 1'
  stop
92 write(*,'(A,/)')'  Error reading file: '//trim(infile)//', line 2'
  stop
93 write(*,'(A,/)')'  Error reading file: '//trim(infile)//', line 3'
  stop
94 write(*,'(A,/)')'  Error reading file: '//trim(infile)//', line 4'
  stop
95 write(*,'(A,/)')'  Error reading file: '//trim(infile)//', line 5'
  stop
96 write(*,'(A,/)')'  Error reading file: '//trim(infile)//', line 6'
  stop
97 write(*,'(A,/)')'  Error reading file: '//trim(infile)//', line 7'
  stop
98 write(*,'(A,/)')'  Error reading file: '//trim(infile)//', line 8'
  stop
99 write(*,'(A,/)')'  Error reading file: '//trim(infile)//', line 9'
  stop
  
end program makerun
!***********************************************************************************************************************************


!***********************************************************************************************************************************
subroutine print_main_settings(sm,m2,per,p1)
  use SUFR_kinds, only: double
  use SUFR_constants, only: solday, msun,rsun
  
  implicit none
  real(double), intent(in) :: sm,m2,per,p1
  real(double) :: aorb, a2rl, Rrl1,Rrl2
  
  write(*,'(5(A,ES10.3),A)') '  M1 = ',sm,' Mo,   M2 = ',m2, ' Mo,   q1 = ',sm/m2,',   Porb = ',per, &
       ' d,   Prot1 = ',p1, ' d.'
  
  call p2a((sm+m2)*msun, per*solday, aorb)
  
  aorb = aorb/rsun
  Rrl1 = a2rl(sm,m2, aorb)
  Rrl2 = a2rl(m2,sm, aorb)
  
  write(*,'(3(A,F0.3),A)') '  aorb = ',aorb, ' Ro,   Rrl1 = ',Rrl1,' Ro,   Rrl2 = ',Rrl2,' Ro.'
  write(*,*)
  
end subroutine print_main_settings
!***********************************************************************************************************************************
