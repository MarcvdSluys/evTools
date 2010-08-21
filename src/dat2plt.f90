!dat2plt:  Reads some data file with a stellar-evolution model from some code and saves the data in plt format
! taken from listplt, 03/02/2009
!
!   Copyright 2002-2010 AstroFloyd - astrofloyd.org
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


program dat2plt
  use constants
  implicit none
  integer, parameter :: nci1=99,nco=89
  integer :: i,iargc,ioi,ioo,nci,translate(nci1),skipinlines,ci
  real*8 :: dati(nci1),dato(nco)
  character :: infile*99,outfile*99,bla
  
  
  !Current settings: for Lev's file, 1/2/2009
  skipinlines = 1 !Skip the first skipinlines from the input file
  nci = 14  !Number of input columns
  translate(1:nci) = (/1,2,4, 56,57,58,59,60,61, 13,11,8,9,10/)  !Contains the target column number in the plt file for each column in the input file
  
  !  1: model      16: Lh         28: Porb      34: Horb    
  !  2: t          17: Lhe        29: FLR       35: dHorb/dt
  !  3: dt         18: Lc         30: F1        36: dHgw/dt 
  !  4: M          19: Lnu        31: dM        37: dHwml/dt
  !  5: Mhe        20: Lth        32: dMwind    38: dHmb/dt 
  !  6: Mco        21: Prot       33: dMmt      39: dHmtr/dt
  !  7: Mone       22: VK2                      40: Mcomp   
  !  8: R          23: Rcz                      41: e       
  !  9: L          24: dRcz                                 
  ! 10: Teff       25: Tet                                  
  ! 11: Tc         26: Ralv                                 
  ! 12: Tmax       27: Bp          H  He   C   N   O  Ne  Mg
  ! 13: Rhoc                Surf  42  43  44  45  46  47  48
  ! 14: RhoTm               Tmax  49  50  51  52  53  54  55
  ! 15: Ubind               Core  56  57  58  59  60  61  62
  !                                                          
  !                                                         
  ! 81: Qconv          86: Rossby nr                          
  ! 82: Pgw,max        87: Pcr (MB)                           
  ! 83: Menv           88: Sills MB                         
  ! 84: Xf             89: Tet: int/anal                    
  ! 85: R/(dR/dt)                                           
  
  
  
  
  call setconstants()
  
  
  
  
  
  
  !************************************************************************
  !***   READ COMMAND LINE VARIABLES
  !************************************************************************
  
  if(iargc().eq.1) then
     call getarg(1,infile)
  else
     write(6,'(/,A)')'  dat2plt:   convert the stellar-evolution output of a different code to the plt format of the Eggleton code'
     write(6,'(A,/)')'  syntax:    dat2plt <filename>'
     stop
  end if
  
  !Get base of input file name and create output file name using the '.plt' extension
  do i=len_trim(infile),1,-1
     if(infile(i:i).eq.'.') exit
  end do
  outfile = infile(1:i-1)//'.plt'
  
  
  write(6,*)
  write(6,'(A)')'  Input file:  '//trim(infile)
  write(6,'(A)')'  Output file: '//trim(outfile)
  
  ioi = 0
  open(unit=10,form='formatted',status='old',action='read',file=trim(infile),iostat=ioi)
  if(ioi.ne.0) then
     write(0,'(//,A,//)')'  Error opening input file '//trim(infile)//', aborting...'
     stop
  end if
  rewind 10
  
  ioo = 0
  open(unit=20,form='formatted',status='replace',action='write',file=trim(outfile),iostat=ioo)
  !open(unit=20,form='formatted',status='new',action='write',file=trim(outfile),iostat=ioo)
  if(ioo.ne.0) then
     write(0,'(//,A,//)')'  Error creating output file '//trim(outfile)//', aborting...'
     stop
  end if
  
  
  !Read first skipinlines of input file and discard them
  if(skipinlines.gt.0) then
     do i=1,skipinlines
        read(10,*)bla
     end do
  end if
  
  !Write number of columns in first line of output file
  write(20,'(I4)')nco
  
  i=0
  do while(ioi.eq.0) 
     i = i+1
     dati = 0.d0
     dato = 0.d0
     read(10,*,iostat=ioi) dati(1:nci)
     if(ioi.lt.0) exit
     if(ioi.gt.0) then
        write(0,'(//,A,I6)')'  Error reading input file '//trim(infile)//', line',i
        if(i.lt.10) then
           write(0,'(//,A)')'  Aborting...'
           stop
        else
           write(0,'(//,A)')'  I could proces only part of the input file'
           exit
        end if
     end if
     
     do ci=1,nci
        dato(translate(ci)) = dati(ci)
     end do
     
     write(20,'(I6,ES17.9,ES14.6,12ES13.5,6ES12.4,3ES13.5,17ES12.4,39ES13.5,ES14.6,ES13.5,F5.1,6ES13.5)')nint(dato(1)),dato(2:nco)
  end do
  
  close(10)
  close(20)
  
  
  write(6,'(A,I5,A,I5,A,/)')'  Read',i-1+skipinlines,' input lines, wrote',i,' output lines'
end program dat2plt

