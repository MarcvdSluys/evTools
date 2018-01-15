!> \file plt2dat.f90  Convert a .plt[12] file to a .dat file with selected columns


! Copyright 2002-2018 AstroFloyd - astrofloyd.org
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
program plt2dat
  use SUFR_kinds, only: double
  
  implicit none
  integer, parameter :: nff=200, n=100000,nc=89
  integer,parameter :: nmax=10000,nvar=229
  integer :: i,j,f,nf,fl,sel(99),nsel,io2, version,nfi,verbose,dpdt, defvar(0:nvar)
  real(double) :: datf(nvar,nmax)
  character :: fnames(nff)*(99),infile*(99),outfile*(99), pglabels(nvar)*(99),asclabels(nvar)*(99)
  
  call setconstants()
  verbose = 1
  write(*,*)
  
  ! Give the columns of the plt file which you want to output:
  !nsel = 7       !t,M,R,L,Teff,M_He,X_c
  !sel(1:nsel) = (/2,4,8,9,10,  5,   56/)
  
  ! For Lennart (May 2010):
  !nsel = 15      !t,M,Mhe,R,L,Teff,Po,Mtr, Xc,Xs,Ys,Cs,Ns,Os,Qcnv
  !sel(1:nsel) = (/2,4,  5,8,9,  10,28, 33, 56,42,43,44,45,46,  81/)
  
  ! Simple version for Lennart (May 2010):
  !nsel = 4      ! i,t,Po,Mtr
  !sel(1:nsel) = (/1,2,28, 33/)
  
  ! Silvia: MB, 12/2011:
  !nsel = 12
  !               t  Xc, Mc R,L,Te, Qconv RosNr Tet  Tet int/anal Pcr SillsMB
  !sel(1:nsel) = (/2, 56, 5, 8,9,10, 81,   120,  25,  123,         121,122     /)
  
  write(*,'(A)') '  Creating output for Silvia:  HG timescales, 30/07/2012'
  nsel = 8
  write(*,'(A,I4,A)', advance='no') '  Writing',nsel, ' variables:  '
  write(*,'(A)') 't, M, P,  a_orb, dMmt, t_nuc, t_KH, t_MT'
  sel(1:nsel) = (/2, 4, 28, 143,   33,   201,   202,  203  /)
  
  
  ! Use the first nff files of *.plt1, *.plt or *.plt2:
  call findfiles('*.plt1',nff,1,fnames,nf)
  if(nf.eq.0) call findfiles('*.plt',nff,1,fnames,nf)
  if(nf.eq.0) call findfiles('*.plt2',nff,1,fnames,nf)
  
  ! Get the labels for the variables; defvar = 0 for non-defined variables:
  call getpltlabels(nf,nvar,pglabels,asclabels,defvar)
  asclabels(201:205) = [character(len=99) :: 'tau_nuc', 'tau_th', 'tau_MT', 'tau_GW', 'tau_dyn' ]
  
  do i=1,nsel
     write(*,'(I4,A,I3.3,A)') i,':  '//trim(asclabels(sel(i)))//'  (',sel(i),')'
  end do
  
  
  do f=1,nf
     infile = fnames(f)
     do fl=len_trim(infile),1,-1
        if(infile(fl:fl).eq.'.') exit
     end do
     fl = fl-1
     outfile = infile(1:fl)//'.dat'
     
     
     ! Read input file:
     call read_plt_bse(10,trim(infile),nmax,nvar,nc,verbose,datf,nfi,version)  ! Use unit 10
     
     ! Change (e.g. de-log) and add plot variables:
     call changepltvars(nmax,nvar,nfi,datf,pglabels,dpdt)
     
     ! Write output file:
     open(unit=20,form='formatted',status='replace',file=trim(outfile),iostat=io2)
     if(io2.ne.0) then
        write(0,'(A,/)') '  Error opening '//trim(outfile)//', aborting...'
        stop
     end if
     
     do i=1,nfi  ! line
        do j=1,nsel  ! variable
           write(20,'(ES17.9)', advance='no',iostat=io2)datf(sel(j),i)
           if(io2.gt.0) then
              write(0,'(A,I4,A,/)') '  Error writing to '//trim(outfile)//', line',i,' aborting...'
              stop
           end if
        end do
        write(20,'(A)')''
     end do! i=1,n
     
     if(i.ge.n) write(0,'(A)') '  End of file not reached, arrays too small!'
     
     
     close(10)
     close(20)
  end do  ! f
  
  write(*,'(A,/)') '  Output written to '//trim(outfile)
  
end program plt2dat
!***********************************************************************************************************************************

