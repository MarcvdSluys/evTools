!> \file selplt.f90  From a .plt? file, or a series of plot files, select the models that fulfill certain conditions and write them to a new file
!!  Use this on a grid of models, e.g. to pick out the models that fit V = 10+-1 and B-V = 1.1+-0.1

! AF, July 2007


! Copyright 2002-2010 AstroFloyd - astrofloyd.org
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


program selplt
  use constants
  implicit none
  ! ndim is the number of dimensions of the conditions array (different variables, for AND), 
  ! ncondmax is the max number of conditions (for OR)
  integer, parameter :: nn=2000,nff=1000,nvar=210,ndim=2,ncondmax=99
  real(double) :: dat(nvar,nn),dato(nvar,nn),x(ndim),y(ndim,ncondmax),dy(ndim,ncondmax),sumsq,maxtime!,zsurf
  integer :: i,j,n,f,nf,sel,cd,d,indx(ndim),ver,dpdt,ncond,ndata,nsel
  character :: fnames(nff)*99,fname*99,labels(nvar)*99
  
  call setconstants()
  
  maxtime = 13.d9
  indx = (/10,9/) !Teff, L
  
  ncond = 17  !Do all
  ncond = 11  !Do lower clump only
  
  !From Reddy & Lambert, ApJ 129, 2831 (2005)
  !Lower clump:      Teff:          logL:
  y(:,1)  = (/   4.7010E+03  ,   1.8977E+00  /)  !Open symbol:  Reddy et al. 2002a/Drake et al. 2002
  y(:,2)  = (/   4.5376E+03  ,   1.8523E+00  /)  !Open symbol:  Reddy et al. 2002a/Drake et al. 2002
  y(:,3)  = (/   4.5989E+03  ,   1.6989E+00  /)  !Open symbol:  Reddy et al. 2002a/Drake et al. 2002
  y(:,4)  = (/   4.4967E+03  ,   1.6989E+00  /)  !Open symbol:  Reddy et al. 2002a/Drake et al. 2002
  y(:,5)  = (/   4.2966E+03  ,   1.5966E+00  /)  !Open symbol:  Reddy et al. 2002a/Drake et al. 2002
  y(:,6)  = (/   4.7010E+03  ,   1.7500E+00  /)  !Closed symbol:  Charbonnel & Balachandran 2000
  y(:,7)  = (/   4.6397E+03  ,   1.9318E+00  /)  !Closed symbol:  Charbonnel & Balachandran 2000
  y(:,8)  = (/   4.5703E+03  ,   2.0682E+00  /)  !Closed symbol:  Charbonnel & Balachandran 2000
  y(:,9)  = (/   4.4722E+03  ,   2.0000E+00  /)  !Closed symbol:  Charbonnel & Balachandran 2000
  y(:,10) = (/   4.3987E+03  ,   1.9205E+00  /)  !Closed symbol:  Charbonnel & Balachandran 2000
  y(:,11) = (/   4.3374E+03  ,   1.5739E+00  /)  !Closed symbol:  Charbonnel & Balachandran 2000
  !Upper clump:      Teff:          logL:  
  y(:,12) = (/   4.1005E+03  ,   2.9545E+00  /)  !Open symbol:  Reddy et al. 2002a/Drake et al. 2002
  y(:,13) = (/   3.9003E+03  ,   2.8125E+00  /)  !Closed symbol:  Charbonnel & Balachandran 2000
  y(:,14) = (/   4.1005E+03  ,   2.8807E+00  /)  !Closed symbol:  Charbonnel & Balachandran 2000
  y(:,15) = (/   4.1985E+03  ,   2.8523E+00  /)  !Closed symbol:  Charbonnel & Balachandran 2000
  y(:,16) = (/   4.1985E+03  ,   2.7614E+00  /)  !Closed symbol:  Charbonnel & Balachandran 2000
  y(:,17) = (/   4.1822E+03  ,   2.7898E+00  /)  !Closed symbol:  Charbonnel & Balachandran 2000
  
  
  ! Lower clump:   Teff-err:      logL-err:
  dy(:,1)  = (/   1.0008E+02  ,   2.0170E-01  /)  !Open symbol:  Reddy et al. 2002a/Drake et al. 2002
  dy(:,2)  = (/   1.0008E+02  ,   2.0170E-01  /)  !Open symbol:  Reddy et al. 2002a/Drake et al. 2002
  dy(:,3)  = (/   1.0212E+02  ,   1.7898E-01  /)  !Open symbol:  Reddy et al. 2002a/Drake et al. 2002
  dy(:,4)  = (/   9.8039E+01  ,   2.0455E-01  /)  !Open symbol:  Reddy et al. 2002a/Drake et al. 2002
  dy(:,5)  = (/   1.0212E+02  ,   2.0170E-01  /)  !Open symbol:  Reddy et al. 2002a/Drake et al. 2002
  dy(:,6)  = (/   1.0212E+02  ,   2.0170E-01  /)  !Closed symbol:  Charbonnel & Balachandran 2000    
  dy(:,7)  = (/   1.0212E+02  ,   2.0455E-01  /)  !Closed symbol:  Charbonnel & Balachandran 2000    
  dy(:,8)  = (/   1.0008E+02  ,   2.0170E-01  /)  !Closed symbol:  Charbonnel & Balachandran 2000    
  dy(:,9)  = (/   9.8039E+01  ,   2.0170E-01  /)  !Closed symbol:  Charbonnel & Balachandran 2000    
  dy(:,10) = (/   1.0008E+02  ,   2.0170E-01  /)  !Closed symbol:  Charbonnel & Balachandran 2000    
  dy(:,11) = (/   1.0008E+02  ,   2.0170E-01  /)  !Closed symbol:  Charbonnel & Balachandran 2000    
  ! Lower clump:   Teff-err:      logL-err:  
  dy(:,12) = (/   1.0008E+02  ,   3.0114E-01  /)  !Open symbol:  Reddy et al. 2002a/Drake et al. 2002
  dy(:,13) = (/   1.0008E+02  ,   2.0170E-01  /)  !Closed symbol:  Charbonnel & Balachandran 2000    
  dy(:,14) = (/   1.0008E+02  ,   2.0455E-01  /)  !Closed symbol:  Charbonnel & Balachandran 2000    
  dy(:,15) = (/   1.0008E+02  ,   1.9886E-01  /)  !Closed symbol:  Charbonnel & Balachandran 2000    
  dy(:,16) = (/   1.0008E+02  ,   2.0170E-01  /)  !Closed symbol:  Charbonnel & Balachandran 2000    
  dy(:,17) = (/   9.8039E+01  ,   2.0455E-01  /)  !Closed symbol:  Charbonnel & Balachandran 2000    
  
  
  
  
  call findfiles('*.plt1',nff,1,fnames,nf)  !Find all files (1)
  
  open(unit=20,form='formatted',status='replace',file='selection.plt1')
  write(20,'(I2)')81 !Save only first 81 colums
  
  ndata = 0
  nsel = 0
  j = 1
  do f=1,nf
     fname = fnames(f)
     !print*,f,nf,trim(fname)
     if(trim(fname).eq.'selection.plt1') cycle
     
     ! When nn is large, this somehow takes a very long time!!!
     call readplt(10,fname,nn,nvar,81,0,dat,n,ver)  !unit=10, nc=81, verbose=0
     dato = dat  !Save original data, for output
     
     call changepltvars(nn,nvar,n,dat,labels,dpdt)  !Change (e.g. de-log) and add plot variables
     
     
     !open(unit=10,form='formatted',status='old',file=fname)
     !rewind(10)
     !read(10,'(I4)')nc
     
     !do j=1,nn
     do j=1,n
        ndata = ndata + 1
        
        if(dat(2,j).gt.maxtime) cycle

        
        !if(nc.eq.81) 
        !read(10,'(F6.0,E17.9,E14.6,12E13.5,7E12.4,3E13.5,16E12.4,39E13.5,E14.6)',err=12,end=15) (dat(i,j),i=1,81)  !81 Columns
        !if(nc.gt.81) read(10,'(F6.0,E17.9,E14.6,12E13.5,7E12.4,3E13.5,16E12.4,39E13.5,E14.6,ES13.5,F2.0)',err=12,end=15) &
        !(dat(i,j),i=1,83)  !83 Columns, Evert(?) added 82, 83=strmdl flag
        
        !zsurf = 1.d0 - dat(42,j) - dat(43,j)                   !Z_surf = 1 - X - Y
        !call lt2ubv(dat(9,j),dat(10,j),dat(4,j),dlog10(zsurf/2.d-2),dat(81,j),dat(82,j),dat(83,j),dat(84,j),dat(85,j))
        !dat(86,j) = dat(82,j)+dat(83,j)  ! (U-V) = (U-B) + (B-V)
        !dat(87,j) = dat(84,j)+dat(85,j)  ! (V-I) = (V-R) + (R-I)
        
        
        !x = (/10.d0**dat(indx(1),j),dat(indx(2),j)/)  !Teff and log(L)
        x = (/dat(indx(1),j),dlog10(dat(indx(2),j))/)  !Teff and log(L)
        !print*,j,x
        
        
        
        !See if point lies in rectangle:
        !sel = 0
        !do cd=1,ncond
        !   dsel = 0
        !   do d=1,ndim
        !      if(dabs(x(d)-y(d,cd)).lt.dy(d,cd)) dsel = dsel+1 
        !   end do
        !   if(dsel.eq.ndim) sel = 1 !Then the condition applies for all dimensions (square)
        !end do
        
        
        !See if point lies in ellipse:
        sel = 0
        do cd=1,ncond
           sumsq = 0.d0
           do d=1,ndim
              sumsq = sumsq + ((x(d)-y(d,cd))/dy(d,cd))**2
           end do
           if(sumsq.lt.1.d0) sel = 1 !Then the point lies within the/an ellips(oid)
        end do
        
        !print*,sel
        
        if(sel.eq.1) then !Write output when ANY of the conditions (OR) holds for ALL (AND) dimensions
           !if(nc.eq.81) write(20,'(I6,ES17.9,ES14.6,12ES13.5,7ES12.4,3ES13.5,16ES12.4,39ES13.5,ES14.6)') nint(dato(1,j)), &
           !(dato(i,j),i=2,81)  !81 Columns
           !if(nc.ne.81) 
           !write(20,'(I6,ES17.9,ES14.6,12ES13.5,7ES12.4,3ES13.5,16ES12.4,39ES13.5,ES14.6,ES13.5,I2)') nint(dato(1,j)), &
           !(dato(i,j),i=2,82),nint(dato(83,j))  !83 Columns, Evert(?) added 82, 83=strmdl flag
           
           !Just print 81 Columns:
           !write(20,'(I6,ES17.9,ES14.6,12ES13.5,7ES12.4,3ES13.5,16ES12.4,39ES13.5,ES14.6)') nint(dat(1,j)),(dat(i,j),i=2,81)
           write(20,'(I6,ES17.9,ES14.6,12ES13.5,7ES12.4,3ES13.5,16ES12.4,39ES13.5,ES14.6)') nint(dato(1,j)),(dato(i,j),i=2,81)
           nsel = nsel + 1
        end if
     end do! m/j=1,n
     
     
     goto 15
     write(6,'(A,I5,A)')'Error reading line',i-1,' of '//trim(fname)//', using the first part of the file only'
15   continue
     close(10)
  end do  !f
  close(20)
  
  write(6,'(I6,A1,I6,A,F4.1,A/)')nsel,'/',ndata,' data points (',real(nsel)/real(ndata)*100,'%) selected.'
end program selplt



