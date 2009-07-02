!Findplt.f:  cloned from findp.f
!Reads plt file and displays properties of the interpolated model with a certain value for a certain variable
!Line width > 72 chars, so compile with --wide or -extend_source or whatever is needed
!Needs the file ~/usr/lib/UBVRI.Kur to calculate magnitudes and colours (see line 15)

program findplt
  use constants
  use ubvdata
  implicit none
  integer, parameter :: nn=30000,nnn=100,ny=18
  real*8 :: x(nnn),x1(nnn),xi(nnn),xfind,a,b
  integer :: i,j,ncols,prmdl,succ,narg,iargc,iin,iout,glt,io
  character :: findfile*20,fname*99,arg*99
  
  call setconstants()
  
  !Read atmosphere-model data
  open(unit=10, file=trim(homedir)//'/usr/lib/UBVRI.Kur',status='old',action='read',iostat=io)
  if(io.eq.0) then
     read(10,*)ubv
     close(10)
  else
     write(6,'(A)')" Warning:  I can't find the file ~/usr/lib/UBVRI.Kur, so I can't calculate colours and magnitudes..."
  end if
  
  !Read the filename from the command line if any, search the current directory otherwise
  narg = iargc()
  if(narg.lt.3.or.narg.gt.4) then
     write(6,'(A)')'Usage:  FINDPLT  <file.plt> <variable> <value>'
     write(6,'(A)')'        findplt finds every instant where <variable> becomes <value>, using interpolation'
     write(6,*)''
     write(6,'(A)')'<variable>:                                                              '
     write(6,'(A)')'  1: model        16: Lh           28: Porb        34: Horb              '
     write(6,'(A)')'  2: t            17: Lhe          29: FLR         35: dHorb/dt          '
     write(6,'(A)')'  3: dt           18: Lc           30: F1          36: dHgw/dt           '
     write(6,'(A)')'  4: M            19: Lnu          31: dM          37: dHwml/dt          '
     write(6,'(A)')'  5: Mhe          20: Lth          32: dMwind      38: dHmb/dt           '
     write(6,'(A)')'  6: Mco          21: Prot         33: dMmt        39: dHmtr/dt          '
     write(6,'(A)')'  7: Mone         22: VK2                          40: Mcomp             '
     write(6,'(A)')'  8: log R        23: Rcz                          41: e                 '
     write(6,'(A)')'  9: log L        24: dRcz                                               '
     write(6,'(A)')' 10: log Teff     25: Tet                                                '
     write(6,'(A)')' 11: log Tc       26: Ralv                                               '
     write(6,'(A)')' 12: log Tmax     27: Bp                 H  He   C   N   O  Ne  Mg       '
     write(6,'(A)')' 13: log Rhoc                    Surf:  42  43  44  45  46  47  48       '
     write(6,'(A)')' 14: log RhoTm                   Tmax:  49  50  51  52  53  54  55       '
     write(6,'(A)')' 15: Ub,env                      Core:  56  57  58  59  60  61  62       '
     write(6,'(A)')'                                                                         ' 
     write(6,'(A)')' 91: V   92: U-B   93: B-V   94: V-I   95: I-R   96: U-V   97: V-R       '
     write(6,'(A)')' 99: Zsurf                                                               '
     write(6,'(A)')'                                                                         '
     write(6,'(A)')'<value>: value to find for <variable>                                    '
     write(6,'(A)')'                                                                         ' 
     write(6,'(A)')'                                                                         '
     fname=findfile('*.plt*',6)
     write(6,'(A27,$)')'Give the variable (1-99): '
     read*,iin
     write(6,'(A17,$)')'Give the value: '
     read*,xfind
  else
     call getarg(1,fname)
     call getarg(2,arg)
     read(arg,*)iin
     call getarg(3,arg)
     read(arg,*)xfind
     iout = 0
     if(narg.eq.4) then
        call getarg(4,arg)
        read(arg,*)iout
     end if
  end if
  
  
  if(fname(1:3).eq.'   ') goto 9999
  
  open(unit=10,form='formatted',status='old',file=fname)
  rewind 10
  read(10,*)ncols
  
  !print*,ncols,'columns'
  
  succ = 0
  glt = 1  		!glt determines whether we search the first model where:  x(iin) < xfind (glt=1),   or  x(iin) > xfind (glt=2)
  
  do j=1,nn
     read(10,10,err=11,end=15) x(1:ncols)
     !10   format(F6.0,E17.9,E14.6,11F9.5,7E12.4,3F9.5,16E12.4,F8.4,21E13.5,12F9.5,6F9.5,E14.6,E12.5) !Can read upto 82 columns
10   format(F6.0,E17.9,E14.6,11F9.5,7E12.4,3F9.5,16E12.4,F8.4,21E13.5,12F9.5,6F9.5,E14.6)
     
     !Calculate colours/magnitude
     x(99) = 1.d0 - x(42) - x(43)			!Z_surf = 1 - X - Y
     call lt2ubv(x(9),x(10),x(4),dlog10(x(99)/2.d-2),x(91),x(92),x(93),x(94),x(95))
     x(96) = x(92)+x(93)  ! (U-V) = (U-B) + (B-V)
     x(97) = x(94)+x(95)  ! (V-I) = (V-R) + (R-I)
     
     if(j.eq.1.and.x(iin).lt.xfind) glt = 2
     
     if(j.gt.1) then
        if((glt.eq.1.and.x(iin).lt.xfind.and.x1(iin).gt.xfind).or.   &   !this is the first model where x < xfind
             (glt.eq.2.and.x(iin).gt.xfind.and.x1(iin).lt.xfind)) then   !this is the first model where x > xfind
           do i=1,nnn
              a     = (x(i)-x1(i))/(x(iin)-x1(iin))  !Interpolate all variables x(1-81), put them in xi
              b     = x1(i) - a*x1(iin)
              xi(i) = a*xfind + b
           end do
           succ = 1
           glt = 3-glt  !Change 1 <==> 2
           
           !Print (some of) the interpolated values
           call printmodel(nnn,xi,iin,iout)
           
        end if !if((glt.eq.1.and.x(iin).lt.xfind.and.x1(iin).gt.xfind).or.(glt.eq.2.and.x(iin).gt.xfind.and.x1(iin).lt.xfind)) then
     end if !if(j.gt.1) then
     
     prmdl = nint(x(1))
     x1 = x		!For the next loop: the previous value for x
  end do !j=1,nn
  goto 15
  
11 write(6,*)''
  print*,'  Error reading file after line ',j-1,', model ',prmdl,'!'
  write(6,'(A)')'  Only the first part of the file will be processed.'
  write(6,*)''
15 close(10)
  
  if(succ.eq.0) then
     !write(6,'(A)')' Value not found, aborting... '
     !goto 9999
     write(6,*)''
     write(6,'(A)')' *** Value not found, printing last model ***'
     call printmodel(nnn,x,iin,iout)
     goto 9999
  end if
21 format(A18,A33,A1,A9,2x,6(A6,F6.3),A8,ES10.2,A8,I5)
  
  
  if(1.eq.2) write(6,'(ES12.5)')x(2)
  
  
9999 continue
end program findplt
!************************************************************************      




!************************************************************************      
subroutine printmodel(n,x,iin,iout)  !Prints a selected (interpolated) model
  implicit none
  integer :: n,iin,iout
  real*8 :: x(n)
  
  if(iout.eq.0) then
     write(6,*)''
     write(6,*)''
     
     write(6,'(A10,5x,A10,A12,4A8,A12)')'General:','Mdl','t (Gyr)','M (Mo)','log R','log L','log Te','dM/dt'
     write(6,'(15x,f10.3,es12.5,4f8.4,es12.4)')x((/1,2,4,8,9,10,33/))
     
     write(6,'(A10,5x,5A10,4A6)')'Surface:','H','He','C','N','O','V','B-V','V-I','U-V'
     write(6,'(15x,5ES10.3,4F6.2)')x((/42,43,44,45,46,91,93,94,96/))
     
     write(6,'(A10,5x,5A10,3A8,A10)')'Core:','H','He','C','N','O','log Tc','log rho','Mhe'
     write(6,'(15x,5es10.3,3f8.4)')x((/56,57,58,59,60,11,13,5/))
     write(6,*)''
  else
     write(6,'(70x,2G)')x(iin),x(iout)
  end if
  
  
  if(1.eq.2) then 
     write(6,*)''
     write(6,'(F8.4,ES12.5,$)')x((/4,2/))
  end if
  
end subroutine printmodel
!************************************************************************      












