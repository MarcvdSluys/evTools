!Convert a .plt? file to a .ce file with selected columns for common-envelope calculations

program plt2ce
  implicit none   
  integer, parameter :: nff=200, n=100000,nc=89
  real*8 :: dat(nc),r,l,t,m0,r0
  integer :: i,nc1,f,nf,fl
  character :: fnames(nff)*99,fname*99
  
  m0 = 1.9891d33
  r0 = 6.9599d10
  
  call findfiles('*.plt1',6,nff,1,fnames,nf)  !Use the first nff files
  
  do f=1,nf
     fname = fnames(f)
     do fl=len_trim(fname),1,-1
        if(fname(fl:fl).eq.'.') exit
     end do
     fl = fl-1
     
     open(unit=10,form='formatted',status='old',file=fname)
     rewind(10)
     read(10,'(I4)')nc1
     if(nc1.ne.nc) write(6,'(A,I3,A,I3,A)')'Data file has',nc1,' columns, the programme is designed for',nc,' columns !'
     open(unit=20,form='formatted',status='replace',file=fname(1:fl)//'.ce')
     
     do i=1,n
        !read(10,10,err=12,end=15) dat
        read(10,*,err=12,end=15) dat
!10      format(F6.0,E17.9,E14.6,12E13.5,7E12.4,3E13.5,17E12.4,39E13.5,E14.6,E13.5,F2.0,4E13.5)
        
        r = 10.d0**dat(8)
        l = 10.d0**dat(9)
        t = 10.d0**dat(10)
        !write time, m,mhe,mco, R,L,Teff, be,be0,be1,be2,be3,I, core He, core C+O, core entropy, T10^5K entropy
        !be: Total Binding Energy, be0: Gravitational BE, 1: Internal BE, 2: Recombination BE, 3: H2 dissociation BE.   I: Moment of Inertia
        write(20,20)dat(2),dat(4:6),r,l,t, dat(15)*m0,dat(84:87)*m0, dat(22)*dat(4)*m0*(r*r0)**2, dat(57),dat(58)+dat(60), dat(88),dat(89)
20      format(ES17.9,16ES13.5)
     enddo! i=1,n
     write(6,'(A)')'End of file not reached, arrays too small!'
     goto 15
12   write(6,'(A,I,A)')'Error reading line',i-1,' of '//trim(fname)//', using the first part of the file only'
15   continue
     close(10)
     close(20)
  enddo  !f
  
end program



