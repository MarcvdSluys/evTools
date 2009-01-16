! Merges the data contained in two .plt-files
! This program merges data from the plot output file from Eggletons code, the TWIN version
! AF 21-01-2004

program mergeplt      
  implicit none
  integer :: nn,nnn
  parameter (nn=30000,nnn=100)
  real*8 :: dat1(nnn,nn),dat2(nnn,nn)
  real :: x
  integer :: model1(nn),model2(nn)
  integer :: narg,iargc,l1,l2,lo

  integer :: i,j,n1,n2,ncols1,ncols2
  character :: fin1*99,fin2*99,fout*99


  narg = iargc()
  if(narg.eq.3) then
     call getarg(1,fin1)
     call getarg(2,fin2)
     call getarg(3,fout)
     do i=1,50
        if(fin1(i:i).ne.' ') l1 = i !Use trim instead
        if(fin2(i:i).ne.' ') l2 = i
        if(fout(i:i).ne.' ') lo = i
     end do
  end if
  if(narg.ne.3) then
     write(*,'(A)')'mergeplt: merges the contents of two plot files to a third file'
     write(*,'(A)')'          syntax:  mergeplt <infile1> <infile2> <outfile>'
     goto 9999
  end if

  write(*,*)''
  write(*,'(A)')'  Reading file '//trim(fin1)
  open (unit=10,form='formatted',status='old',file=fin1(1:l1)) !Use trim instead
  rewind 10
  read(10,*)ncols1
  if(ncols1.gt.nnn+1) then
     write(*,'(A)')'  The array DAT is too small for the number of columns'
     write(*,'(A)')'  Aborting...'
     goto 9999
  end if
  do j=1,nn
     read(10,*,err=12,end=11) model1(j), (dat1(i,j),i=1,ncols1-1)
  end do
  write(*,'(A)')'  End of file reached, arrays too small!'
  close(10)
  goto 15

11 write(*,'(A,I6,A,I6,A,I6)')'  End of the file reached,',j-1,' lines read: model',model1(1),' - ',model1(j-1)
  write(*,'(A8,ES14.7,A3,ES14.7)')'  time: ', dat1(1,1),' - ',dat1(1,j-1)
  close(10)
  goto 15

12 write(*,*)'  File unreadable after line ',j-1
  write(*,'(A)')'  Make sure files overlap !!!'
  write(*,*)''
  write(*,'(A,I6,A,I6,A,I6)')'  ',j-1,' lines read: model', model1(1),' - ',model1(j-1)
  write(*,'(A8,ES14.7,A3,ES14.7)')'  time: ', dat1(1,1),' - ',dat1(1,j-1)
  close(10)
  !goto 9999

15 n1=j-1


  write(*,*)''


  write(*,'(A)')'  Reading file '//trim(fin2)
  open (unit=20,form='formatted',status='old',file=fin2(1:l2)) !Use trim instead
  rewind 20
  read(20,*)ncols2
  if(ncols2.ne.ncols1) then
     write(*,'(A)')'  The two files have different numbers of columns!!!'  !This can happen because I write 81 cols max.
     !write(*,'(A)')'  Aborting...'
     !goto 9999
  end if
  do j=1,nn
     read(20,*,err=22,end=21) model2(j), (dat2(i,j),i=1,ncols1-1)
  end do
  write(*,'(A)')'  End of file reached, arrays too small!'
  close(20)
  goto 25

21 write(*,'(A,I6,A,I6,A,I6)')'  End of the file reached,',j-1,' lines read: model',model2(1),' - ',model2(j-1)
  write(*,'(A8,ES14.7,A3,ES14.7)')'  time: ', dat2(1,1),' - ',dat2(1,j-1)
  close(10)
  goto 25

22 write(*,*)'  File unreadable after line ',j-1
  write(*,'(A)')'  Continuing process, check the result !!!'
  write(*,*)''
  write(*,'(A,I6,A,I6,A,I6)')'  ',j-1,' lines read: model', model2(1),' - ',model2(j-1)
  write(*,'(A8,ES14.7,A3,ES14.7)')'  time: ', dat2(1,1),' - ',dat2(1,j-1)
  close(20)
  !goto 9999

25 n2=j-1

  write(*,*)''
  
  if(ncols1.gt.81) then
     write(*,'(A)')'  The number of columns is larger that 81, I can only save the first 81!!!'
     ncols1 = 81
  end if
  
  open(unit=30,form='formatted',status='new',file=fout(1:lo),iostat=i) !Use trim instead
  if(i.ne.0) then
     write(*,'(A)')'  File already exists: '//fout(1:lo) !Use trim instead
     write(*,'(A)')'  Aborting...'
     goto 9999
  end if
  write(*,'(A)')'  Creating output file: '//fout(1:lo) !Use trim instead
  write(30,'(I4)')ncols1
  do j=1,n1
     !if(model1(j).ge.model2(1)) goto 28
     if(dat1(1,j).ge.dat2(1,1)) goto 28 !Use time rather than model number
     if(ncols1.le.81) write(30,30) model1(j), (dat1(i,j),i=1,ncols1-1)
     !if(ncols1.gt.81) write(30,40) model1(j), (dat1(i,j),i=1,ncols1-1)
  end do

28 do j=1,n2
     if(ncols1.le.81) write(30,30) model2(j), (dat2(i,j),i=1,ncols1-1)
     !if(ncols1.gt.81) write(30,40) model2(j), (dat2(i,j),i=1,ncols1-1)
  end do
  close(30)

30 format(I6,ES17.9,ES14.6,11F9.5,7ES12.4,3F9.5,16ES12.4,F8.4,21ES13.5,12F9.5,6F9.5,ES14.6)
!40 format()





  write(*,'(A)')'  Program finished'
9999 write(*,*)''
end program mergeplt

