!***********************************************************************************************************************************
program testplt
  use kinds, only: double
  implicit none
  integer :: spi,lci
  real(double) :: sptyp,lumcl, lum,teff
  
  call setconstants()
  
  !write(*,'(2A6, 2A8)') 'lumcl','sptyp','logT','logL'
  
  if(1.eq.1) then
     write(*,'(/,A)') 'log Teff:'
     write(*,'(8x)', advance='no')
     do lci=0,5
        lumcl = dble(lci)
        write(*,'(F8.1)', advance='no') lumcl
     end do
     write(*,*)
     
     do spi = 30,830,40 !200 !800 !40
        sptyp = dble(spi)/100.d0
        lci = 5
        write(*,'(F8.2)', advance='no') sptyp
        do lci=0,5
           lumcl = dble(lci)
           
           call num_sp_type_2_lt(sptyp,lumcl, lum,teff)
           
           write(*,'(F8.3)', advance='no') log10(teff)
        !write(*,'(F8.3)', advance='no') log10(lum)
        end do
        !write(*,'(2F6.2, 2F8.3)') lumcl,sptyp, log10(teff), log10(lum)
        write(*,*)
     end do
     write(*,*)
  end if
  
  write(*,'(/,A)') 'log L:'
  write(*,'(8x)', advance='no')
  do lci=0,5
     lumcl = dble(lci)
     write(*,'(F8.1)', advance='no') lumcl
  end do
  write(*,*)
  
  do spi = 30,790,40 !190 !760 !40
     sptyp = dble(spi)/100.d0
     lci = 5
     write(*,'(F8.2)', advance='no') sptyp
     do lci=0,5
        lumcl = dble(lci)
        
        call num_sp_type_2_lt(sptyp,lumcl, lum,teff)
     
        !write(*,'(F8.3)', advance='no') log10(teff)
        write(*,'(F8.3)', advance='no') log10(lum)
     end do
     !write(*,'(2F6.2, 2F8.3)') lumcl,sptyp, log10(teff), log10(lum)
     write(*,*)
  end do
  write(*,*)
  
  
  ! Compare to cheby40i.awk
  !write(*,*)
  !sptyp = 0.3d0
  !lumcl = 1.d0
  !do spi=1,7
  !   if(spi.eq.2) sptyp = 1.05d0
  !   if(spi.eq.3) sptyp = 2.10d0
  !   if(spi.eq.4) sptyp = 4.d0
  !   if(spi.eq.5) sptyp = 5.d0
  !   if(spi.eq.6) sptyp = 6.d0
  !   if(spi.eq.7) sptyp = 6.9d0
  !   call num_sp_type_2_lt(sptyp,lumcl, lum,teff)
  !   write(*,'(2F6.2,5x,2F7.3)') sptyp,lumcl, log10((/teff,lum/))
  !end do
  !write(*,*)
  
end program testplt
!***********************************************************************************************************************************
