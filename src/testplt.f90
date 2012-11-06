!***********************************************************************************************************************************
program selplt
  use kinds, only: double
  implicit none
  integer :: spi,lci
  real(double) :: sptyp,lumcl, lum,teff
  
  call setconstants()
  
  !write(*,'(2A6, 2A8)') 'lumcl','sptyp','logT','logL'
  
  if(1.eq.1) then
     write(*,'(/,8x)', advance='no')
     do lci=0,5
        lumcl = dble(lci)
        write(*,'(F8.1)', advance='no') lumcl
     end do
     write(*,*)
     
     do spi = 30,830,800 !40
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
  
  write(*,'(/,8x)', advance='no')
  do lci=0,5
     lumcl = dble(lci)
     write(*,'(F8.1)', advance='no') lumcl
  end do
  write(*,*)
  
  do spi = 30,790,760 !40
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

end program selplt
!***********************************************************************************************************************************
