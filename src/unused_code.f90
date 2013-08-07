



!***********************************************************************************************************************************
!> \brief  NRF random-number generator
!!
!! \param seed  Seed (I/O)

function ran1(seed)
  implicit none
  integer, intent(inout) :: seed
  integer, parameter :: ia=16807,im=2147483647,iq=127773,ir=2836,ntab=32,ndiv=1+(im-1)/ntab
  real, parameter :: am=1.0/im, eps=1.2e-7, rnmx=1.0-eps
  integer :: j,k,iv(ntab),iy
  real :: ran1
  
  save iv,iy
  iv = 0
  iy = 0
  
  if (seed.le.0.or.iy.eq.0) then
     seed = max(-seed,1)
     do j=ntab+8,1,-1
        k = seed/iq
        seed = ia*(seed-k*iq)-ir*k
        if (seed.lt.0) seed = seed+im
        if (j.le.ntab) iv(j) = seed
     end do
     iy = iv(1)
  end if
  
  k = seed/iq
  seed = ia*(seed-k*iq)-ir*k
  if (seed.lt.0) seed = seed+im
  j = 1+iy/ndiv
  iy = iv(j)
  iv(j) = seed
  ran1 = min(am*iy,rnmx)
  
end function ran1
!***********************************************************************************************************************************




!***********************************************************************************************************************************
!> \brief  Return a sorted index ind of array arr of length narr - double precision
!!
!! \param  narr  Number of elements in array arr
!! \param  arr   Data array
!! \retval ind   Array with sorted indices

subroutine dindex(narr,arr,ind)
  use SUFR_kinds, only: double
  implicit none
  integer, intent(in) :: narr
  real(double), intent(in) :: arr(narr)
  integer, intent(out) :: ind(narr)
  integer, parameter :: m=7,nstack=50
  real(double) :: a
  integer :: i,indxt,ir,itemp,j,jstack,k,l,istack(nstack)
  
  do j=1,narr
     ind(j) = j
  end do
  
  jstack = 0
  l = 1
  ir = narr
  
1 if(ir-l.lt.m) then
     do j = l+1,ir
        indxt = ind(j)
        a = arr(indxt)
        do i = j-1,l,-1
           if(arr(ind(i)).le.a) goto 2
           ind(i+1) = ind(i)
        end do
        i = l-1
2       ind(i+1) = indxt
     end do
     if(jstack.eq.0) return
     ir = istack(jstack)
     l = istack(jstack-1)
     jstack = jstack-2
     
  else
     
     k = (l+ir)/2
     itemp = ind(k)
     ind(k) = ind(l+1)
     ind(l+1) = itemp
     if(arr(ind(l)) .gt. arr(ind(ir))) then
        itemp = ind(l)
        ind(l) = ind(ir)
        ind(ir) = itemp
     end if
     if(arr(ind(l+1)) .gt. arr(ind(ir))) then
        itemp = ind(l+1)
        ind(l+1) = ind(ir)
        ind(ir) = itemp
     end if
     if(arr(ind(l)) .gt. arr(ind(l+1))) then
        itemp = ind(l)
        ind(l) = ind(l+1)
        ind(l+1) = itemp
     end if
     i = l+1
     j = ir
     indxt = ind(l+1)
     a = arr(indxt)
     
3    continue
     i = i+1
     if(arr(ind(i)).lt.a) goto 3
     
4    continue
     j = j-1
     if(arr(ind(j)).gt.a) goto 4
     if(j.lt.i) goto 5
     itemp = ind(i)
     ind(i) = ind(j)
     ind(j) = itemp
     goto 3
     
5    ind(l+1) = ind(j)
     ind(j) = indxt
     jstack = jstack+2
     if(jstack.gt.nstack) write(0,'(A)')'  *** Warning:  nstack too small in dindex  ***'
     if(ir-i+1.ge.j-l) then
        istack(jstack) = ir
        istack(jstack-1) = i
        ir = j-1
     else
        istack(jstack) = j-1
        istack(jstack-1) = l
        l = i
     end if
  end if
  
  goto 1
  
end subroutine dindex
!***********************************************************************************************************************************


!***********************************************************************************************************************************
!> \brief  Return a sorted index indx of array rarr of length n - single precision wrapper for dindex()
!!
!! \param  narr  Number of elements in array arr
!! \param  rarr  Data array
!! \retval indx  Array with sorted indices

subroutine rindex(narr,rarr, indx)
  use SUFR_kinds, only: double
  implicit none
  integer, intent(in) :: narr
  real, intent(in) :: rarr(narr)
  integer, intent(out) :: indx(narr)
  real(double) :: darr(narr)
  
  darr = dble(rarr)
  call dindex(narr,darr, indx)  ! Call the double-precision routine and return indx to calling routine
  
end subroutine rindex
!***********************************************************************************************************************************



!***********************************************************************************************************************************
!> \brief  Interpolate - single precision
!!
!! \param  xa  Array (n)
!! \param  ya  Array (n)
!! \param  n   Size of xa, ya
!! \retval x   x
!! \retval y   y
!! \retval dy  dy

subroutine polint(xa,ya,n, x,y,dy)
  implicit none
  integer, intent(in) :: n
  real, intent(in) :: xa(n),ya(n)
  real, intent(out) :: x,y,dy
  integer, parameter :: nmax=10
  integer :: i,m,ns
  real :: den,dif,dift,ho,hp,w,c(nmax),d(nmax)
  
  ns = 1
  dif = abs(x-xa(1))
  do i=1,n
     dift = abs(x-xa(i))
     if (dift.lt.dif) then
        ns = i
        dif = dift
     end if
     c(i) = ya(i)
     d(i) = ya(i)
  end do
  y = ya(ns)
  ns = ns-1
  do m=1,n-1
     do i=1,n-m
        ho = xa(i)-x
        hp = xa(i+m)-x
        w = c(i+1)-d(i)
        den = ho-hp
        if(den.eq.0.) write(0,'(A)')'  *** Warning:  Failure in polint  ***'
        den = w/den
        d(i) = hp*den
        c(i) = ho*den
     end do
     if (2*ns.lt.n-m) then
        dy = c(ns+1)
     else
        dy = d(ns)
        ns = ns-1
     end if
     y = y+dy
  end do
  
end subroutine polint
!***********************************************************************************************************************************




!***********************************************************************************************************************************
!> \brief  Interpolate - double precision
!!
!! \param  xa  Array (n)
!! \param  ya  Array (n)
!! \param  n   Size of xa, ya
!! \retval x   x
!! \retval y   y
!! \retval dy  dy

subroutine polintd(xa,ya,n, x,y,dy)
  use SUFR_kinds, only: double
  implicit none
  integer, intent(in) :: n
  real(double), intent(in) :: xa(n),ya(n)
  real(double), intent(out) :: x,y,dy
  integer, parameter :: nmax=10
  integer :: i,m,ns
  real(double) :: den,dif,dift,ho,hp,w,c(nmax),d(nmax)
  
  ns = 1
  dif = abs(x-xa(1))
  do i=1,n
     dift = abs(x-xa(i))
     if (dift.lt.dif) then
        ns = i
        dif = dift
     end if
     c(i) = ya(i)
     d(i) = ya(i)
  end do
  y = ya(ns)
  ns = ns-1
  do m=1,n-1
     do i=1,n-m
        ho = xa(i)-x
        hp = xa(i+m)-x
        w = c(i+1)-d(i)
        den = ho-hp
        if(den.eq.0.d0) write(0,'(A)')'  *** Warning:  Failure in polintd  ***'
        den = w/den
        d(i) = hp*den
        c(i) = ho*den
     end do
     if (2*ns.lt.n-m) then
        dy = c(ns+1)
     else
        dy = d(ns)
        ns = ns-1
     end if
     y = y+dy
  end do
  
end subroutine polintd
!***********************************************************************************************************************************



!***********************************************************************************************************************************
!> \brief  Spline interpolation

subroutine spline(x,y,n,yp1,ypn,y2)
  use SUFR_kinds, only: double
  implicit none
  integer :: n,i,k
  real(double) :: yp1,ypn,x(n),y(n),y2(n)
  real(double) :: p,qn,sig,un,u(n)
  
  if(yp1.gt.0.99d30) then
     y2(1) = 0.
     u(1) = 0.
  else
     y2(1) = -0.5
     u(1) = (3./(x(2)-x(1)))*((y(2)-y(1))/(x(2)-x(1))-yp1)
  end if
  
  do i=2,n-1
     sig = (x(i)-x(i-1))/(x(i+1)-x(i-1))
     p = sig*y2(i-1)+2.
     y2(i) = (sig-1.)/p
     u(i) = (6 * ( (y(i+1)-y(i)) / (x(i+1)-x(i)) - (y(i)-y(i-1)) / (x(i)-x(i-1)) ) / (x(i+1)-x(i-1)) - sig*u(i-1)) / p
  end do
  
  if(ypn.gt..99e30) then
     qn = 0.
     un = 0.
  else
     qn = 0.5
     un = (3./(x(n)-x(n-1)))*(ypn-(y(n)-y(n-1))/(x(n)-x(n-1)))
  end if
  
  y2(n) = (un-qn*u(n-1))/(qn*y2(n-1)+1.)
  do k=n-1,1,-1
     y2(k) = y2(k)*y2(k+1)+u(k)
  end do
  
end subroutine spline
!***********************************************************************************************************************************


!***********************************************************************************************************************************
!> \brief  Spline interpolation

subroutine splint(xa,ya,y2a,n,x,y)
  use SUFR_kinds, only: double
  implicit none
  integer :: n,k,khi,klo
  real(double) :: x,y,xa(n),y2a(n),ya(n),a,b,h
  
  klo = 1
  khi = n
1 if(khi-klo.gt.1) then
     k = (khi+klo)/2
     if(xa(k).gt.x) then
        khi = k
     else
        klo = k
     end if
     goto 1
  end if
  
  h = xa(khi)-xa(klo)
  if(h.eq.0.) write(0,'(A)')'  *** Warning:  Bad xa input in splint  ***'
  a = (xa(khi)-x)/h
  b = (x-xa(klo))/h
  y = a*ya(klo)+b*ya(khi)+((a**3-a)*y2a(klo)+(b**3-b)*y2a(khi))*(h**2)/6.
  
end subroutine splint
!***********************************************************************************************************************************
