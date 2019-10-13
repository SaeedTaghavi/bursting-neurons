subroutine linspace(start, sstop, num, array)
!!!
!!! Returns `num` evenly spaced samples, calculated over the
!!! interval [`start`, `stop`].
!!!
   implicit none
   integer::num,i
   real(8)::start, sstop, array(num)
   real(8):: diff
   diff = (sstop - start)/real(num-1)
!   print*,start,sstop
!   print*,diff
   array(1) = start
   array(num) = sstop
   do i = 1 , num-2
      array(i+1) = array(1)+(i)*diff
   end do
end subroutine linspace

