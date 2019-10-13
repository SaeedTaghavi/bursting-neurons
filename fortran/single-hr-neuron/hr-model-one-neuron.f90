!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!hr neuron model!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!! the model is based on the paper:
!!! Do Brain Networks Evolve by Maximizing Their Information Flow Capacity?
!!! By: C. G. Antonopoulos, et al. PLOS computational biology, 2015
!!!

program HRmodel
   implicit none
   integer, parameter :: dp = kind(1.0d0)
   integer, parameter :: seed = 12345
   integer, parameter :: num_time_steps = 10000
   real(dp), parameter :: dt=.1
   real(dp) :: p, q, n
   real(dp) :: a, b, c, d, p0, r, s,Iext(num_time_steps)
   real(dp) :: dpdt, dqdt, dndt
   real(dp) :: pdot,qdot,ndot
   integer :: i
   open(unit=1,file="output.txt")
   !!! for the following parameters system exhibits a multi-scale chaotic behavior characterized as spike bursting
   a = 1.0_dp
   b = 3.0_dp
   c = 1.0_dp
   d = 5.0_dp
   s = 4.0_dp
   p0 = -1.6_dp
   
   ! Iext = 3.25_dp

   Iext = -1.5_dp
   Iext(1:3000)=3.2_dp
   Iext(6001:8000)=2.2_dp

   !!! r modulates the slow dynamics of the system
   r = 0.005_dp

   ! call linspace(0.0_dp, t_end, num_time_steps, t_arr)
   ! dt = t_arr(2) - t_arr(1)

   call srand(seed)
   p = rand()
   q = rand()
   n = rand()
   write(1,*)"#","    ","step","    ", "time" ,"    ","p","    ","q","    ","n","    ","Iext"
   write(1,*)0, 0.0_dp,p,q,n,0.0

   do i = 1, num_time_steps
      pdot=dpdt(p,q,n,a,b,Iext(i))
      qdot=dqdt(p,q,c,d)
      ndot=dndt(p,n,r,s,p0)
      call one_step_euler(p,pdot,dt)
      call one_step_euler(q,qdot,dt)
      call one_step_euler(n,ndot,dt)
      write(1,*)i,i*dt,p,q,n,Iext(i)
   end do
   close(1)
end program HRmodel

subroutine one_step_euler(x, dxdt, dt)
   implicit none
   real(8):: x, dxdt, dt
   x = x + dxdt*dt
end subroutine one_step_euler

function dpdt(p, q, n, a, b, Iext)
   implicit none
   real(8):: p, q, n, a, b, Iext, dpdt
   dpdt = q - a*p*p*p + b*p*p - n + Iext
end function dpdt

function dqdt(p, q, c, d)
   implicit none
   real(8):: p, q, c, d, dqdt
   dqdt = c - d*p*p - q
end function dqdt

function dndt(p, n, r, s, p0)
   implicit none
   real(8):: p, n, r, s, p0, dndt
   dndt = r*(s*(p - p0) - n)
end function dndt
