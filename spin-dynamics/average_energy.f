      implicit none
      
      double precision h,k,v,T(1000000)
      double precision u,B,pi,e0,hv,Eavg(1000000)
      integer i,j

      h=6.626d-34
      pi=acos(-1d0)
      e0=pi*4d-7
      k=1/(4*pi*e0)
      hv=2.2*(1.602d-19)
      v=hv/h

      T(1) = 1d0

      do i=2,1000000
         T(i)=T(1)*i
      end do

      open(1,file='EversesT')

      do j=1,1000000
         Eavg(j)=hv/(exp(hv/(k*T(j)))-1)
         write(1,*)T(j),Eavg(j)
      end do

      close(1)

      end
      
