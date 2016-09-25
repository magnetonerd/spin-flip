      subroutine fcn(x,y,f)
      implicit none
      include'common.f'

      doubleprecision y(ns),f(ns),total1,total2,total3,total4
      doubleprecision S(3),Sj(3),Sk(3),LS1(3),SS1(3),DSS(6),A(3)
      doubleprecision SS2(3),SS3(3),L1(3),L2(3),LS2(3),ANISS(6)
      doubleprecision x,t,amax,A0,E0,AS(3),spin_mag1,spin_mag2,Eff(3)
      integer max,heavy,orbit_mag1,orbit_mag2,B(3),C(3),G(3),H(6)

      do i=1,ns
         f(i)=0d0
      end do

      t=x-delay

      A0=-amplitude*dexp(-t**2/tau**2)!*dcos(w*t)

      A(1)=A0*polar(1)
      A(2)=A0*polar(2)
      A(3)=A0*polar(3)

!      if(t.lt.0)then
!         heavy=0
!      else
!         heavy=1
!      end if

      S(1)=y(1)
      Sj(1)=y(2)
      S(2)=y(3)
      Sj(2)=y(4)
      S(3)=y(5)
      Sj(3)=y(6)
      
      

!      if(Sj(3)/Sj(1).le.0.02d0)then
!         heavy=0
!      else
!         heavy=1
!      end if

!      write(*,*)heavy


      L1(1)=y(7)
      L1(2)=y(9)
      L1(3)=y(11)
      L2(1)=y(8)
      L2(2)=y(10)
      L2(3)=y(12)

      LS1(1)=L1(2)*S(3)-L1(3)*S(2)
      LS1(2)=L1(3)*S(1)-L1(1)*S(3)
      LS1(3)=L1(1)*S(2)-L1(2)*S(1)
      LS2(1)=L2(3)*Sj(2)-L2(2)*Sj(3)
      LS2(2)=L2(1)*Sj(3)-L2(3)*Sj(1)
      LS2(3)=L2(2)*Sj(1)-L2(1)*Sj(2)

      SS1(1)=S(2)*Sj(3)-S(3)*Sj(2)
      SS1(2)=S(3)*Sj(1)-S(1)*Sj(3)
      SS1(3)=S(1)*Sj(2)-S(2)*Sj(1)
      SS2(1)=Sj(3)*S(2)-Sj(2)*S(3)
      SS2(2)=Sj(1)*S(3)-Sj(3)*S(1)
      SS2(3)=Sj(2)*S(1)-Sj(1)*S(2) 

      do i=1,nm
         f(i)=soc*LS1(1)
         f(i+2)=soc*LS1(2)
         f(i+4)=soc*LS1(3)
      end do
      
      do i=1,nm
         f(i)=f(i)+soc*LS2(1)
         f(i+2)=f(i+2)+soc*LS2(2)
         f(i+4)=f(i+4)+soc*LS2(3)
      end do

      do i=1,nm
         f(i)=exchange*SS1(1)
         f(i+2)=exchange*SS1(2)
         f(i+4)=exchange*SS1(3)
      end do

      do i=1,nm
         f(i)=f(i)+exchange*SS2(1)
         f(i+2)=f(i+2)+exchange*SS2(2)
         f(i+4)=f(i+4)+exchange*SS2(3)
      end do


      DSS(1)=S(1)*Sj(2)-S(2)*Sj(1)
      DSS(2)=S(1)*Sj(3)-S(3)*Sj(1)
      DSS(3)=S(2)*Sj(1)-S(1)*Sj(2)
      DSS(4)=S(2)*Sj(3)-S(3)*Sj(2)
      DSS(5)=S(3)*Sj(1)-S(1)*Sj(3)
      DSS(6)=S(3)*Sj(2)-S(2)*Sj(3)

!      do i=1,nm
!         f(i)=f(i)+Dy*DSS(1)+Dz*DSS(2)
!         f(i+2)=f(i+2)+Dx*DSS(3)+Dz*DSS(4)
!         f(i+4)=f(i+4)+Dx*DSS(5)+Dy*DSS(6)
!      end do

      ANISS(1)=S(2)*S(3)+S(3)*S(2)
      ANISS(2)=-S(1)*S(3)-S(3)*S(1)
      ANISS(3)=0d0
      ANISS(4)=Sj(2)*Sj(3)+Sj(3)*Sj(2)
      ANISS(5)=-Sj(1)*Sj(3)-Sj(3)*Sj(1)
      ANISS(6)=0d0

      do i=1,nm
         f(i)=(f(i)+d*ANISS(1)+d*ANISS(4))!*heavy
         f(i+2)=(f(i+2)+d*ANISS(2)+d*ANISS(5))!*heavy
         f(i+4)=(f(i+4)+d*ANISS(3)+d*ANISS(6))!*heavy
      end do

      AS(1)=A(2)*L1(3)-A(3)*L1(2)
      AS(2)=A(3)*L1(1)-A(1)*L1(3)
      AS(3)=A(1)*L1(2)-A(2)*L1(1)

      Eff(1)=soc*A(1)*L1(1)
      Eff(2)=soc*A(2)*L1(2)
      Eff(3)=soc*A(3)*L1(3)

      do i=1,nm
         f(i)=f(i)+Eff(1)!AS(1)
         f(i+2)=f(i+2)+Eff(2)!AS(2)
         f(i+4)=f(i+4)+Eff(3)!AS(3)
      end do

      do  i=1,nm
         f(i)=hbar_i*f(i)
         f(i+2)=hbar_i*f(i+2)
         f(i+4)=hbar_i*f(i+4)
      end do

 11   format(f12.6,1x,20(d16.10,1x))

      return

      end
