      implicit none
      include'common.f'

      doubleprecision h,tol,hw,period
      doubleprecision x
      doubleprecision t,A0,A
      doubleprecision echarge
      doubleprecision y(ns),work(23+21*ns)
      external fcn,output,periodfind,chebfit
      integer ir,ifail
      parameter(h=6.62606957d0,echarge=1.60217653d0)
      parameter(tol=5d-14,ir=0)
      pi=dacos(-1d0)
      hbar=h/(pi*2d0)/echarge
      hbar_i=1d0/hbar

!      write(*,*)'Photon Energy?'
!      read(*,*)hw

      hw=2d0
      w=hw/hbar
      period=2d0*pi/w

!      write(*,*)'Amplitude?'
!      read(*,*)amplitude

      amplitude=0.05d0

!      write(*,*)'soc?'
!      read(*,*)soc

      tau=60d0
      delay=0d0
      t_bgn=-200*period
      t_end=200*period
      t_stp=period/6d0
      n_stp=(t_end-t_bgn)/t_stp
     
      start_time=t_bgn
      icontrol=0
      d=0.5d0
      Dx=0.0d0
      Dy=0.0d0
      Dz=0.007d0
      soc=0.38d0
      exchange=-1.0d0


!      write(*,*)'Polarization angle?'
!      read(*,*)polang

      polar(1)=dcos(pi/4d0)
      polar(2)=0d0
      polar(3)=dsin(pi/4d0)

      write(*,*)'To Advance press [Enter]'
      read(*,*)

!      write(*,*)'Exchange value?'
!      read(*,*)exchange

      do i = 1, ns
         y(i) = 0d0
      end do
      
      
      y(1)=0.01d0
      y(2)=0.01d0
      y(3)=0.02d0
      y(4)=0.07d0
      y(5)=0.6d0
      y(6)=0.6d0

      y(7)=0.2d0
      y(8)=0.2d0
      y(9)=0.3d0
      y(10)=0.3d0
      y(11)=0.5d0
      y(12)=0.5d0
      
 9999 continue
      
!      open(12,file='angfreq2',position='append')
!      
!      write(12,*)w,(y(3)+y(6))/2
!
!      close(12)


!      open(ispinfile,file='spin')
!      open(30,file='z-spin')
!      open(15,file='ang',position='append')
!      open(45,file='spincomp')
!      open(31,file='orbit')

      call d02cbf(t_bgn,t_end,ns,y,tol,ir,fcn,output,work,ifail)
      
      if(ifail.ne.0)then
         write(*,*)'IFAIL',ifail
         stop'*****FAILURE*****'
      end if
      
      end
      
