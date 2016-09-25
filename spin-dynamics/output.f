      subroutine output(xsol,y)
      implicit none
      include'common.f'
      
      double precision sx,sy,sz,lx,ly,lz
      double precision s1x,s1y,s1z,l1x,l1y,l1z
      double precision s2x,s2y,s2z,l2x,l2y,l2z
      double precision xsol,spin,value
      double precision spin1,spin2,orbit1,orbit2
      double precision y(ns)
      integer icount
      save icount

 1    continue

      if(icontrol.eq.0)then
         icontrol=1
         icount=1
      else
         icount=icount+1
      end if

      sx=0d0
      sy=0d0
      sz=0d0
      lx=0d0
      ly=0d0
      lz=0d0

      s1x=0d0
      s1y=0d0
      s1z=0d0
      l1x=0d0
      l1y=0d0
      l1z=0d0

      s2x=0d0
      s2y=0d0
      s2z=0d0
      l2x=0d0
      l2y=0d0
      l2z=0d0

      s1x=y(1)
      s2x=y(2)
      s1y=y(3)
      s2y=y(4)
      s1z=y(5)
      s2z=y(6)

      l1x=y(7)
      l2x=y(8)
      l1y=y(9)
      l2y=y(10)
      l1z=y(11)
      l2z=y(12)


!      do i=1,nm
!         sx=sx+y(i)
!         sy=sy+y(i+2)
!         sz=sz+y(i+4)
!      end do
!
!      do i=1,nm
!         lx=lx+y(i+6)
!         ly=ly+y(i+8)
!         lz=lz+y(i+10)
!      end do

!      spin=sqrt(sx**2+sy**2+sz**2)

      spin1=sqrt(s1x**2+s1y**2+s1z**2)
      spin2=sqrt(s2x**2+s2y**2+s2z**2)

!      orbit=sqrt(lx**2+ly**2+lz**2)
      
      orbit1=sqrt(l1x**2+l1y**2+l1z**2)
      orbit2=sqrt(l2x**2+l2y**2+l2z**2)

!      write(ispinfile,11)xsol,sx/2,sy/2,sz/2!,spin

!      write(45,11)s1x,s1y,s1z

!      write(30,11)sz/2
     
!      write(31,11)xsol,lx/2,ly/2,lz/2!,orbit

!      if(mod(icount,10).eq.0)then
!         write(*,11)xsol,sx/2,sy/2,sz/2,orbit,spin
!      end if

!      write(15,11)w,sz/2

! 5    if(y(icount).ge.y(icount-1))then
!         value=y(icount)
!      else
!         write(*,*)'NOT FINISHED YET!'
!      end if
!      write(*,*)value
!      goto 5

      write(983,*)xsol

      write(999,*)s1x
      write(998,*)s1y
      write(997,*)s1z
      write(996,*)s2x
      write(995,*)s2y
      write(994,*)s2z
      write(993,*)l1x
      write(992,*)l1y
      write(991,*)l1z
      write(990,*)l2x
      write(989,*)l2y
      write(988,*)l2z
      write(987,*)spin1
      write(986,*)spin2
      write(985,*)orbit1
      write(984,*)orbit2

      xsol=start_time+icount*t_stp

 11   format(f12.6,1x,20(d16.10,1x))

      end
