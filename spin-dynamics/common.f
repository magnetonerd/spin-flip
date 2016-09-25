      integer i,j,k,ii,jj,kk
      integer nx,ny,nz,nm,ns,nspin,norbit,npos,neighbor
      parameter(nx=2,ny=1,nz=1,nm=nx*ny*nz,ns=12)
      parameter(nspin=3*nm,norbit=6*nm,npos=9*nm)
      parameter(neighbor=2)
      integer mmm,n_stp
      doubleprecision polar(3),alpha,pi,orbit
      doubleprecision amplitude,w,delay,tau,t_bgn,t_end,t_stp
      common/laser/amplitude,w,polar(3),delay,tau,t_bgn,t_end,t_stp
      doubleprecision exchange,soc,hbar,hbar_i,start_time
      integer ispinfile,icontrol
      parameter(ispinfile=20)
      doubleprecision Dx,Dy,Dz,d,polang
      common/system/mmm(2,2),exchange,soc,Dx,Dy,Dz,d
      common/constant/hbar_i,n_stp,hbar,alpha,pi
      common/control/icontrol,start_time,polang,orbit
