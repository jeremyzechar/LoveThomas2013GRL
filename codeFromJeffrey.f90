      PROGRAM jeremy

      dimension data0(1000),data1(1000),data2(1000)    

      do i = 1,1000
        read(*,*,end=1111)idumb,rdumb,data0(i),rdumb,rdumb,rdumb
      end do 
      
 1111 continue
      nnn = i -1  

      nnn2 = nnn/2
      write(*,*)nnn2
      do i = 1,nnn2
        data1(i) = data0(i)
        data2(i) = data0(i+nnn2)
      end do
      
      knstrn = 0
      call chstwo(data1,data2,nnn2,knstrn,df,chsq,prob)  
          
      write(*,*)'nnn2 ',nnn2
      write(*,*)'x**2 ',chsq
      write(*,*)'prob ',prob

      
      END


      SUBROUTINE chstwo(bins1,bins2,nbins,knstrn,df,chsq,prob)
      INTEGER knstrn,nbins
      REAL chsq,df,prob,bins1(nbins),bins2(nbins)
      INTEGER j
      REAL gammq
      df=nbins-knstrn
      chsq=0.
      do 11 j=1,nbins
        if(bins1(j).eq.0..and.bins2(j).eq.0.)then
          df=df-1.
        else
          chsq=chsq+(bins1(j)-bins2(j))**2/(bins1(j)+bins2(j))
        endif
11    continue
      prob=gammq(0.5*df,0.5*chsq)
      return
      END

      FUNCTION gammq(a,x)
      REAL a,gammq,x
      REAL gammcf,gamser,gln
      if(x.lt.0..or.a.le.0.)then
        write(*,*) 'bad arguments in gammq'
        stop
      end if
      if(x.lt.a+1.)then
        call gser(gamser,a,x,gln)
        gammq=1.-gamser
      else
        call gcf(gammcf,a,x,gln)
        gammq=gammcf
      endif
      return
      END

      SUBROUTINE gcf(gammcf,a,x,gln)
      INTEGER ITMAX
      REAL a,gammcf,gln,x,EPS,FPMIN
      PARAMETER (ITMAX=100,EPS=3.e-7,FPMIN=1.e-30)
      INTEGER i
      REAL an,b,c,d,del,h,gammln
      gln=gammln(a)
      b=x+1.-a
      c=1./FPMIN
      d=1./b
      h=d
      do 11 i=1,ITMAX
        an=-i*(i-a)
        b=b+2.
        d=an*d+b
        if(abs(d).lt.FPMIN)d=FPMIN
        c=b+an/c
        if(abs(c).lt.FPMIN)c=FPMIN
        d=1./d
        del=d*c
        h=h*del
        if(abs(del-1.).lt.EPS)goto 1
11    continue
      write(*,*) 'a too large, ITMAX too small in gcf'
      stop
1     gammcf=exp(-x+a*log(x)-gln)*h
      return
      END

      SUBROUTINE gser(gamser,a,x,gln)
      INTEGER ITMAX
      REAL a,gamser,gln,x,EPS
      PARAMETER (ITMAX=100,EPS=3.e-7)
      INTEGER n
      REAL ap,del,sum,gammln
      gln=gammln(a)
      if(x.le.0.)then
        if(x.lt.0.)then
          write(*,*) 'x < 0 in gser'
          stop
        end if
        gamser=0.
        return
      endif
      ap=a
      sum=1./a
      del=sum
      do 11 n=1,ITMAX
        ap=ap+1.
        del=del*x/ap
        sum=sum+del
        if(abs(del).lt.abs(sum)*EPS)goto 1
11    continue
      write(*,*) 'a too large, ITMAX too small in gser'
      stop
1     gamser=sum*exp(-x+a*log(x)-gln)
      return
      END

      FUNCTION gammln(xx)
      REAL gammln,xx
      INTEGER j
      DOUBLE PRECISION ser,stp,tmp,x,y,cof(6)
      SAVE cof,stp
      DATA cof,stp/76.18009172947146d0,-86.50532032941677d0,24.01409824083091d0, &
        -1.231739572450155d0,.1208650973866179d-2,-.5395239384953d-5,2.5066282746310005d0/
      x=xx
      y=x
      tmp=x+5.5d0
      tmp=(x+0.5d0)*log(tmp)-tmp
      ser=1.000000000190015d0
      do 11 j=1,6
        y=y+1.d0
        ser=ser+cof(j)/y
11    continue
      gammln=tmp+log(stp*ser/x)
      return
      END

