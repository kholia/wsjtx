subroutine decode0(dd,ss,savg)

  use timer_module, only: timer
  parameter (NSMAX=60*96000)

  real*4 dd(4,NSMAX),ss(322,NFFT),savg(NFFT)
  real*8 fcenter
  integer hist(0:32768)
  logical ldecoded
  character mycall*12,hiscall*12,mygrid*6,hisgrid*6,datetime*20
  character mycall0*12,hiscall0*12,hisgrid0*6
  character*60 result
  common/decodes/ndecodes,ncand,result(50)
  common/npar/fcenter,nutc,idphi,mousedf,mousefqso,nagain,                &
       ndepth,ndiskdat,neme,newdat,nfa,nfb,nfcal,nfshift,                 &
       mcall3,nkeep,ntol,nxant,nrxlog,nfsample,nxpol,nmode,               &
       ndop00,nsave,max_drift,nhsym,mycall,mygrid,hiscall,hisgrid,datetime
  common/early/nhsym1,nhsym2,ldecoded(32768)
  data neme0/-99/
  save

  call sec0(0,tquick)
  if(newdat.ne.0) then
     nz=96000*nhsym/5.3833
     hist=0
     do i=1,nz
        j1=min(abs(dd(1,i)),32768.0)
        hist(j1)=hist(j1)+1
        j2=min(abs(dd(2,i)),32768.0)
        hist(j2)=hist(j2)+1
        j3=min(abs(dd(3,i)),32768.0)
        hist(j3)=hist(j3)+1
        j4=min(abs(dd(4,i)),32768.0)
        hist(j4)=hist(j4)+1
     enddo
     m=0
     do i=0,32768
        m=m+hist(i)
        if(m.ge.2*nz) go to 10
     enddo
10   rmsdd=1.5*i
  endif
      
  mycall0=mycall
  hiscall0=hiscall
  hisgrid0=hisgrid
  neme0=neme

  call timer('q65wa   ',0)
  call q65wa(dd,ss,savg,newdat,nutc,fcenter,ntol,nfa,nfb,           &
       mousedf,mousefqso,nagain,ndecdone,nfshift,max_drift,          &
       nfcal,nsum,nxant,mycall,mygrid,                 &
       hiscall,hisgrid,nhsym,nfsample,             &
       ndiskdat,nxpol,nmode,ndop00)
  call timer('q65wa   ',1)
  flush(6)

  return
end subroutine decode0
