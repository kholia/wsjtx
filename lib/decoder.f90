subroutine multimode_decoder(ss,id2,params,nfsample)

!$ use omp_lib
  use prog_args
  use timer_module, only: timer
  use jt4_decode
  use jt65_decode
  use jt9_decode
  use ft8_decode
  use ft4_decode
  use fst4_decode
  use q65_decode

  include 'jt9com.f90'
  include 'timer_common.inc'

  type, extends(jt4_decoder) :: counting_jt4_decoder
     integer :: decoded
  end type counting_jt4_decoder

  type, extends(jt65_decoder) :: counting_jt65_decoder
     integer :: decoded
  end type counting_jt65_decoder

  type, extends(jt9_decoder) :: counting_jt9_decoder
     integer :: decoded
  end type counting_jt9_decoder

  type, extends(ft8_decoder) :: counting_ft8_decoder
     integer :: decoded
  end type counting_ft8_decoder

  type, extends(ft4_decoder) :: counting_ft4_decoder
     integer :: decoded
  end type counting_ft4_decoder

  type, extends(fst4_decoder) :: counting_fst4_decoder
     integer :: decoded
  end type counting_fst4_decoder

  type, extends(q65_decoder) :: counting_q65_decoder
     integer :: decoded
  end type counting_q65_decoder

  real ss(184,NSMAX)
  logical baddata,newdat65,newdat9,single_decode,bVHF,bad0,newdat,ex
  logical lprinthash22
  integer*2 id2(NTMAX*12000)
  integer nqf(20)
  type(params_block) :: params
  real*4 dd(NTMAX*12000)
  character(len=20) :: datetime
  character(len=12) :: mycall, hiscall
  character(len=6) :: mygrid, hisgrid
  character(len=256) :: cmnd
  character*60 line
  data ndec8/0/,ntr0/-1/
  save
  type(counting_jt4_decoder) :: my_jt4
  type(counting_jt65_decoder) :: my_jt65
  type(counting_jt9_decoder) :: my_jt9
  type(counting_ft8_decoder) :: my_ft8
  type(counting_ft4_decoder) :: my_ft4
  type(counting_fst4_decoder) :: my_fst4
  type(counting_q65_decoder) :: my_q65

  if(.not.params%newdat .and. params%ntr.gt.ntr0) go to 800
  ntr0=params%ntr
  rms=sqrt(dot_product(float(id2(1:180000)),                         &
       float(id2(1:180000)))/180000.0)
  if(rms.lt.0.5) go to 800

  !cast C character arrays to Fortran character strings
  datetime=transfer(params%datetime, datetime)
  mycall=transfer(params%mycall,mycall)
  hiscall=transfer(params%hiscall,hiscall)
  mygrid=transfer(params%mygrid,mygrid)
  hisgrid=transfer(params%hisgrid,hisgrid)

  ! initialize decode counts
  my_jt4%decoded = 0
  my_jt65%decoded = 0
  my_jt9%decoded = 0
  my_ft8%decoded = 0
  my_ft4%decoded = 0
  my_fst4%decoded = 0
  my_q65%decoded = 0
  
! For testing only: return Rx messages stored in a file as decodes
  inquire(file='rx_messages.txt',exist=ex)
  if(ex) then
     if(params%nzhsym.eq.41) then
        open(39,file='rx_messages.txt',status='old')
        do i=1,9999
           read(39,'(a60)',end=5) line
           if(line(1:1).eq.' ' .or. line(1:1).eq.'-') go to 800
           write(*,'(a)') trim(line)
        enddo
5       close(39)
     endif
     go to 800
  endif

  ncontest=iand(params%nexp_decode,7)
  single_decode=iand(params%nexp_decode,32).ne.0
  bVHF=iand(params%nexp_decode,64).ne.0
  if(mod(params%nranera,2).eq.0) ntrials=10**(params%nranera/2)
  if(mod(params%nranera,2).eq.1) ntrials=3*10**(params%nranera/2)
  if(params%nranera.eq.0) ntrials=0
  
  nfail=0
10 if (params%nagain) then
     open(13,file=trim(temp_dir)//'/decoded.txt',status='unknown',            &
          position='append',iostat=ios13)
  else
     open(13,file=trim(temp_dir)//'/decoded.txt',status='unknown',iostat=ios13)
  endif
  if(ios13.ne.0) then
     nfail=nfail+1
     if(nfail.le.3) then
        call sleep_msec(10)
        go to 10
     endif
  endif

  if(params%nmode.eq.8) then
! We're in FT8 mode
     if(ncontest.eq.6) then            !Fox=6, Hound=7
! Fox mode: initialize and open houndcallers.txt     
        inquire(file=trim(temp_dir)//'/houndcallers.txt',exist=ex)
        if(.not.ex) then
           c2fox='            '
           g2fox='    '
           nsnrfox=-99
           nfreqfox=-99
           n30z=0
           nwrap=0
           nfox=0
        endif
        open(19,file=trim(temp_dir)//'/houndcallers.txt',status='unknown')
     endif

     if(ncontest.eq.7 .and. params%b_superfox .and. params%b_even_seq) then
        if(params%nzhsym.lt.50) go to 800
! Call the superFox decoder
        open(47,file=trim(temp_dir)//'/fort.47',status='unknown',  &
             access='stream')
        write(47) params%yymmdd,params%nutc,id2(1:20),id2(1:180000)
        close(47)
        cmnd='"'//trim(exe_dir)//'/sfrx" "'//trim(temp_dir)//'/fort.47"'
        call execute_command_line(cmnd,exitstat=ierr)
        if(ierr.ne.0) print*,trim(cmnd),' failed'
     else
        call timer('decft8  ',0)
        newdat=params%newdat
        if(params%emedelay.ne.0.0) then
           id2(1:156000)=id2(24001:180000)  ! Drop the first 2 seconds of data
           id2(156001:180000)=0
        endif
        call my_ft8%decode(ft8_decoded,id2,params%nQSOProgress,params%nfqso, &
             params%nftx,newdat,params%nutc,params%nfa,params%nfb,           &
             params%nzhsym,params%ndepth,params%emedelay,ncontest,           &
             logical(params%nagain),logical(params%lft8apon),                &
             logical(params%lapcqonly),params%napwid,mycall,hiscall,         &
             params%ndiskdat)
        call timer('decft8  ',1)
     endif
     if(nfox.gt.0) then
        n30min=minval(n30fox(1:nfox))
        n30max=maxval(n30fox(1:nfox))
     endif
     j=0

     if(ncontest.eq.6) then
! Fox mode: save decoded Hound calls for possible selection by FoxOp
        rewind 19
        if(nfox.eq.0) then
           endfile 19
           rewind 19
        else
           do i=1,nfox
              n=n30fox(i)
              if(n30max-n30fox(i).le.4) then
                 j=j+1
                 c2fox(j)=c2fox(i)
                 g2fox(j)=g2fox(i)
                 nsnrfox(j)=nsnrfox(i)
                 nfreqfox(j)=nfreqfox(i)
                 n30fox(j)=n
                 m=n30max-n
                 if(len(trim(g2fox(j))).eq.4) then
                    call azdist(mygrid,g2fox(j)//'  ',0.d0,nAz,nEl,nDmiles, &
                         nDkm,nHotAz,nHotABetter)
                 else
                    nDkm=9999
                 endif
                 write(19,1004) c2fox(j),g2fox(j),nsnrfox(j),nfreqfox(j),nDkm,m
1004             format(a12,1x,a4,i5,i6,i7,i3)
              endif
           enddo
           nfox=j
           flush(19)
        endif
     endif
     go to 800
  endif

  if(params%nmode.eq.5) then
     call timer('decft4  ',0)
     call my_ft4%decode(ft4_decoded,id2,params%nQSOProgress,params%nfqso,    &
          params%nfa,params%nfb,params%ndepth,                               &
          logical(params%lapcqonly),ncontest,mycall,hiscall)
     call timer('decft4  ',1)
     go to 800
  endif

  if(params%nmode.eq.66) then        !NB: JT65 = 65, Q65 = 66.
! We're in Q65 mode
     open(17,file=trim(temp_dir)//'/red.dat',status='unknown')
     open(14,file=trim(temp_dir)//'/avemsg.txt',status='unknown')
     call timer('dec_q65 ',0)
     nqd=1
     call my_q65%decode(q65_decoded,id2,nqd,params%nutc,params%ntr,      &
          params%nsubmode,params%nfqso,params%ntol,params%ndepth,        &
          params%nfa,params%nfb,logical(params%nclearave),               &
          single_decode,logical(params%nagain),params%max_drift,         &
          logical(params%newdat),params%emedelay,mycall,hiscall,hisgrid, &
          params%nQSOProgress,ncontest,logical(params%lapcqonly),navg0,nqf)
     params%nclearave=.false.

     if(.not.params%nagain) then
! Go through identified candidates again, treating each as if it had been
! double-clicked on the waterfall.
        do k=1,20
           if(nqf(k).eq.0) exit
           if(params%nagain .and. abs(nqf(k)-params%nfqso).gt.params%ntol) cycle
           nqd=1
           navg0=0
           ntol=5
           call my_q65%decode(q65_decoded,id2,nqd,params%nutc,params%ntr,    &
                params%nsubmode,nqf(k),ntol,params%ndepth,                   &
                params%nfa,params%nfb,logical(params%nclearave),             &
                .true.,.true.,params%max_drift,                              &
                .false.,params%emedelay,mycall,hiscall,hisgrid,              &
                params%nQSOProgress,ncontest,logical(params%lapcqonly),      &
                navg0,nqf)
        enddo
     endif

     call timer('dec_q65 ',1)
     close(17)
     go to 800
  endif

  if(params%nmode.eq.240) then
! We're in FST4 mode
     ndepth=iand(params%ndepth,3)
     iwspr=0
     lprinthash22=.false.
     params%nsubmode=0
     call timer('dec_fst4',0)
     call my_fst4%decode(fst4_decoded,id2,params%nutc,                &
          params%nQSOProgress,params%nfa,params%nfb,                  &
          params%nfqso,ndepth,params%ntr,params%nexp_decode,          &
          params%ntol,params%emedelay,logical(params%nagain),         &
          logical(params%lapcqonly),mycall,hiscall,iwspr,lprinthash22)
     call timer('dec_fst4',1)
     go to 800
  endif

    if(params%nmode.eq.241 .or. params%nmode.eq.242) then
! We're in FST4W mode
     ndepth=iand(params%ndepth,3)
     iwspr=1
     lprinthash22=.false.
     if(params%nmode.eq.242) lprinthash22=.true. 
     call timer('dec_fst4',0)
     call my_fst4%decode(fst4_decoded,id2,params%nutc,                &
          params%nQSOProgress,params%nfa,params%nfb,                  &
          params%nfqso,ndepth,params%ntr,params%nexp_decode,          &
          params%ntol,params%emedelay,logical(params%nagain),         &
          logical(params%lapcqonly),mycall,hiscall,iwspr,lprinthash22)
     call timer('dec_fst4',1)
     go to 800
  endif

! Zap data at start that might come from T/R switching transient?
  nadd=100
  k=0
  bad0=.false.
  do i=1,240
     sq=0.
     do n=1,nadd
        k=k+1
        sq=sq + float(id2(k))**2
     enddo
     rms=sqrt(sq/nadd)
     if(rms.gt.10000.0) then
        bad0=.true.
        kbad=k
        rmsbad=rms
     endif
  enddo
  if(bad0) then
     nz=min(NTMAX*12000,kbad+100)
!     id2(1:nz)=0                ! temporarily disabled as it can breaak the JT9 decoder, maybe others
  endif
  
  if(params%nmode.eq.4 .or. params%nmode.eq.65) open(14,file=trim(temp_dir)// &
       '/avemsg.txt',status='unknown')

  if(params%nmode.eq.4) then
     jz=52*nfsample
     if(params%newdat) then
        if(nfsample.eq.12000) call wav11(id2,jz,dd)
        if(nfsample.eq.11025) dd(1:jz)=id2(1:jz)
     else
        jz=52*11025
     endif
     call my_jt4%decode(jt4_decoded,dd,jz,params%nutc,params%nfqso,         &
          params%ntol,params%emedelay,params%dttol,logical(params%nagain),  &
          params%ndepth,logical(params%nclearave),params%minsync,           &
          params%minw,params%nsubmode,mycall,hiscall,         &
          hisgrid,params%nlist,params%listutc,jt4_average)
     go to 800
  endif

  npts65=52*12000
  if(baddata(id2,npts65)) then
     nsynced=0
     ndecoded=0
     go to 800
  endif
 
  ntol65=params%ntol              !### is this OK? ###
  newdat65=params%newdat
  newdat9=params%newdat

!$call omp_set_dynamic(.true.)
!$omp parallel sections num_threads(2) copyin(/timer_private/) shared(ndecoded) if(.true.) !iif() needed on Mac

!$omp section
  if(params%nmode.eq.65) then
! We're in JT65 mode

     if(newdat65) dd(1:npts65)=id2(1:npts65)
     nf1=params%nfa
     nf2=params%nfb
     call timer('jt65a   ',0)
     call my_jt65%decode(jt65_decoded,dd,npts65,newdat65,params%nutc,      &
          nf1,nf2,params%nfqso,ntol65,params%nsubmode,params%minsync,      &
          logical(params%nagain),params%n2pass,logical(params%nrobust),    &
          ntrials,params%naggressive,params%ndepth,params%emedelay,        &
          logical(params%nclearave),mycall,hiscall,          &
          hisgrid,params%nexp_decode,params%nQSOProgress,           &
          logical(params%ljt65apon))
     call timer('jt65a   ',1)

  else if(params%nmode.eq.9 .or. (params%nmode.eq.(65+9) .and. params%ntxmode.eq.9)) then
! We're in JT9 mode, or should do JT9 first
     call timer('decjt9  ',0)
     call my_jt9%decode(jt9_decoded,ss,id2,params%nfqso,       &
          newdat9,params%npts8,params%nfa,params%nfsplit,params%nfb,       &
          params%ntol,params%nzhsym,logical(params%nagain),params%ndepth,  &
          params%nmode,params%nsubmode,params%nexp_decode)
     call timer('decjt9  ',1)
  endif

!$omp section
  if(params%nmode.eq.(65+9)) then       !Do the other mode (we're in dual mode)
     if (params%ntxmode.eq.9) then
        if(newdat65) dd(1:npts65)=id2(1:npts65)
        nf1=params%nfa
        nf2=params%nfb
        call timer('jt65a   ',0)
        call my_jt65%decode(jt65_decoded,dd,npts65,newdat65,params%nutc,   &
             nf1,nf2,params%nfqso,ntol65,params%nsubmode,params%minsync,   &
             logical(params%nagain),params%n2pass,logical(params%nrobust), &
             ntrials,params%naggressive,params%ndepth,params%emedelay,     &
             logical(params%nclearave),mycall,hiscall,       &
             hisgrid,params%nexp_decode,params%nQSOProgress,        &
             logical(params%ljt65apon))
        call timer('jt65a   ',1)
     else
        call timer('decjt9  ',0)
        call my_jt9%decode(jt9_decoded,ss,id2,params%nfqso,                &
             newdat9,params%npts8,params%nfa,params%nfsplit,params%nfb,    &
             params%ntol,params%nzhsym,logical(params%nagain),             &
             params%ndepth,params%nmode,params%nsubmode,params%nexp_decode)
        call timer('decjt9  ',1)
     end if
  endif

!$omp end parallel sections

! JT65 is not yet producing info for nsynced, ndecoded.
800 ndecoded = my_jt4%decoded + my_jt65%decoded + my_jt9%decoded +       &
         my_ft8%decoded + my_ft4%decoded + my_fst4%decoded +             &
         my_q65%decoded
  if(params%nmode.eq.8 .and. params%nzhsym.eq.41) ndec41=ndecoded
  if(params%nmode.eq.8 .and. params%nzhsym.eq.47) ndec47=ndecoded
  if(params%nmode.eq.8 .and. params%nzhsym.eq.50) then
     ndecoded=ndec41+ndec47+ndecoded
  endif
  if(params%nmode.ne.8 .or. params%nzhsym.eq.50 .or.                     &
       .not.params%ndiskdat) then

     write(*,1010) nsynced,ndecoded,navg0
1010 format('<DecodeFinished>',2i4,i9)
     call flush(6)
  endif
  close(13)
  if(ncontest.eq.6) close(19)
  if(params%nmode.eq.4 .or. params%nmode.eq.65 .or. params%nmode.eq.66) close(14)
  return
contains

  subroutine jt4_decoded(this,snr,dt,freq,have_sync,sync,is_deep,    &
       decoded0,qual,ich,is_average,ave)
    implicit none
    class(jt4_decoder), intent(inout) :: this
    integer, intent(in) :: snr
    real, intent(in) :: dt
    integer, intent(in) :: freq
    logical, intent(in) :: have_sync
    logical, intent(in) :: is_deep
    character(len=1), intent(in) :: sync
    character(len=22), intent(in) :: decoded0
    real, intent(in) :: qual
    integer, intent(in) :: ich
    logical, intent(in) :: is_average
    integer, intent(in) :: ave

    character*22 decoded
    character*3 cflags

    if(ich.eq.-99) stop                         !Silence compiler warning
    if (have_sync) then
       decoded=decoded0
       cflags='   '
       if(decoded.ne.'                      ') then
          cflags='f  '
          if(is_deep) then
             cflags='d  '
             write(cflags(2:2),'(i1)') min(int(qual),9)
             if(qual.ge.10.0) cflags(2:2)='*'
             if(qual.lt.3.0) decoded(22:22)='?'
          endif
          if(is_average) then
             write(cflags(3:3),'(i1)') min(ave,9)
             if(ave.ge.10) cflags(3:3)='*'
             if(cflags(1:1).eq.'f') cflags=cflags(1:1)//cflags(3:3)//' '
          endif
       endif
       write(*,1000) params%nutc,snr,dt,freq,sync,decoded,cflags
1000   format(i4.4,i4,f5.1,i5,1x,'$',a1,1x,a22,1x,a3)
    else
       write(*,1000) params%nutc,snr,dt,freq
    end if

    select type(this)
    type is (counting_jt4_decoder)
       this%decoded = this%decoded + 1
    end select
  end subroutine jt4_decoded

  subroutine jt4_average (this, used, utc, sync, dt, freq, flip)
    implicit none
    class(jt4_decoder), intent(inout) :: this
    logical, intent(in) :: used
    integer, intent(in) :: utc
    real, intent(in) :: sync
    real, intent(in) :: dt
    integer, intent(in) :: freq
    logical, intent(in) :: flip
    character(len=1) :: cused, csync

    cused = '.'
    csync = '*'
    if (used) cused = '$'
    if (flip) csync = '$'
    write(14,1000) cused,utc,sync,dt,freq,csync
1000 format(a1,i5.4,f6.1,f6.2,i6,1x,a1)
  end subroutine jt4_average

  subroutine jt65_decoded(this,sync,snr,dt,freq,drift,nflip,width,     &
       decoded0,ft,qual,nsmo,nsum,minsync)

    use jt65_decode
    implicit none

    class(jt65_decoder), intent(inout) :: this
    real, intent(in) :: sync
    integer, intent(in) :: snr
    real, intent(in) :: dt
    integer, intent(in) :: freq
    integer, intent(in) :: drift
    integer, intent(in) :: nflip
    real, intent(in) :: width
    character(len=22), intent(in) :: decoded0
    integer, intent(in) :: ft
    integer, intent(in) :: qual
    integer, intent(in) :: nsmo
    integer, intent(in) :: nsum
    integer, intent(in) :: minsync

    integer i,nap
    logical is_deep,is_average
    character decoded*22,csync*2,cflags*3

    if(width.eq.-9999.0) stop              !Silence compiler warning
!$omp critical(decode_results)
    decoded=decoded0
    cflags='   '
    is_deep=ft.eq.2

    if(ft.eq.0 .and. minsync.ge.0 .and. int(sync).lt.minsync) then
       write(*,1010) params%nutc,snr,dt,freq
    else
       is_average=nsum.ge.2
       if(bVHF .and. ft.gt.0) then
          cflags='f  '
          if(is_deep) then
             cflags='d  '
             write(cflags(2:2),'(i1)') min(qual,9)
             if(qual.ge.10) cflags(2:2)='*'
             if(qual.lt.3) decoded(22:22)='?'
          endif
          if(is_average) then
             write(cflags(3:3),'(i1)') min(nsum,9)
             if(nsum.ge.10) cflags(3:3)='*'
          endif
          nap=ishft(ft,-2)
          if(nap.ne.0) then
             if(nsum.lt.2) write(cflags(1:3),'(a1,i1," ")') 'a',nap
             if(nsum.ge.2) write(cflags(1:3),'(a1,2i1)') 'a',nap,min(nsum,9)
          endif
       endif
       csync='# '
       i=0
       if(bVHF .and. nflip.ne.0 .and.                         &
            sync.ge.max(0.0,float(minsync))) then
          csync='#*'
          if(nflip.eq.-1) then
             csync='##'
             if(decoded.ne.'                      ') then
                do i=22,1,-1
                   if(decoded(i:i).ne.' ') exit
                enddo
                if(i.gt.18) i=18
                decoded(i+2:i+4)='OOO'
             endif
          endif
       endif
       n=len(trim(decoded))
       if(n.eq.2 .or. n.eq.3) csync='# '
       if(cflags(1:1).eq.'f') then
          cflags(2:2)=cflags(3:3)
          cflags(3:3)=' '
       endif
       write(*,1010) params%nutc,snr,dt,freq,csync,decoded,cflags
1010   format(i4.4,i4,f5.1,i5,1x,a2,1x,a22,1x,a3)
    endif
    if(ios13.eq.0) write(13,1012) params%nutc,nint(sync),snr,dt,    &
         float(freq),drift,decoded,ft,nsum,nsmo
1012 format(i4.4,i4,i5,f6.2,f8.0,i4,3x,a22,' JT65',3i3)
    call flush(6)

!$omp end critical(decode_results)
    select type(this)
    type is (counting_jt65_decoder)
       this%decoded = this%decoded + 1
    end select
  end subroutine jt65_decoded

  subroutine jt9_decoded (this, sync, snr, dt, freq, drift, decoded)
    use jt9_decode
    implicit none

    class(jt9_decoder), intent(inout) :: this
    real, intent(in) :: sync
    integer, intent(in) :: snr
    real, intent(in) :: dt
    real, intent(in) :: freq
    integer, intent(in) :: drift
    character(len=22), intent(in) :: decoded

    !$omp critical(decode_results)
    write(*,1000) params%nutc,snr,dt,nint(freq),decoded
1000 format(i4.4,i4,f5.1,i5,1x,'@ ',1x,a22)
    if(ios13.eq.0) write(13,1002) params%nutc,nint(sync),snr,dt,freq,  &
         drift,decoded
1002 format(i4.4,i4,i5,f6.1,f8.0,i4,3x,a22,' JT9')
    call flush(6)
    !$omp end critical(decode_results)
    select type(this)
    type is (counting_jt9_decoder)
       this%decoded = this%decoded + 1
    end select
  end subroutine jt9_decoded

  subroutine ft8_decoded (this,sync,snr,dt,freq,decoded,nap,qual)
    use ft8_decode
    implicit none

    class(ft8_decoder), intent(inout) :: this
    real, intent(in) :: sync
    integer, intent(in) :: snr
    real, intent(in) :: dt
    real, intent(in) :: freq
    character(len=37), intent(in) :: decoded
    character c1*12,c2*12,g2*4,w*4
    integer i0,i1,i2,i3,i4,i5,n30,nwrap
    integer, intent(in) :: nap 
    real, intent(in) :: qual 
    character*2 annot
    character*37 decoded0
    logical isgrid4,first,b0,b1,b2
    data first/.true./
    save

    isgrid4(w)=(len_trim(w).eq.4 .and.                                        &
         ichar(w(1:1)).ge.ichar('A') .and. ichar(w(1:1)).le.ichar('R') .and.  &
         ichar(w(2:2)).ge.ichar('A') .and. ichar(w(2:2)).le.ichar('R') .and.  &
         ichar(w(3:3)).ge.ichar('0') .and. ichar(w(3:3)).le.ichar('9') .and.  &
         ichar(w(4:4)).ge.ichar('0') .and. ichar(w(4:4)).le.ichar('9'))

    if(first) then
       c2fox='            '
       g2fox='    '
       nsnrfox=-99
       nfreqfox=-99
       n30z=0
       nwrap=0
       nfox=0
       first=.false.
    endif
    
    decoded0=decoded

    annot='  ' 
    if(nap.ne.0) then
       write(annot,'(a1,i1)') 'a',nap
       if(qual.lt.0.17) decoded0(37:37)='?'
    endif

!    i0=index(decoded0,';')
! Always print 37 characters? Or, send i3,n3 up to here from ft8b_2 and use them
! to decide how many chars to print?
!TEMP
    i0=1
    if(i0.le.0) write(*,1000) params%nutc,snr,dt,nint(freq),decoded0(1:22),annot
1000 format(i6.6,i4,f5.1,i5,' ~ ',1x,a22,1x,a2)
    if(i0.gt.0) write(*,1001) params%nutc,snr,dt,nint(freq),decoded0,annot
1001 format(i6.6,i4,f5.1,i5,' ~ ',1x,a37,1x,a2)
    if(ios13.eq.0) write(13,1002) params%nutc,nint(sync),snr,dt,freq,0,decoded0
1002 format(i6.6,i4,i5,f6.1,f8.0,i4,3x,a37,' FT8')

    if(ncontest.eq.6) then
       i1=index(decoded0,' ')
       i2=i1 + index(decoded0(i1+1:),' ')
       i3=i2 + index(decoded0(i2+1:),' ')
       if(i1.ge.3 .and. i2.ge.7 .and. i3.ge.10) then
          c1=decoded0(1:i1-1)//'            '
          c2=decoded0(i1+1:i2-1)
          g2=decoded0(i2+1:i3-1)
          b0=c1.eq.mycall
          if(c1(1:3).eq.'DE ' .and. index(c2,'/').ge.2) b0=.true.
          if(len(trim(c1)).ne.len(trim(mycall))) then
             i4=index(trim(c1),trim(mycall))
             i5=index(trim(mycall),trim(c1))
             if(i4.ge.1 .or. i5.ge.1) b0=.true.
          endif
          b1=i3-i2.eq.5 .and. isgrid4(g2)
          b2=i3-i2.eq.1
          if(b0 .and. (b1.or.b2) .and. nint(freq).ge.1000) then
             n=params%nutc
             n30=(3600*(n/10000) + 60*mod((n/100),100) + mod(n,100))/30
             if(n30.lt.n30z) nwrap=nwrap+5760    !New UTC day, handle the wrap
             n30z=n30
             n30=n30+nwrap
             if(nfox.lt.MAXFOX) nfox=nfox+1
             c2fox(nfox)=c2
             g2fox(nfox)=g2
             nsnrfox(nfox)=snr
             nfreqfox(nfox)=nint(freq)
             n30fox(nfox)=n30
          endif
       endif
    endif
    
    call flush(6)
    if(ios13.eq.0) call flush(13)
    
    select type(this)
    type is (counting_ft8_decoder)
       this%decoded = this%decoded + 1
    end select

    return
  end subroutine ft8_decoded

  subroutine ft4_decoded (this,sync,snr,dt,freq,decoded,nap,qual)
    use ft4_decode
    implicit none

    class(ft4_decoder), intent(inout) :: this
    real, intent(in) :: sync
    integer, intent(in) :: snr
    real, intent(in) :: dt
    real, intent(in) :: freq
    character(len=37), intent(in) :: decoded
    integer, intent(in) :: nap
    real, intent(in) :: qual 
    character*2 annot
    character*37 decoded0
    
    decoded0=decoded

    annot='  ' 
    if(nap.ne.0) then
       write(annot,'(a1,i1)') 'a',nap
       if(qual.lt.0.17) decoded0(37:37)='?'
    endif

    write(*,1001) params%nutc,snr,dt,nint(freq),decoded0,annot
1001 format(i6.6,i4,f5.1,i5,' + ',1x,a37,1x,a2)

    if(ios13.eq.0) then
       write(13,1002,err=10) params%nutc,nint(sync),snr,dt,freq,0,decoded0
1002   format(i6.6,i4,i5,f6.1,f8.0,i4,3x,a37,' FT4')
       flush(13)
    endif
    
10  call flush(6)
    
    select type(this)
    type is (counting_ft4_decoder)
       this%decoded = this%decoded + 1
    end select

    return
  end subroutine ft4_decoded

  subroutine fst4_decoded (this,nutc,sync,nsnr,dt,freq,decoded,nap,   &
       qual,ntrperiod,fmid,w50)

    use fst4_decode
    implicit none

    class(fst4_decoder), intent(inout) :: this
    integer, intent(in) :: nutc
    real, intent(in) :: sync
    integer, intent(in) :: nsnr
    real, intent(in) :: dt
    real, intent(in) :: freq
    character(len=37), intent(in) :: decoded
    integer, intent(in) :: nap
    real, intent(in) :: qual
    integer, intent(in) :: ntrperiod
    real, intent(in) :: fmid
    real, intent(in) :: w50

    character*2 annot
    character*37 decoded0
    character*70 line

    decoded0=decoded
    annot='  '
    if(nap.ne.0) then
       write(annot,'(a1,i1)') 'a',nap
       if(qual.lt.0.17) decoded0(37:37)='?'
    endif

    if(ntrperiod.lt.60) then
       write(line,1001) nutc,nsnr,dt,nint(freq),decoded0,annot
1001   format(i6.6,i4,f5.1,i5,' ` ',1x,a37,1x,a2)
       if(ios13.eq.0) write(13,1002) nutc,nint(sync),nsnr,dt,freq,0,decoded0
1002   format(i6.6,i4,i5,f6.1,f8.0,i4,3x,a37,' FST4')
    else
       write(line,1003) nutc,nsnr,dt,nint(freq),decoded0,annot
1003   format(i4.4,i4,f5.1,i5,' ` ',1x,a37,1x,a2,2f7.3)
       if(ios13.eq.0) write(13,1004) nutc,nint(sync),nsnr,dt,freq,0,decoded0
1004   format(i4.4,i4,i5,f6.1,f8.0,i4,3x,a37,' FST4')
    endif

    if(fmid.ne.-999.0) then
       if(w50.lt.0.95) write(line(65:70),'(f6.3)') w50
       if(w50.ge.0.95) write(line(65:70),'(f6.2)') w50
    endif

    write(*,1005) line
1005 format(a70)

    call flush(6)
    if(ios13.eq.0) call flush(13)

    select type(this)
    type is (counting_fst4_decoder)
       this%decoded = this%decoded + 1
    end select

   return
 end subroutine fst4_decoded

 subroutine q65_decoded (this,nutc,snr1,nsnr,dt,freq,decoded,idec,   &
      nused,ntrperiod)

    use q65_decode
    implicit none

    class(q65_decoder), intent(inout) :: this
    integer, intent(in) :: nutc
    real, intent(in) :: snr1
    integer, intent(in) :: nsnr
    real, intent(in) :: dt
    real, intent(in) :: freq
    character(len=37), intent(in) :: decoded
    integer, intent(in) :: idec
    integer, intent(in) :: nused
    integer, intent(in) :: ntrperiod
    character*3 cflags
  
    cflags='   '
    if(idec.ge.0) then
       cflags='q  '
       write(cflags(2:2),'(i1)') idec
       if(nused.ge.2) write(cflags(3:3),'(i1)') nused
    endif

    if(ntrperiod.lt.60) then
       write(*,1001) nutc,nsnr,dt,nint(freq),decoded,cflags
1001   format(i6.6,i4,f5.1,i5,' : ',1x,a37,1x,a3)
    if(ios13.eq.0) write(13,1002) nutc,nint(snr1),nsnr,dt,freq,0,decoded
1002 format(i6.6,i4,i5,f6.1,f8.0,i4,3x,a37,' Q65')
    else
       write(*,1003) nutc,nsnr,dt,nint(freq),decoded,cflags
1003   format(i4.4,i4,f5.1,i5,' : ',1x,a37,1x,a3)
       if(ios13.eq.0) write(13,1004) nutc,nint(snr1),nsnr,dt,freq,0,decoded
1004   format(i4.4,i4,i5,f6.1,f8.0,i4,3x,a37,' Q65')

    endif
    call flush(6)
    if(ios13.eq.0) call flush(13)

    select type(this)
    type is (counting_q65_decoder)
       if(idec.ge.0) this%decoded = this%decoded + 1
    end select

   return
 end subroutine q65_decoded

end subroutine multimode_decoder
