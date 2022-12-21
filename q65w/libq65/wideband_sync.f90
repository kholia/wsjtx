module wideband_sync

  type candidate
     real :: snr          !Relative S/N of sync detection
     real :: f            !Freq of sync tone, 0 to 96000 Hz
     real :: xdt          !DT of matching sync pattern, -1.0 to +4.0 s
     real :: pol          !Polarization angle, degrees
     integer :: ipol      !Polarization angle, 1 to 4 ==> 0, 45, 90, 135 deg
     integer :: iflip     !Sync type: JT65 = +/- 1, Q65 = 0
     integer :: indx
  end type candidate
  type sync_dat
     real :: ccfmax
     real :: xdt
     real :: pol
     integer :: ipol
     integer :: iflip
     logical :: birdie
  end type sync_dat

  parameter (NFFT=32768)
  parameter (MAX_CANDIDATES=50)
  parameter (SNR1_THRESHOLD=4.5)
  type(sync_dat) :: sync(NFFT)
  integer nkhz_center

end module wideband_sync
