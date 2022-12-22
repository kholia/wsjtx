module wideband_sync

  type candidate
     real :: snr          !Relative S/N of sync detection
     real :: f            !Freq of sync tone, 0 to 96000 Hz
     real :: xdt          !DT of matching sync pattern, -1.0 to +4.0 s
  end type candidate

  parameter (NFFT=32768)
  parameter (MAX_CANDIDATES=50)
  parameter (SNR1_THRESHOLD=4.5)
  integer nkhz_center

end module wideband_sync
