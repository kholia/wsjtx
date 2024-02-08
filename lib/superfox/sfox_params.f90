! Our code is RS(127,51).  We puncture two symbols to give (125,49)
  parameter (NN=127)              !Channel symbols, before puncture
  parameter (KK=51)               !Information symbols, before puncture
  parameter (ND1=25)              !Data symbols before sync 
  parameter (ND2=100)             !Data symbols after sync 
  parameter (ND=ND1+ND2)          !Total data symbols (125)
  parameter (NS=24)               !Sync symbols (for length)
  parameter (NSPS=1024)           !Samples per symbol at 12000 S/s
  parameter (NSYNC=NS*NSPS)       !Samples in sync waveform (24,576)
  parameter (NZ=NSPS*(ND+NS))     !Samples in full Tx waveform (151,552)
  parameter (NMAX=15*12000)       !Samples in iwave (180,000)
  parameter (NFFT=32768)          !Length of FFT for sync waveform
  parameter (NFFT1=2*NSPS)        !Length of FFTs for symbol spectra
  parameter (MM=7)                !Bits per symbol
  parameter (NQ=2**MM)            !Q, number of MFSK tones
  parameter (NFZ=3)               !First zero
