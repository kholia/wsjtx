#ifndef COMMONS_H
#define COMMONS_H

#define NFFT 32768

extern "C" {

extern struct {                     //This is "common/datcom/..." in Fortran
  float d4[2*5760000];              //Raw I/Q data from Linrad
  float ss[322*NFFT];               //Half-symbol spectra at 0,45,90,135 deg pol
  float savg[NFFT];                 //Avg spectra at 0,45,90,135 deg pol
  double fcenter;                   //Center freq from Linrad (MHz)
  int nutc;                         //UTC as integer, HHMM
  float fselected;                  //Selected frequency for nagain decodes
  int mousedf;                      //User-selected DF
  int mousefqso;                    //User-selected QSO freq (kHz)
  int nagain;                       //1 ==> decode only at fQSO +/- Tol
  int ndepth;                       //How much hinted decoding to do?
  int ndiskdat;                     //1 ==> data read from *.iq file
  int neme;                         //Hinted decoding tries only for EME calls
  int newdat;                       //1 ==> new data, must do long FFT
  int nfa;                          //Low decode limit (kHz)
  int nfb;                          //High decode limit (kHz)
  int nfcal;                        //Frequency correction, for calibration (Hz)
  int nfshift;                      //Shift of displayed center freq (kHz)
  int mcall3;                       //1 ==> CALL3.TXT has been modified
  int ntimeout;                     //Max for timeouts in Messages and BandMap
  int ntol;                         //+/- decoding range around fQSO (Hz)
  int nxant;                        //1 ==> add 45 deg to measured pol angle
  int junk_1;
  int nfsample;                     //Input sample rate
  int nxpol;                        //1 if using xpol antennas, 0 otherwise
  int nmode;                        //nmode = 10*m_modeQ65 + m_modeJT65
  int ndop00;                       //EME Self Doppler
  int nsave;                        //Number of s3(64,63) spectra saved
  int max_drift;                    //Maximum Q65 drift: units symbol_rate/TxT
  int nhsym;                        //Number of available JT65 half-symbols
  char mycall[12];
  char mygrid[6];
  char hiscall[12];
  char hisgrid[6];
  char datetime[20];
  int junk1;                        //Used to test extent of copy to shared memory
  int junk2;
} datcom_;

extern struct {                     //This is "common/datcom/..." in Fortran
  float d4[2*5760000];              //Raw I/Q data from Linrad
  float ss[322*NFFT];               //Half-symbol spectra at 0,45,90,135 deg pol
  float savg[NFFT];                 //Avg spectra at 0,45,90,135 deg pol
  double fcenter;                   //Center freq from Linrad (MHz)
  int nutc;                         //UTC as integer, HHMM
  float fselected;                  //Selected frequency for nagain decodes
  int mousedf;                      //User-selected DF
  int mousefqso;                    //User-selected QSO freq (kHz)
  int nagain;                       //1 ==> decode only at fQSO +/- Tol
  int ndepth;                       //How much hinted decoding to do?
  int ndiskdat;                     //1 ==> data read from *.iq file
  int neme;                         //Hinted decoding tries only for EME calls
  int newdat;                       //1 ==> new data, must do long FFT
  int nfa;                          //Low decode limit (kHz)
  int nfb;                          //High decode limit (kHz)
  int nfcal;                        //Frequency correction, for calibration (Hz)
  int nfshift;                      //Shift of displayed center freq (kHz)
  int mcall3;                       //1 ==> CALL3.TXT has been modified
  int ntimeout;                     //Max for timeouts in Messages and BandMap
  int ntol;                         //+/- decoding range around fQSO (Hz)
  int nxant;                        //1 ==> add 45 deg to measured pol angle
  int junk_1;
  int nfsample;                     //Input sample rate
  int nxpol;                        //1 if using xpol antennas, 0 otherwise
  int nmode;                        //nmode = 10*m_modeQ65 + m_modeJT65
  int ndop00;                       //EME Self Doppler
  int nsave;                        //Number of s3(64,63) spectra saved
  int max_drift;                    //Maximum Q65 drift: units symbol_rate/TxT
  int nhsym;                        //Number of available JT65 half-symbols
  char mycall[12];
  char mygrid[6];
  char hiscall[12];
  char hisgrid[6];
  char datetime[20];
  int junk1;                        //Used to test extent of copy to shared memory
  int junk2;
} datcom2_;

extern struct {
  int ndecodes;          //These are flags for inter-process communication
  int ncand;             //between QMAP and WSJT-X
  int nQDecoderDone;     //1 for real-time decodes, 2 for data from disk
  int nWDecoderBusy;     //Set to 1 when WSJT-X decoder is busy
  int nWTransmitting;    //Set to 1 when WSJT-X is transmitting
  char result[50][60];   //Staging area for QMAP decodes
} decodes_;

}

#endif // COMMONS_H
