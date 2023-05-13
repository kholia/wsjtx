#include "soundin.h"
#include <math.h>

#ifdef Q_OS_WIN32
#include <windows.h>
#else
#include <sys/socket.h>
#endif

#define NFFT 32768
#define FRAMES_PER_BUFFER 1024

extern "C"
{
  struct
  {
    double d8[60*96000];              //This is "common/datcom/..." in fortran
    float ss[322*NFFT];
    float savg[NFFT];
    double fcenter;
    int nutc;
    float fselected;                  //Selected frequency for nagain decodes
    int mousedf;                      //User-selected DF
    int mousefqso;                    //User-selected QSO freq (kHz)
    int nagain;                       //1 ==> decode only at fQSO +/- Tol
    int ndepth;                       //How much hinted decoding to do?
    int ndiskdat;                     //1 ==> data read from *.tf2 or *.iq file
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
    int junk_1;                       //Flags to control log files
    int nfsample;                     //Input sample rate
    int nxpol;                        //1 if using xpol antennas, 0 otherwise
    int mode65;                       //JT65 sub-mode: A=1, B=2, C=4
    int nfast;                        //1No longer used
    int nsave;                        //Number of s3(64,63) spectra saved
    char mycall[12];
    char mygrid[6];
    char hiscall[12];
    char hisgrid[6];
    char datetime[20];
  } datcom_;
}

namespace
{
  struct COMWrapper
  {
    explicit COMWrapper ()
    {
#ifdef Q_OS_WIN32
      // required because Qt only does this for GUI thread
      CoInitializeEx (nullptr, COINIT_APARTMENTTHREADED | COINIT_DISABLE_OLE1DDE);
#endif
    }
    ~COMWrapper ()
    {
#ifdef Q_OS_WIN32
      CoUninitialize ();
#endif
    }
  };
}

void SoundInThread::run()                           //SoundInThread::run()
{
  quitExecution = false;

  if (m_net) {
    inputUDP();
    return;
  }
}

void SoundInThread::setScale(qint32 n)
{
  m_dB=n;
}
void SoundInThread::setPort(int n)                              //setPort()
{
  if (isRunning()) return;
  this->m_udpPort=n;
}

void SoundInThread::setRate(double rate)                         //setRate()
{
  if (isRunning()) return;
  this->m_rate = rate;
}

void SoundInThread::setBufSize(unsigned n)                      //setBufSize()
{
  if (isRunning()) return;
  this->bufSize = n;
}

void SoundInThread::setFadd(double x)
{
  m_fAdd=x;
}


void SoundInThread::quit()                                       //quit()
{
  quitExecution = true;
}

void SoundInThread::setNetwork(bool b)                          //setNetwork()
{
  m_net = b;
}

void SoundInThread::setMonitoring(bool b)                    //setMonitoring()
{
  m_monitoring = b;
}

void SoundInThread::setNrx(int n)                              //setNrx()
{
  m_nrx = n;
}

int SoundInThread::nrx()
{
  return m_nrx;
}

int SoundInThread::mhsym()
{
  return m_hsym;
}

void SoundInThread::setPeriod(int n)
{
  m_TRperiod=n;
}

//--------------------------------------------------------------- inputUDP()
void SoundInThread::inputUDP()
{
  udpSocket = new QUdpSocket();
  if(!udpSocket->bind(m_udpPort,QUdpSocket::ShareAddress) )
  {
    emit error(tr("UDP Socket bind failed."));
    return;
  }

  // Set this socket's total buffer space for received UDP packets
  udpSocket->setSocketOption (QUdpSocket::ReceiveBufferSizeSocketOption, 141600);

  bool qe = quitExecution;
  struct linradBuffer {
    double cfreq;
    int msec;
    float userfreq;
    int iptr;
    quint16 iblk;
    qint8 nrx;
    char iusb;
    double d8[174];
  } b;

  int ntr0=99;
  int k=0;
  int nsec;
  int ntr;
  int nhsym0=0;
  int iz=174;

  // Main loop for input of UDP packets over the network:
  while (!qe) {
    qe = quitExecution;
    if (qe) break;
    if (!udpSocket->hasPendingDatagrams()) {
//      msleep(2);                  // Sleep if no packet available
      QObject().thread()->usleep(2000);
    } else {
      int nBytesRead = udpSocket->readDatagram((char *)&b,1416);
      if (nBytesRead != 1416) qDebug() << "UDP Read Error:" << nBytesRead;

      qint64 ms = QDateTime::currentMSecsSinceEpoch() % 86400000;
      nsec = ms/1000;             // Time according to this computer
      ntr = nsec % m_TRperiod;

// Reset buffer pointer and symbol number at start of minute
      if(ntr < ntr0 or !m_monitoring or m_TRperiod!=m_TRperiod0) {
        k=0;
        nhsym0=0;
        m_TRperiod0=m_TRperiod;
      }

      ntr0=ntr;

      if(m_monitoring) {
        m_nrx=b.nrx;
        if(m_nrx == +1) iz=348;                 //One RF channel, i*2 data
        if(m_nrx == -1 or m_nrx == +2) iz=174;  //One Rf channel, r*4 data
                                                // or 2 RF channels, i*2 data
        if(m_nrx == -2) iz=87;                  // Two RF channels, r*4 data

        // If buffer will not overflow, move data into datcom_
        if ((k+iz) <= 60*96000) {
          int nsam=-1;
          recvpkt_(&nsam, &b.iblk, &b.nrx, &k, b.d8, b.d8);
          datcom_.fcenter=b.cfreq + m_fAdd;
//          qDebug() << "bb" << b.cfreq << m_fAdd << datcom_.fcenter;
        }

        m_hsym=(k-2048)*11025.0/(2048.0*m_rate);
        if(m_hsym != nhsym0) {
          if(!m_dataSinkBusy) {
            m_dataSinkBusy=true;
            emit readyForFFT(k);         //Signal to compute new FFTs
          }
          nhsym0=m_hsym;
        }
      }
    }
  }
  delete udpSocket;
}
