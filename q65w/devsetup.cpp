#include "devsetup.h"
#include "mainwindow.h"
#include <QTextStream>
#include <QDebug>
#include <cstdio>
#include <portaudio.h>

#define MAXDEVICES 200

//----------------------------------------------------------- DevSetup()
DevSetup::DevSetup(QWidget *parent) :	QDialog(parent)
{
  ui.setupUi(this);	//setup the dialog form
  m_restartSoundIn=false;
}

DevSetup::~DevSetup()
{
}

void DevSetup::initDlg()
{
  int k,id;
  int valid_devices=0;
  int minChan[MAXDEVICES];
  int maxChan[MAXDEVICES];
  int minSpeed[MAXDEVICES];
  int maxSpeed[MAXDEVICES];
  char hostAPI_DeviceName[MAXDEVICES][50];
  char s[256];
  int numDevices=Pa_GetDeviceCount();
  getDev(&numDevices,hostAPI_DeviceName,minChan,maxChan,minSpeed,maxSpeed);
  k=0;
  for(id=0; id<numDevices; id++)  {
    if(96000 >= minSpeed[id] && 96000 <= maxSpeed[id]) {
      m_inDevList[k]=id;
      k++;
      sprintf(s,"%2d   %d  %-49s",id,maxChan[id],hostAPI_DeviceName[id]);
      QString t(s);
      ui.comboBoxSndIn->addItem(t);
      valid_devices++;
    }
  }

  const PaDeviceInfo *pdi;
  int nchout;
  char *p,*p1;
  char p2[256];
  char pa_device_name[128];
  char pa_device_hostapi[128];

  k=0;
  for(id=0; id<numDevices; id++ )  {
    pdi=Pa_GetDeviceInfo(id);
    nchout=pdi->maxOutputChannels;
    if(nchout>=2) {
      m_outDevList[k]=id;
      k++;
      sprintf((char*)(pa_device_name),"%s",pdi->name);
      sprintf((char*)(pa_device_hostapi),"%s",
              Pa_GetHostApiInfo(pdi->hostApi)->name);

      p1=(char*)"";
      p=strstr(pa_device_hostapi,"MME");
      if(p!=NULL) p1=(char*)"MME";
      p=strstr(pa_device_hostapi,"Direct");
      if(p!=NULL) p1=(char*)"DirectX";
      p=strstr(pa_device_hostapi,"WASAPI");
      if(p!=NULL) p1=(char*)"WASAPI";
      p=strstr(pa_device_hostapi,"ASIO");
      if(p!=NULL) p1=(char*)"ASIO";
      p=strstr(pa_device_hostapi,"WDM-KS");
      if(p!=NULL) p1=(char*)"WDM-KS";

      sprintf(p2,"%2d   %-8s  %-39s",id,p1,pa_device_name);
      QString t(p2);
    }
  }

  ui.myCallEntry->setText(m_myCall);
  ui.myGridEntry->setText(m_myGrid);
  ui.astroFont->setValue(m_astroFont);
  ui.saveDirEntry->setText(m_saveDir);
  ui.azelDirEntry->setText(m_azelDir);
  ui.fCalSpinBox->setValue(m_fCal);
  ui.faddEntry->setText(QString::number(m_fAdd,'f',3));
  ui.networkRadioButton->setChecked(m_network);
  ui.soundCardRadioButton->setChecked(!m_network);
  ui.comboBoxSndIn->setEnabled(!m_network);
  ui.comboBoxSndIn->setCurrentIndex(m_nDevIn);
  ui.sbPort->setValue(m_udpPort);
  ui.cbIQswap->setChecked(m_IQswap);
  ui.sb_dB->setValue(m_dB);

  m_paInDevice=m_inDevList[m_nDevIn];
  m_paOutDevice=m_outDevList[m_nDevOut];

}

//------------------------------------------------------- accept()
void DevSetup::accept()
{
  // Called when OK button is clicked.
  // Check to see whether SoundInThread must be restarted,
  // and save user parameters.

  if(m_network!=ui.networkRadioButton->isChecked() or
     m_nDevIn!=ui.comboBoxSndIn->currentIndex() or
     m_paInDevice!=m_inDevList[m_nDevIn] or
     m_udpPort!=ui.sbPort->value()) m_restartSoundIn=true;

  m_myCall=ui.myCallEntry->text();
  m_myGrid=ui.myGridEntry->text();
  m_astroFont=ui.astroFont->value();
  m_saveDir=ui.saveDirEntry->text();
  m_azelDir=ui.azelDirEntry->text();
  m_fCal=ui.fCalSpinBox->value();
  m_fAdd=ui.faddEntry->text().toDouble();
  m_network=ui.networkRadioButton->isChecked();
  m_nDevIn=ui.comboBoxSndIn->currentIndex();
  m_paInDevice=m_inDevList[m_nDevIn];
  m_paOutDevice=m_outDevList[m_nDevOut];
  m_udpPort=ui.sbPort->value();
  m_IQswap=ui.cbIQswap->isChecked();
  m_dB=ui.sb_dB->value();
  QDialog::accept();
}

void DevSetup::on_soundCardRadioButton_toggled(bool checked)
{
  ui.comboBoxSndIn->setEnabled(ui.soundCardRadioButton->isChecked());
  ui.label_InputDev->setEnabled(checked);
  ui.label_Port->setEnabled(!checked);
  ui.sbPort->setEnabled(!checked);
  ui.cbIQswap->setEnabled(checked);
  ui.sb_dB->setEnabled(checked);
}
