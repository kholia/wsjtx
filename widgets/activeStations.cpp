#include "activeStations.h"

#include <QSettings>
#include <QApplication>
#include <QTextCharFormat>
#include <QDateTime>
#include <QDebug>

#include "SettingsGroup.hpp"
#include "qt_helpers.hpp"
#include "ui_activeStations.h"

#include "moc_activeStations.cpp"

ActiveStations::ActiveStations(QSettings * settings, QFont const& font, QWidget *parent) :
  QWidget(parent),
  settings_ {settings},
  ui(new Ui::ActiveStations)
{
  ui->setupUi(this);
  setWindowTitle (QApplication::applicationName () + " - " + tr ("Active Stations"));
  ui->RecentStationsPlainTextEdit->setReadOnly (true);
  changeFont (font);
  read_settings ();
  ui->header_label2->setText("  N   Call    Grid   Az  S/N  Freq Tx Age Pts");
  connect(ui->RecentStationsPlainTextEdit, SIGNAL(selectionChanged()), this, SLOT(select()));
  connect(ui->cbReadyOnly, SIGNAL(toggled(bool)), this, SLOT(on_cbReadyOnly_toggled(bool)));
}

ActiveStations::~ActiveStations()
{
  write_settings ();
}

void ActiveStations::changeFont (QFont const& font)
{
  ui->header_label2->setStyleSheet (font_as_stylesheet (font));
  ui->RecentStationsPlainTextEdit->setStyleSheet (font_as_stylesheet (font));
  updateGeometry ();
}

void ActiveStations::read_settings ()
{
  SettingsGroup group {settings_, "ActiveStations"};
  restoreGeometry (settings_->value ("window/geometry").toByteArray ());
  ui->sbMaxRecent->setValue(settings_->value("MaxRecent",10).toInt());
  ui->sbMaxAge->setValue(settings_->value("MaxAge",10).toInt());
  ui->cbReadyOnly->setChecked(settings_->value("ReadyOnly",false).toBool());
}

void ActiveStations::write_settings ()
{
  SettingsGroup group {settings_, "ActiveStations"};
  settings_->setValue ("window/geometry", saveGeometry ());
  settings_->setValue("MaxRecent",ui->sbMaxRecent->value());
  settings_->setValue("MaxAge",ui->sbMaxAge->value());
  settings_->setValue("ReadyOnly",ui->cbReadyOnly->isChecked());
}

void ActiveStations::displayRecentStations(QString mode, QString const& t)
{
  m_mode=mode;
  bool b=(m_mode=="Q65");
  if(b) {
    ui->header_label2->setText("  N  Freq    Call      Tx  Age");
    ui->label->setText("QSOs:");
  } else {
    ui->header_label2->setText("  N   Call    Grid   Az  S/N  Freq Tx Age Pts");
    ui->label->setText("Rate:");
  }
  ui->bandChanges->setVisible(!b);
  ui->cbReadyOnly->setVisible(!b);
  ui->label_2->setVisible(!b);
  ui->label_3->setVisible(!b);
  ui->score->setVisible(!b);
  ui->sbMaxRecent->setVisible(!b);
  ui->RecentStationsPlainTextEdit->setPlainText(t);

  QString t1= " 1.  R7BI    KN96   41  -12   764  0  0   18\n 2.  LA6OP   JP67   29  +07   696  0  0*  13\n 3.  G0OUC   IO93   49  -20  1628  0  0   13\n 4.  G5EA    IO93   49  -13  1747  0  0   13\n 5.  G7BHU   IO93   49  -17  1191  0  0   13\n 6.  ON4EB   JO11   50  -01  2188  0  0   13\n 7.  K2AK    DM41  264  +03  1432  0  0    8\n 8.  N2DEE   DM79  277  -01  1297  0  0    7\n 9.  AK0MR   DM59  279  +07  2478  0  0    7\n10.  NK5G    EM20  245  -07  2149  0  0    6\n"; //TEMP
  ui->RecentStationsPlainTextEdit->setPlainText(t1);


}

int ActiveStations::maxRecent()
{
  return ui->sbMaxRecent->value();
}

int ActiveStations::maxAge()
{
  return ui->sbMaxAge->value();
}

void ActiveStations::select()
{
  if(m_clickOK) {
    qint64 msec=QDateTime::currentMSecsSinceEpoch();
    if((msec-m_msec0)<500) return;
    m_msec0=msec;
    int nline=ui->RecentStationsPlainTextEdit->textCursor().blockNumber();

    qDebug() << "aa" << nline << ui->RecentStationsPlainTextEdit->textCursor().position();
    qDebug() << "bb" << ui->RecentStationsPlainTextEdit->toPlainText();

//    if(nline!=-99) return;   //TEMPORARY
    emit callSandP(nline);
  }
}

void ActiveStations::setClickOK(bool b)
{
  m_clickOK=b;
}

void ActiveStations::erase()
{
  ui->RecentStationsPlainTextEdit->clear();
}

bool ActiveStations::readyOnly()
{
  return ui->cbReadyOnly->isChecked();
}

void ActiveStations::on_cbReadyOnly_toggled(bool b)
{
  m_bReadyOnly=b;
  emit activeStationsDisplay();
}

void ActiveStations::setRate(int n)
{
  ui->rate->setText(QString::number(n));
}

void ActiveStations::setScore(int n)
{
  ui->score->setText(QLocale(QLocale::English).toString(n));
}

void ActiveStations::setBandChanges(int n)
{
  if(n >= 8) {
    ui->bandChanges->setStyleSheet("QLineEdit{background: rgb(255, 64, 64)}");
  } else {
    ui->bandChanges->setStyleSheet ("");
  }
  ui->bandChanges->setText(QString::number(n));
}
