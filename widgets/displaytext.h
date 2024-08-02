// -*- Mode: C++ -*-
#ifndef DISPLAYTEXT_H
#define DISPLAYTEXT_H

#include <QTextEdit>
#include <QFont>
#include <QHash>
#include <QPair>
#include <QString>

class QAction;
class Configuration;
class LogBook;
class DecodedText;

class DisplayText
  : public QTextEdit
{
  Q_OBJECT
public:
  explicit DisplayText(QWidget *parent = nullptr);
  void set_configuration (Configuration const * configuration, bool high_volume = false)
  {
    disconnect (vertical_scroll_connection_);
    m_config = configuration;
    high_volume_ = high_volume;
  }
  void setContentFont (QFont const&);
  void insertLineSpacer(QString const&);
  void displayDecodedText(DecodedText const& decodedText, QString const& myCall, QString const& mode,
                          bool displayDXCCEntity, LogBook const& logBook,
                          QString const& currentBand=QString {}, bool ppfx=false, bool bCQonly=false,
                          bool haveFSpread = false, float fSpread = 0.0, bool bDisplayPoints=false,
                          int points=-99);
  void displayTransmittedText(QString text, QString modeTx, qint32 txFreq, bool bFastMode,
                              double TRperiod, bool bSuperfox);
  void displayQSY(QString text);
  void displayHoundToBeCalled(QString t, bool bAtTop=false, QColor bg = QColor {}, QColor fg = QColor {});
  void setHighlightedHoundText(QString text);
  void new_period ();
  QString CQPriority(){return m_CQPriority;};
  qint32 m_points;
  bool m_bDisplayPoints;

  Q_SIGNAL void selectCallsign (Qt::KeyboardModifiers);
  Q_SIGNAL void erased ();

  Q_SLOT void insertText (QString const& text, QColor bg = QColor {}, QColor fg = QColor {}
                          , QString const& call1 = QString {}, QString const& call2 = QString {}, QTextCursor::MoveOperation location=QTextCursor::End);
  Q_SLOT void erase ();
  Q_SLOT void highlight_callsign (QString const& callsign, QColor const& bg, QColor const& fg, bool last_period_only);

private:
  QString leftJustifyAppendage (QString message, QString const& appendage) const;
  void mouseDoubleClickEvent (QMouseEvent *) override;

  void extend_vertical_scrollbar (int min, int max);

  Configuration const * m_config;
  bool m_bPrincipalPrefix;
  QString m_CQPriority;
  QString appendWorkedB4(QString message, QString callsign
                         , QString const& grid, QColor * bg, QColor * fg
                         , LogBook const& logBook, QString const& currentBand
                         , QString const& currentMode, QString extra);
  QFont char_font_;
  QAction * erase_action_;

  QHash<QString, QPair<QColor, QColor>> highlighted_calls_;
  bool high_volume_;
  QMetaObject::Connection vertical_scroll_connection_;
  long long modified_vertical_scrollbar_max_;
};

#endif // DISPLAYTEXT_H
